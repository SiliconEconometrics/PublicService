/*
    Copyright 2015-2016 Silicon Econometrics Pty. Ltd.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

 */



package org.greatcactus.vote.count

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object RunElection extends App {

  val useIvote = true
  val useNormal = false
  
  val rawstats = NSWStateElectionData.load(useIvote,useNormal)
  rawstats.printStatus()
  val random = new Random
  val worker = new NSWElectionHelper(rawstats,21,random,NSWLegislativeCouncilRules,Set.empty)
  worker.run()
  ElectionReport.saveReports(new java.io.File(NSWStateElectionData.reportDir,"Single Run"+(if (useIvote) " iVote" else "")+(if (useNormal) " Normal" else "")),worker.report)
  //println("\n\nIVOTE\n")
  //val ivote = LoadFromFile.load(true)
  //ivote.printStatus()
}

/** A partially distributed vote. Ignore votes before upto. Really the most important data structure in the program. */
class DVote(val upto:Int,val numVoters:Double,val prefs:Array[Int]) {
  def current : Int = prefs(upto)
  def next : DVote = new DVote(upto+1,numVoters,prefs)
  def isExhausted : Boolean = upto==prefs.length
  def skipNotContinuingCandidates(continuingCandidates:Set[Int]) : DVote = {
    if (isExhausted || continuingCandidates(current)) this
    else next.skipNotContinuingCandidates(continuingCandidates)
  }
  def applyTransferValue(transferValue:Double) : DVote = new DVote(upto,numVoters*transferValue,prefs)
}

class CandidateTally(val candidateID:Int
    ) {
  val allVotes = new ArrayBuffer[DVote]
  val redistributableVotes = new ArrayBuffer[DVote]
  val mayWantBackAsRedistributableVotes = new ArrayBuffer[DVote]
  var numVotes = 0.0
  def add(v:DVote) {
    redistributableVotes+=v
    allVotes+=v
    numVotes+=v.numVoters
  }
  var elected=false
  var surplusVotes =0.0
  def clearRedistributableVotes(mayWantBack:Boolean) { 
    mayWantBackAsRedistributableVotes.clear()
    if (mayWantBack) mayWantBackAsRedistributableVotes++=redistributableVotes
    redistributableVotes.clear() 
  }
  def retrieveNotQuiteGoneVotes() {
    redistributableVotes++=mayWantBackAsRedistributableVotes
  }
}

class ElectionRules(
    /** Use NSW Local Government Election rules for countback and a few minor things */
  val useNSWLocalGovernmentElectionRules:Boolean,
    /** Use the (IMHO buggy) redistributable rules used by NSW EC 2012 LGE and later rules */
  val useNWSECredistributableRules:Boolean,
    /** Use the NSW probabilistic sampling rather than weighted votes */
  val useNSWProbabilisticSamplingOfElections:Boolean
)

object NSWLegislativeCouncilRules extends ElectionRules(false,false,true)
object NSWLocalGovernmentElectionRules extends ElectionRules(true,false,true)
object NSWLocalGovernmentBuggyElectionRules extends ElectionRules(true,true,true)
    
/**
 * Does most of the work of the election counting. Numbers refer to the sections in
 * the specification document from the specification.
 * useNWSECredistributableRules: if true, use the rules in 1.4.14.2 instead of 1.4.14.1
 */
class NSWElectionHelper(data:ElectionData,candidatesToBeElected:Int,random:Random,electionRules:ElectionRules,val ineligibleCandidates:Set[Int]) {
  val numCandidates = data.candidates.length
  val tallys = Array.tabulate(numCandidates){new CandidateTally(_)}
  val report = new ElectionResultReport(data.candidates,tallys,ineligibleCandidates)
  def numElectedCandidates = report.electedCandidates.length
  def emptySeats = candidatesToBeElected-numElectedCandidates
    
  var continuingCandidates : Set[Int] = (0 until numCandidates).toSet--ineligibleCandidates

  val candidatesWithUndistributedVotes = new ArrayBuffer[CandidateTally]
  def undistributedVotes = candidatesWithUndistributedVotes.map{_.surplusVotes}.sum
  def clearRedistributableVotes(isBecauseOfExclusion:Boolean) {
      for (c<-continuingCandidates) tallys(c).clearRedistributableVotes(isBecauseOfExclusion && electionRules.useNWSECredistributableRules)
  }
  
  {
    val votes : Array[DVote] = for (v<-data.makeVotes) yield new DVote(0,v.numVoters,v.preferences).skipNotContinuingCandidates(continuingCandidates)
    // 1. assign first preference votes
    for (v<-votes) {
      if (v.isExhausted) report.addExhaustedVotes(v.numVoters)
      else tallys(v.current).add(v)
    }
    // 2. calculate  Candidate first preference total (implicit)
  }
  // 3. Determine quota
  val formalVotes = tallys.map{_.numVotes}.sum
  val quota = (Math.floor(formalVotes/(1+candidatesToBeElected))).toInt+1
  report.setQuota(quota)
  report.note(s"Formal votes : $formalVotes  Candidates to be elected : $candidatesToBeElected   Quota : $quota")
  
  report.initialCountDone()
  // 
  def run() { while (true) {
   
    // 4. Has any candidate reached the quota?
    val candidatesWithQuota = continuingCandidates.filter { i => tallys(i).numVotes>=quota }
    if (!candidatesWithQuota.isEmpty) {
      // 5. Identify Candidates Elected
      continuingCandidates--=candidatesWithQuota
      clearRedistributableVotes(false)
      if (electionRules.useNWSECredistributableRules) for (i<-candidatesWithQuota) tallys(i).retrieveNotQuiteGoneVotes()
      for (i<-candidatesWithQuota) tallys(i).elected=true
      // 6. Calculate elected candidate surplus
      for (i<-candidatesWithQuota) tallys(i).surplusVotes=tallys(i).numVotes-quota
      // 7. Identify position elected
      val orderedCandidates = {
        val grouped = candidatesWithQuota.toList.map{tallys(_)}.groupBy { _.surplusVotes }.toList.sortBy{_._1}.reverse
        // 8. Draw for position required?
        // 9. Process draw for position
        grouped.flatMap{case (_,l)=>
          val lookback : Option[List[Int]] = if (electionRules.useNSWLocalGovernmentElectionRules && l.size>1) report.searchRecentHistoryForCandidatesWithHigherCountsAndOrder(l.map{_.candidateID}) else None
          val lookback2 : Option[List[CandidateTally]] = for (ordering<-lookback) yield { 
            val candidateIDToTally = Map.empty ++ (for (ct<-l) yield ct.candidateID->ct)
            ordering.map{candidateIDToTally.get(_).get} 
          }
          lookback2.getOrElse{RandomUtil.randomPermutation(l,random)}
        }
      }
      for (c<-orderedCandidates) {
        report.declareElected(c.candidateID,"Has Quota")
        candidatesWithUndistributedVotes+=c
      }
      // 10 More candidates to be elected?
      if (emptySeats==0) { report.finishCount() ; return }
    }
    // 11 Elect candidates without reaching quota?
    def computeElectedCandidatesWithoutQuota() : List[Int] = {
      if (emptySeats==1) {
         val totalRemainingVotes = continuingCandidates.toList.map{tallys(_).numVotes}.sum+undistributedVotes
 //        println("totalRemainingVotes = "+totalRemainingVotes+" undistributedVotes="+undistributedVotes+" continuingCandidates="+continuingCandidates+" tallys="+continuingCandidates.toList.map{tallys(_).numVotes})
         for (i<-continuingCandidates) if (tallys(i).numVotes*2>totalRemainingVotes) return List(i)
      }
      if (electionRules.useNSWLocalGovernmentElectionRules && emptySeats>0) {
        val ordered = continuingCandidates.toList.sortBy { i => -tallys(i).numVotes }
        val minorities = ordered.drop(emptySeats).map { i => tallys(i).numVotes }.sum + undistributedVotes
        if (tallys(ordered(emptySeats-1)).numVotes>minorities) return ordered.take(emptySeats)
      }
      if (emptySeats==continuingCandidates.size) return continuingCandidates.toList
      Nil
    }
    val electedWithoutQuota = computeElectedCandidatesWithoutQuota()
    def orderRemainingCandidates(unordered:List[Int],reason:String) {
      // 30 Order remaining candidates
      val ordered = unordered.sortWith{case (i,j)=>val d=tallys(i).numVotes-tallys(j).numVotes; d>0 || (d==0 && i<j)}
      for (c<-ordered) report.declareElected(c, reason)      
    }
    if (!electedWithoutQuota.isEmpty) {
      orderRemainingCandidates(electedWithoutQuota,"Elected without quota")
      report.finishCount()
      return;
    }
    
    def shouldDistributeSomeUndistributedVotes : Boolean = (!candidatesWithUndistributedVotes.isEmpty) && {
      // 12 Total undistributed surplus from elected candidates above deferral cut-off
      val margin= {
         val simpletallys = continuingCandidates.toList.map{tallys(_).numVotes}.sorted
         simpletallys(1)-simpletallys(0)
      }
      if (electionRules.useNSWLocalGovernmentElectionRules) undistributedVotes>margin else undistributedVotes>=margin 
    }
    def distributeOneCandidatesWorthOfUndistributedVotes() {
      // distribute votes
      // 13 identify candidate whose votes are to be distributed
     // count+=1
      val candidateToDistribute = candidatesWithUndistributedVotes.remove(0)
      // 14 identify votes to be distributed
      // done implicitly as unredistributable votes are cleared elsewhere
      val votesToBeRedistributed = candidateToDistribute.redistributableVotes.toArray
      val numVotesToBeRedistributed = votesToBeRedistributed.map{_.numVoters}.sum
      // 15 distribute preferences
      val notExhausted = votesToBeRedistributed.map{_.skipNotContinuingCandidates(continuingCandidates)}.filter {! _.isExhausted}
      val numDistributed = notExhausted.map{_.numVoters}.sum
      val numRedistributedExhausted = numVotesToBeRedistributed-numDistributed
      val numExhaustedThatWouldBeCarriedOn = (candidateToDistribute.surplusVotes-numDistributed) max 0
      val numExhaustedSetAside = numRedistributedExhausted-numExhaustedThatWouldBeCarriedOn
      val distribution : Map[Int,Array[DVote]] = notExhausted.groupBy { _.current}
      // 16 Calculate transfer value
      val transferValue = (candidateToDistribute.surplusVotes.toDouble/(numDistributed max 1)) min 1.0
      // 17 Calculate number of ballot papers to be transferred and set aside
      val recipients = for ((id,dvotes)<-distribution) yield new VotesToBeTransferred(id,dvotes,transferValue)
      val sortedRecipients = recipients.toList.sortWith{case (a,b) => a.fractonalVotesToBeTransferred>b.fractonalVotesToBeTransferred || (a.fractonalVotesToBeTransferred==b.fractonalVotesToBeTransferred && a.intVotesToBeTransferred>b.intVotesToBeTransferred)}
      if (electionRules.useNSWProbabilisticSamplingOfElections) {
        val numRoundUps = candidateToDistribute.surplusVotes.toInt-recipients.map{_.intVotesToBeTransferred}.sum
        val recipientsToRoundUp = if (numRoundUps>0 && numRoundUps<sortedRecipients.size && sortedRecipients(numRoundUps-1).originalNumVotes==sortedRecipients(numRoundUps).originalNumVotes) {
          // there is an exact tie to be broken.
          val tiedVotes = sortedRecipients(numRoundUps-1).originalNumVotes
          val definitelyRoundUp = sortedRecipients.takeWhile { _.originalNumVotes!=tiedVotes }
          val toBeResolved = sortedRecipients.drop(definitelyRoundUp.size).takeWhile { _.originalNumVotes==tiedVotes } // tied!
          val wantedFromResolved = numRoundUps-definitelyRoundUp.size
          assert (wantedFromResolved>0 && wantedFromResolved<toBeResolved.size)
          val toBeResolvedIDs=toBeResolved.map{_.candidateID}
          // Steps 18 and 19 - rounding randomization
          val resolvedCandidates = report.searchRecentHistoryForCandidatesWithHigherCounts(wantedFromResolved,toBeResolvedIDs).getOrElse{RandomUtil.randomPermutation(toBeResolvedIDs, random).take(wantedFromResolved).toSet}
          val tieBrokenRecipients = toBeResolved.filter{r=>resolvedCandidates(r.candidateID)}
          val finalList = definitelyRoundUp++tieBrokenRecipients
          assert (finalList.size==numRoundUps)
          finalList
        } else sortedRecipients.take(numRoundUps)
        for (r<-recipientsToRoundUp) r.roundUp()
        // Step 20 Set aside ballot papers (random selection)
        for (r<-sortedRecipients) r.setAsideRandomly(random)
      } else {
        for (r<-sortedRecipients) r.setAsideUsingWeights()
      }
      val totalTransferred = sortedRecipients.map{_.votesToActuallyDistribute.map{_.numVoters}.sum}.sum
      val numExhausted = candidateToDistribute.surplusVotes-totalTransferred
      // println("transferValue="+transferValue+" surplus="+candidateToDistribute.surplusVotes+" transferred="+totalTransferred+" distributed="+numDistributed+" numExhausted="+numExhausted+" numExhaustedThatWouldBeCarriedOn="+numExhaustedThatWouldBeCarriedOn)
      if (Math.abs(numExhausted-numExhaustedThatWouldBeCarriedOn)>1e-6) throw new Exception
      // Step 21 
      report.declareCandidateDistributed(candidateToDistribute.candidateID,candidateToDistribute.surplusVotes,totalTransferred,transferValue,sortedRecipients,numVotesToBeRedistributed,numExhaustedSetAside,numExhaustedThatWouldBeCarriedOn)
      report.addExhaustedVotes(numExhaustedThatWouldBeCarriedOn)
      candidateToDistribute.numVotes = quota
      for (r<-sortedRecipients) {
        val c = tallys(r.candidateID)
        for (v<-r.votesToActuallyDistribute) c.add(v)
      }
      report.finishCount()
    }
    
    if (shouldDistributeSomeUndistributedVotes) {
      distributeOneCandidatesWorthOfUndistributedVotes()
      // Step 22/23 - elected candidate surpluses still to be distributed?
      while (shouldDistributeSomeUndistributedVotes) distributeOneCandidatesWorthOfUndistributedVotes()      
    } else {
      // choose someone to be eliminated
      clearRedistributableVotes(true)
      // 24 identify candidates for exclusion
      val groupedByVotes = continuingCandidates.toArray.map{tallys}.groupBy { _.numVotes }.toList.sortBy{_._1}
      //println(groupedByVotes)
      val candidatesForExclusion = groupedByVotes.head._2 // rather inefficient
      val margin = if (candidatesForExclusion.length>1) 0 else groupedByVotes(1)._1-groupedByVotes(0)._1
      // 25 draw for exclusion?
      val candidateExcluded = if (candidatesForExclusion.length==1) candidatesForExclusion(0) 
                              else {
                                 //println("Candidates tied")
                                 val toTakeOut = if (electionRules.useNSWLocalGovernmentElectionRules) report.searchRecentHistoryForCandidatesWithHigherCounts(candidatesForExclusion.length-1,candidatesForExclusion.map{_.candidateID}.toList) else None
                                 if (toTakeOut.isDefined) {
                                   //println("Of "+candidatesForExclusion.map{c => data.candidates(c.candidateID).name }.mkString(" ")+" preferred are "+toTakeOut.get.map{data.candidates(_).name}.mkString(" "))
                                   candidatesForExclusion.find{c=> !toTakeOut.get.contains(c.candidateID)}.get
                                 }
                                 // 26 process draw for exclusion
                                 else candidatesForExclusion(random.nextInt(candidatesForExclusion.length))
                              }
      continuingCandidates-=candidateExcluded.candidateID
      report.declareCandidateExcluded(candidateExcluded.candidateID,margin)
      // 27 Continuing candidates equals unfilled vacancies?
      if (continuingCandidates.size==emptySeats) {
        // 30 Order remaining candidates
        orderRemainingCandidates(continuingCandidates.toList,"Only remaining")
        report.finishCount()
        // 31 End election
        return
      }
      // 28 Transfer Ballot papers
      var numExhaustedInDistribution = 0.0
      val numRedistributedByCandidate = new Array[Double](numCandidates)
      for (v<- candidateExcluded.allVotes) {
        val vv = v.skipNotContinuingCandidates(continuingCandidates)
        if (vv.isExhausted) numExhaustedInDistribution+=v.numVoters
        else {
          tallys(vv.current).add(vv)
          numRedistributedByCandidate(vv.current)+=vv.numVoters
        }
      }
      report.addExhaustedVotes(numExhaustedInDistribution)
      candidateExcluded.numVotes=0
      // 29 Calculate Progressive Totals
      report.finishCount()
      // deal with NSWEC redistributable votes interpretation
      if (electionRules.useNSWLocalGovernmentElectionRules) {
         val candidatesWithQuota = continuingCandidates.filter { i => tallys(i).numVotes>=quota }
         for (c<-continuingCandidates--candidatesWithQuota) tallys(c).clearRedistributableVotes(false)
      }
    }
  }}
}


class VotesToBeTransferred(val candidateID:Int,val votes:Array[DVote],transferValue:Double) {
  val originalNumVotes=votes.map{_.numVoters}.sum
  private val rawTransfer = transferValue*originalNumVotes
  var intVotesToBeTransferred = rawTransfer.toInt
  val fractonalVotesToBeTransferred = rawTransfer-intVotesToBeTransferred
  def roundUp() { if (fractonalVotesToBeTransferred>0) intVotesToBeTransferred+=1 }
  var votesToActuallyDistribute : Array[DVote] = null
  def setAsideRandomly(random:Random) {
    votesToActuallyDistribute = if (intVotesToBeTransferred==originalNumVotes) votes else {
      val chosen = RandomUtil.choose(intVotesToBeTransferred,originalNumVotes.toInt,random)
      //val chosen = RandomUtil.choose(originalNumVotes.toInt-intVotesToBeTransferred,originalNumVotes.toInt,random,votes.flatMap{v=>List.fill(v.numVoters.toInt)(50/v.prefs.length)}).map{! _} // simulate a bug where the prob of choosing a vote depends in the length of the vote.      
      val res = new ArrayBuffer[DVote]
      var upto = 0
      for (v<-votes) {
        var passedOn = 0
        for (i<-0 until v.numVoters.toInt) { 
          if (chosen(upto)) passedOn+=1
          upto+=1
        }
        if (passedOn>0) res+= (if (passedOn==v.numVoters) v else new DVote(v.upto,passedOn,v.prefs))
      }
      res.toArray
      //Array.fill(intVotesToBeTransferred)(votes(random.nextInt(votes.length))) // Possible bug simulation
    }
  }
  def setAsideUsingWeights() {
    votesToActuallyDistribute = for (dv<-votes) yield dv.applyTransferValue(transferValue) 
  }
}

object RandomUtil {
  /** produce a boolean array of length outOf, with wanted elements true. */
  def choose(wanted:Int,outOf:Int,random:Random) : Array[Boolean] = {
    val doInverse = wanted > outOf/2
    val res = Array.fill(outOf)(doInverse)
    var togo = if (doInverse) outOf-wanted else wanted
    while (togo>0) {
      val pos = random.nextInt(outOf)
      //random.nextInt
      if (res(pos)==doInverse) { res(pos) = !doInverse; togo-=1}
    }
    res
  }

  /** produce a boolean array of length outOf, with wanted elements true, with a weighted probability of choosing a given vote. Used for testing biases. */
  def choose(wanted:Int,outOf:Int,random:Random,weights:Array[Int]) : Array[Boolean] = {
    val cumulativeWeights = new Array[Int](outOf)
    var sumWeights = 0
    for (i<-0 until outOf) {
      sumWeights+=weights(i)
      cumulativeWeights(i)=sumWeights
    }
    val inverseCumulativeWeights = new Array[Int](sumWeights) 

    for (i<-0 until outOf) {
      val start = if (i==0) 0 else cumulativeWeights(i-1)
      for (j<-start until cumulativeWeights(i)) inverseCumulativeWeights(j)=i
    }

    
    val res = Array.fill(outOf)(false)
    var togo = wanted
    while (togo>0) {
      val pos = inverseCumulativeWeights(random.nextInt(sumWeights))
      if (res(pos)==false) { res(pos) = true; togo-=1}
    }
    res
  }
  
  
  def randomPermutation[T](l:List[T],random:Random) : List[T] = {
    if (l.length<=1) l else {
      val ind = random.nextInt(l.length)
      l(ind)::randomPermutation(l.take(ind)++l.drop(ind+1), random)
    }
  }

}