/*
    Copyright 2015 Silicon Econometrics Pty. Ltd. 

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

  val rawstats = LoadFromFile.load(false)
  rawstats.printStatus()
  val random = new Random
  val worker = new NSWElectionHelper(rawstats,21,random)
  worker.run()
  ElectionReport.saveReports(worker.report,rawstats.candidates,worker.quota)
  //println("\n\nIVOTE\n")
  //val ivote = LoadFromFile.load(true)
  //ivote.printStatus()
}

/** A partially distributed vote. Ignore votes before upto. Really the most important data structure in the program. */
class DVote(val upto:Int,val numVoters:Int,val prefs:Array[Int]) {
  def current : Int = prefs(upto)
  def next : DVote = new DVote(upto+1,numVoters,prefs)
  def isExhausted : Boolean = upto==prefs.length
  def skipNotContinuingCandidates(continuingCandidates:Set[Int]) : DVote = {
    if (isExhausted || continuingCandidates(current)) this
    else next.skipNotContinuingCandidates(continuingCandidates)
  }
}

class CandidateTally(val candidateID:Int
    ) {
  val allVotes = new ArrayBuffer[DVote]
  val redistributableVotes = new ArrayBuffer[DVote]
  var numVotes = 0
  def add(v:DVote) {
    redistributableVotes+=v
    allVotes+=v
    numVotes+=v.numVoters
  }
  var elected=false
  var surplusVotes =0
  def clearRedistributableVotes() { redistributableVotes.clear() }
}
    
/**
 * Does most of the work of the election counting. Numbers refer to the sections in
 * the specification document from the specification.
 */
class NSWElectionHelper(data:ElectionData,candidatesToBeElected:Int,random:Random) {
  val numCandidates = data.candidates.length
  val tallys = Array.tabulate(numCandidates){new CandidateTally(_)}
  val report = new ElectionResultReport(data.candidates,tallys)
  def numElectedCandidates = report.electedCandidates.length
  def emptySeats = candidatesToBeElected-numElectedCandidates
    
  var continuingCandidates : Set[Int] = (0 until numCandidates).toSet

  val candidatesWithUndistributedVotes = new ArrayBuffer[CandidateTally]
  def undistributedVotes : Int = candidatesWithUndistributedVotes.map{_.surplusVotes}.sum
  def clearRedistributableVotes() {
      for (c<-continuingCandidates) tallys(c).clearRedistributableVotes()
  }
  
  {
    val votes : Array[DVote] = for (v<-data.makeVotes) yield new DVote(0,v.numVoters,v.preferences)
    // 1. assign first preference votes
    for (v<-votes) tallys(v.current).add(v)
    // 2. calculate  Candidate first preference total (implicit)
  }
  // 3. Determine quota
  val formalVotes = tallys.map{_.numVotes}.sum
  val quota = formalVotes/(1+candidatesToBeElected)+1
  report.note(s"Formal votes : $formalVotes  Candidates to be elected : $candidatesToBeElected   Quota : $quota")
  
  report.initialCountDone()
  // 
  def run() { while (true) {
   
    

    // 4. Has any candidate reached the quota?
    val candidatesWithQuota = continuingCandidates.filter { i => tallys(i).numVotes>=quota }
    if (!candidatesWithQuota.isEmpty) {
      // 5. Identify Candidates Elected
      continuingCandidates--=candidatesWithQuota
      clearRedistributableVotes()
      for (i<-candidatesWithQuota) tallys(i).elected=true
      // 6. Calculate elected candidate surplus
      for (i<-candidatesWithQuota) tallys(i).surplusVotes=tallys(i).numVotes-quota
      // 7. Identify position elected
      val orderedCandidates = {
        val grouped = candidatesWithQuota.toList.map{tallys(_)}.groupBy { _.surplusVotes }.toList.sortBy{_._1}.reverse
        // 8. Draw for position required?
        // 9. Process draw for position
        grouped.flatMap{case (_,l)=>RandomUtil.randomPermutation(l,random)}
      }
      for (c<-orderedCandidates) {
        report.declareElected(c.candidateID,"Has Quota")
        candidatesWithUndistributedVotes+=c
      }
      // 10 More candidates to be elected?
      if (emptySeats==0) return
    }
    // 11 Elect candidates without reaching quota?
    def computeElectedCandidatesWithoutQuota() : List[Int] = {
      if (emptySeats==1) {
         val totalRemainingVotes = continuingCandidates.map{tallys(_).numVotes}.sum+undistributedVotes
         for (i<-continuingCandidates) if (tallys(i).numVotes*2>totalRemainingVotes) return List(i)
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
      return;
    }
    
    def shouldDistributeSomeUndistributedVotes : Boolean = (!candidatesWithUndistributedVotes.isEmpty) && {
      // 12 Total undistributed surplus from elected candidates above deferral cut-off
      val margin= {
         val simpletallys = continuingCandidates.toList.map{tallys(_).numVotes}.sorted
         simpletallys(1)-simpletallys(0)
      }
      undistributedVotes>=margin
    }
    def distributeOneCandidatesWorthOfUndistributedVotes() {
      // distribute votes
      // 13 identify candidate whose votes are to be distributed
     // count+=1
      val candidateToDistribute = candidatesWithUndistributedVotes.remove(0)
      // 14 identify votes to be distributed
      // done implicitly as unredistributable votes are cleared elsewhere
      val votesToBeRedistributed = candidateToDistribute.redistributableVotes.toArray
      // 15 distribute preferences
      val notExhausted = votesToBeRedistributed.map{_.skipNotContinuingCandidates(continuingCandidates)}.filter {! _.isExhausted}
      val numDistributed = notExhausted.map{_.numVoters}.sum
      val distribution : Map[Int,Array[DVote]] = notExhausted.groupBy { _.current}
      // 16 Calculate transfer value
      val transferValue = (candidateToDistribute.surplusVotes.toDouble/(numDistributed max 1)) min 1.0
      // 17 Calculate number of ballot papers to be transferred and set aside
      val recipients = for ((id,dvotes)<-distribution) yield new VotesToBeTransferred(id,dvotes,transferValue)
      val sortedRecipients = recipients.toList.sortWith{case (a,b) => a.fractonalVotesToBeTransferred>b.fractonalVotesToBeTransferred || (a.fractonalVotesToBeTransferred==b.fractonalVotesToBeTransferred && a.intVotesToBeTransferred<b.intVotesToBeTransferred)}
      val numRoundUps = candidateToDistribute.surplusVotes-recipients.map{_.intVotesToBeTransferred}.sum
      for (r<-sortedRecipients.take(numRoundUps)) r.roundUp()
      val totalTransferred = recipients.map{_.intVotesToBeTransferred}.sum
      val numExhausted = candidateToDistribute.surplusVotes-totalTransferred
      //println("transferValue="+transferValue+" surplus="+candidateToDistribute.surplusVotes+" transferred="+totalTransferred+" distributed="+numDistributed)
      if (numExhausted<0) throw new Exception
      report.addExhaustedVotes(numExhausted)
      // TODO Note I have skipped step 18 and 19 - rounding randomization.
      // Step 20 Set aside ballot papers (random selection)
      for (r<-sortedRecipients) r.setAsideRandomly(random)
      // Step 21 
      report.declareCandidateDistributed(candidateToDistribute.candidateID,candidateToDistribute.surplusVotes,totalTransferred,transferValue,sortedRecipients)
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
      clearRedistributableVotes()
      // 24 identify candidates for exclusion
      val groupedByVotes = continuingCandidates.toArray.map{tallys}.groupBy { _.numVotes }.toList.sortBy{_._1}
      val candidatesForExclusion = groupedByVotes.head._2 // rather inefficient
      val margin = if (candidatesForExclusion.length>1) 0 else groupedByVotes(1)._1-groupedByVotes(0)._1
      // 25 draw for exclusion?
      val candidateExcluded = if (candidatesForExclusion.length==1) candidatesForExclusion(0) 
      // 26 process draw for exclusion
                              else candidatesForExclusion(random.nextInt(candidatesForExclusion.length))
      continuingCandidates-=candidateExcluded.candidateID
      report.declareCandidateExcluded(candidateExcluded.candidateID,margin)
      // 27 Continuing candidates equals unfilled vacancies?
      if (continuingCandidates.size==emptySeats) {
        // 30 Order remaining candidates
        orderRemainingCandidates(continuingCandidates.toList,"Only remaining")
        // 31 End election
        return
      }
      // 28 Transfer Ballot papers
      var numExhaustedInDistribution = 0
      val numRedistributedByCandidate = new Array[Int](numCandidates)
      for (v<- candidateExcluded.allVotes) {
        val vv = v.skipNotContinuingCandidates(continuingCandidates)
        if (vv.isExhausted) numExhaustedInDistribution+=1
        else {
          tallys(vv.current).add(vv)
          numRedistributedByCandidate(vv.current)+=vv.numVoters
        }
      }
      // 29 Calculate Progressive Totals
      report.finishCount()
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
      val chosen = RandomUtil.choose(intVotesToBeTransferred,originalNumVotes,random)
      val res = new ArrayBuffer[DVote]
      var upto = 0
      for (v<-votes) {
        var passedOn = 0
        for (i<-0 until v.numVoters) { 
          if (chosen(upto)) passedOn+=1
          upto+=1
        }
        if (passedOn>0) res+= (if (passedOn==v.numVoters) v else new DVote(v.upto,passedOn,v.prefs))
      }
      res.toArray
    }
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
      if (res(pos)==doInverse) { res(pos) = !doInverse; togo-=1}
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