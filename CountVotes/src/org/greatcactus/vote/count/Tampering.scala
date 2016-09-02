/*
    Copyright 2016 Silicon Econometrics Pty. Ltd.

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

import FederalSenateCount._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class WhatMarginInformationToCompute(
    /** Whether we should try to compute any information at all */ val wantAnyMarginInfo:Boolean,
    /** Whether we should try to compute exclusion information for specific votes and rerun */ val shouldIncludeSpecificVotes:Boolean,
    /** Whether we should try to compute exclusion information for specific candidates */ val candidatesToTryToExclude : Option[Set[CandidateIndex]],
    /** Whether we should try the time intensive binary-search-on-rerun optimization */ val shouldOptimizeBinarySearchRerunElection : Boolean,
    /** Whether we should try for indirect effects */ val tryIndirectExclusion : Boolean,
    /** Whether we should try to reduce low transfer values from the person being excluded */ val optimizeLowTransferValuesFromNewLoser : Boolean,
    /** Whether we should allow use of ATL votes */ val mayUseATLs : Boolean
    )
    
object WhatMarginInformationToCompute {
  val none = new WhatMarginInformationToCompute(false,false,None,false,false,false,false)
  val simpleRerun = new WhatMarginInformationToCompute(true,true,None,false,false,false,true)
}

class TamperedVote(val whoFrom:Int,val whoTo:Int,val numPapers:Int,val numVotes:Int,val src:Option[ActualListOfTamperableVotes],val isATL:Boolean) {
  if (numPapers<0) throw new IllegalArgumentException
  def desc(candidates:Array[Candidate]) : String = candidates(whoFrom).name+"\u2192"+candidates(whoTo).name+":"+numPapers+(if (numPapers!=numVotes) "("+numVotes+" votes)" else "") // \u2192 is right arrow
  override def toString = whoFrom+"\u2192"+whoTo+":"+votedesc // \u2192 is right arrow
  def votedesc : String = numPapers.toString+(if (numPapers!=numVotes) "("+numVotes+" votes)" else "")
}
class Margin(val step:Int,val tamperings:Array[TamperedVote]) {
  val votes = tamperings.map { _.numVotes}.sum
  val papers = tamperings.map {_.numPapers}.sum

  /** Pretty printing string */
  def how(candidates:Array[Candidate]) : String = tamperings.map{_.desc(candidates)}.mkString(" , ")
  /** Pretty printing, line of strings, each line a set of tamperings all from/to same person */
  def howLines(candidates:Array[Candidate]) : List[String] = {
    class HowHelper(val whoFrom:Int,val whoTo:Int) {
      var atl = new ListBuffer[TamperedVote]
      var btl = new ListBuffer[TamperedVote]
      def add(tv:TamperedVote) { if (tv.isATL) atl+=tv else btl+=tv }
      override def toString : String = {
        val who = candidates(whoFrom).name+"\u2192"+candidates(whoTo).name+":"
        val atls = if (atl.isEmpty) "" else " "+atl.map{_.votedesc}.mkString(",")+" ATL"
        val btls = if (btl.isEmpty) "" else " "+btl.map{_.votedesc}.mkString(",")+" BTL"
        who+atls+btls
      }
    }
    val linesMap = new collection.mutable.HashMap[(CandidateIndex,CandidateIndex),HowHelper]
    //val lines = new ArrayBuffer[HowHelper]
    for (t<-tamperings) {
      linesMap.getOrElseUpdate((t.whoFrom,t.whoTo),new HowHelper(t.whoFrom,t.whoTo)).add(t)
      //if (lines.isEmpty || lines.last.whoFrom!=t.whoFrom || lines.last.whoTo!=t.whoTo) lines+=new HowHelper(t.whoFrom,t.whoTo)
      //lines.last.add(t)
    }
    //lines.toArray.map{_.toString}
    linesMap.toList.sortBy(_._1._2).map{_._2.toString}
  }
}
class ElectionChanged(val originalElected:List[Int],val newElected:List[Int]) {
  val newWinners : Set[Int] = newElected.toSet--originalElected
  val newLosers : Set[Int] = originalElected.toSet--newElected
  def descList(candidates:Array[Candidate]) : List[String] = newLosers.toList.map{"- "+candidates(_).name}++newWinners.toList.map{"+ "+candidates(_).name}
  def desc(candidates:Array[Candidate]) : String = descList(candidates).mkString(" ")
}

/** Types of margins you may want to compute */
object MarginTypes {
  class MarginType(val n:Int,val name:String,val filenamebase:String,val allowFirstPrefChanges:Boolean,val allowPreferencesTakenFromNewLoser:Boolean,val allowPreferencesGivenToNewWinner:Boolean,val allowAboveTheLine:Boolean) {
    def filename:String = filenamebase+".html"
  }
  
  val AllowAnything = new MarginType(0,"allowing any changes","marginsAllow1PrefsChanges",true,true,true,true)
  val No1Prefs = new MarginType(1,"BTL, not letting first preferences change","marginsNo1Prefs",false,true,true,false)
  val NoFromLoser = new MarginType(2,"BTL, no first preferences changed, and no preferences taken from new loser","marginsNoFromLoser",false,false,true,false)
  val NoFromLoserOrToWinner = new MarginType(3,"BTL, no first preferences changed, and no preferences take from new loser or given to new winner","marginsNoToWinner",false,false,false,false)
  val No1PrefsATL = new MarginType(4,"not letting first preferences change","marginsNo1PrefsATL",false,true,true,true)
  val NoFromLoserATL = new MarginType(5,"no first preferences changed, and no preferences taken from new loser","marginsNoFromLoserATL",false,false,true,true)
  val NoFromLoserOrToWinnerATL = new MarginType(6,"no first preferences changed, and no preferences take from new loser or given to new winner","marginsNoToWinnerATL",false,false,false,true)
  
  val allTypes = List(AllowAnything,No1Prefs,NoFromLoser,NoFromLoserOrToWinner,No1PrefsATL,NoFromLoserATL,NoFromLoserOrToWinnerATL)
  val numTypes = allTypes.length
}


case class CanGiveAwayVotes(val from:CandidateIndex,val votes:TamperableVotes)

class TransferResolution(val tamperedVotes:List[TamperedVote],val remaining:List[CanGiveAwayVotes]) {
  def toMargin(count:Int) = new Margin(count,tamperedVotes.toArray)
}

/**
 * Nothing to do with the count directly, but compute some margin information 
 */
class TamperingMarginCalculation(val helper:FederalSenateCountHelper,excludeList:List[CandidateIndex],val marginOptions:WhatMarginInformationToCompute,/** If this is being done after the step is complete (at end of election, special case ending). */afterStepCount:Boolean) {
  def tallys = helper.tallys
  def ballots = helper.ballots
  def continuingCandidates = helper.continuingCandidates
  def report = helper.report
  val data = helper.data
  
  val continuingLowToHigh = continuingCandidates.orderedList.reverse.toArray
    // compute margin info
  val excludeCutoff = excludeList.map{tallys(_)}.max
  
  /** Candidates at the top of their party, or all elected above them */
  val canUseATL : Set[CandidateIndex] = {
    val candidates = helper.data.candidates
    def topOfRemaining(inGroup:List[CandidateIndex]) : CandidateIndex = inGroup.sortBy { candidates(_).position  }.head
    helper.continuingCandidates.orderedList.groupBy { candidates(_).group }.filter{_._1 != "UG"}.map{case (_,candidatesInGroup)=>topOfRemaining(candidatesInGroup)}.toSet
  }

  for (c<-continuingCandidates.orderedList) tryToExcludeCandidate(c) // Can't be done in parallel as they modify tallys field. TODO work around.
    
  def tryToExcludeCandidate(c:CandidateIndex) {  
    if (marginOptions.candidatesToTryToExclude.map{_.contains(c)}.getOrElse(true) && !excludeList.contains(c) /* && countNumber==334 &&c==7 - interesting for TAS 2016 */) {
      processCandidate(c,false)
      if (marginOptions.mayUseATLs) processCandidate(c,true)
    }
  }

  /** Reorder list, moving elements that satisfy f to the front */
  def moveToFront[T](l:List[T])(f:T=>Boolean) : List[T] = {
    val (isf,notf) = l.partition(f)
    isf++notf
  }
  
    
          private def getTransfers1(surplusVotesAvailable:List[CanGiveAwayVotes],neededVotes:List[(CandidateIndex,Tally)]) : TransferResolution = neededVotes match {
            case Nil => new TransferResolution(Nil,surplusVotesAvailable) // all done
            case (_,0)::nt => getTransfers1(surplusVotesAvailable,nt) // 0 votes wanted
            case (candidateNeedingVotes,numberVotesNeeded)::nt =>
              if (numberVotesNeeded<0) throw new IllegalArgumentException
              if (surplusVotesAvailable.isEmpty) throw new CannotFindTamperableVotesException
              val CanGiveAwayVotes(surplusWho,surplus) = surplusVotesAvailable.head
              val willGiveVotes = surplus.votes min (numberVotesNeeded)
              val willGivePapers = Math.ceil((willGiveVotes+surplus.maxPapersLostToRounding)/surplus.tv).toInt
              val (actualVotesWillGivePapers:List[ActualListOfTamperableVotes],remainingActualVotes:List[ActualListOfTamperableVotes]) = surplus.src.map{_.split(willGivePapers)}.unzip // really option, not list.
              val tampering = new TamperedVote(surplusWho,candidateNeedingVotes,willGivePapers,willGiveVotes,actualVotesWillGivePapers.headOption,surplus.isAboveTheLine)
              val remainingNeeded = numberVotesNeeded-willGiveVotes
              val newNeeded = if (remainingNeeded==0) nt else (candidateNeedingVotes,remainingNeeded)::nt
              val remainingAvailable = new TamperableVotes(surplus.tv,surplus.papers-willGivePapers,remainingActualVotes.headOption,surplus.maxPapersLostToRounding,surplus.isAboveTheLine)
              val newAvailable = if (remainingAvailable.votes>0) (new CanGiveAwayVotes(surplusWho,remainingAvailable))::surplusVotesAvailable.tail else surplusVotesAvailable.tail
              val rest = getTransfers1(newAvailable,newNeeded)
              if (tampering.numPapers>0) new TransferResolution(tampering::rest.tamperedVotes,rest.remaining) else rest
          }

  def name(index:CandidateIndex) = helper.data.candidates(index).name
  def countNumber = helper.report.history.length+(if (afterStepCount) 0 else 1)
  def processCandidate(c:CandidateIndex,useATL:Boolean) {
      //println("processCandidate("+c+" "+data.candidates(c).name+","+useATL+")")
      val takeFromCtamperable = ballots(c).getTamperableVotes(data.candidates(c).position-1,marginOptions.shouldIncludeSpecificVotes,useATL)
      val availableFromCtamperable = for (tamperable<-takeFromCtamperable if tamperable.votes>0) yield new CanGiveAwayVotes(c,tamperable)
      lazy val takeFromCAll = ballots(c).getTamperableVotesConsideringEverythingTamperable(marginOptions.shouldIncludeSpecificVotes)
      val availableFromCAll = for (tamperable<-takeFromCAll if tamperable.votes>0) yield new CanGiveAwayVotes(c,tamperable)

      /** Generalization of getVotes that gets the specific papers involved in providing needed votes from the start of the surplusVotesAvailable list */
      def getTransfers(surplusVotesAvailable:List[CanGiveAwayVotes],neededVotes:List[(CandidateIndex,Tally)]) : TransferResolution =  {
          val (canHaveATL,requireBTL) = neededVotes.partition{p=>canUseATL(p._1)}
          val (availableATL,availableBTL) = surplusVotesAvailable.partition { _.votes.isAboveTheLine }
          // first do those that require BTLs with just BTLs
          val btls = getTransfers1(availableBTL,requireBTL)
          val atls = getTransfers1(moveToFront((btls.remaining++availableATL).sortBy { - _.votes.tv })(g=> c==g.from),canHaveATL) 
          //if (availableATL.length>=0) {
            // println("ATL Have available "+availableATL+" may use "+canHaveATL+" BTL have available "+availableBTL+" required by "+requireBTL)
          //}
          new TransferResolution(btls.tamperedVotes++atls.tamperedVotes,atls.remaining)
      }
  
      def computeRecipients(votesLost:Int) : Array[TamperedVoteNoPapers] = { // work out who gets the votes
        val votesGivenTo : Array[Int] = new Array[Int](continuingLowToHigh.length) // votes given to candidate by index position 
        var numGivenTo = 0 // number of candidates given votes
        var togo = votesLost
        def tally(index:Int) = { // modified tally due to getting extra votes
          val who = continuingLowToHigh(index)
          if (who==c) Integer.MAX_VALUE else votesGivenTo(index)+tallys(who)
        }
        def give(index:Int,n:Int) { votesGivenTo(index)+=n; togo-=n }
        while (togo>0) {
          numGivenTo+=1
          val toCatchUp = tally(numGivenTo)-tally(numGivenTo-1) // distribute all votes until the next smallest candidate is reached
          val maxToAll = togo/numGivenTo
          val giveEach = maxToAll min toCatchUp
          for (i<-0 until numGivenTo) give(i,giveEach)
          if (giveEach<toCatchUp) // distribute remaining lot, weakest candidate first
            for (i<-0 until togo) give(i,1)          
        }
        val result = for (i<-0 until numGivenTo if votesGivenTo(i)>0) yield new TamperedVoteNoPapers(c,continuingLowToHigh(i),votesGivenTo(i))
        result.toArray
      }
      def isExcluded(votesLost:Int) : Boolean = { // see if candidate c is excluded if s/he loses votesLost votes.
        //println(s"trying to exclude $votesLost for candidate $c tally "+tallys(c))
        val recipients = computeRecipients(votesLost)
        // change universe to have candidate c lose votesLost votes
        helper.tallys(c)-=votesLost
        for (recipient<-recipients) helper.tallys(recipient.candidateTo)+=recipient.numVotes
        val newOrdered = continuingCandidates.orderedList.sortBy { -helper.tallys(_) } 
        val nowExcluded = helper.candidatesForExclusion(newOrdered,false)
        //println("Ordered : "+newOrdered+" nowExcluded="+nowExcluded.mkString(",")); Thread.sleep(300)
        // unchange universe
        helper.tallys(c)+=votesLost
        for (recipient<-recipients) helper.tallys(recipient.candidateTo)-=recipient.numVotes
        nowExcluded.contains(c)
      }
      def computeRecipientsDetailed(votesLost:Int,requireTamperable:Boolean) : Margin = {
        val fudgeFactorForRoundingWhenDeterminingTamperMargins = if (marginOptions.shouldOptimizeBinarySearchRerunElection && requireTamperable) 20 else 0 // edit this by hand if needed.
        val surplusVotesAvailable:List[CanGiveAwayVotes] = if (requireTamperable) availableFromCtamperable else availableFromCAll
        val neededVotes:List[(CandidateIndex,Tally)] = computeRecipients(votesLost+fudgeFactorForRoundingWhenDeterminingTamperMargins).map{tvnp=>(tvnp.candidateTo,tvnp.numVotes)}.toList
        val neededVoteeOptimized = if (marginOptions.shouldOptimizeBinarySearchRerunElection && requireTamperable) binarySearchOptimizeVotesNeeded(surplusVotesAvailable,neededVotes) else neededVotes 
        val res = getTransfers(surplusVotesAvailable,neededVoteeOptimized)
        res.toMargin(countNumber)
      }
      /** Optimize votesNeededByLowerThanCToPassC by binary searching on lower values and rerunning the election to see if it works. Returns optimized list */
      def binarySearchOptimizeVotesNeeded(surplusVotesAvailable:List[CanGiveAwayVotes],votesNeededByLowerThanCToPassC : List[(CandidateIndex,Tally)],debug:Boolean=false) : List[(CandidateIndex,Tally)]= {
        if (marginOptions.shouldOptimizeBinarySearchRerunElection && votesNeededByLowerThanCToPassC.length<5 ) { // for each candidate, work out the minimum number of votes needed by binary search on a rerun election.
            def works(baseVotesNeeded:List[(CandidateIndex,Tally)],changingCandidate:CandidateIndex)(changeTo:Int) : Boolean = {
              // if (changingCandidate!= -1) println("Trying to change candidate "+name(changingCandidate)+" to "+changeTo+" to try to exclude "+name(c))
              try {
                 val transfers = getTransfers(surplusVotesAvailable,baseVotesNeeded.map{case (who,howmany) => if (who==changingCandidate) (who,changeTo) else (who,howmany)})
                 val margin = transfers.toMargin(countNumber)
                 //println("Transfers = "+transfers)
                 val newWinners = helper.rerunElectionModifiedData(margin, "bso"+countNumber+" candidate "+c, debug && changingCandidate== -1, false,None)
                 //if (changingCandidate!= -1) println("Trying to change candidate "+name(changingCandidate)+" to "+changeTo+" to try to exclude "+name(c)+" winners = "+newWinners) 
                 !newWinners.contains(c)
              } catch { case _:CannotFindTamperableVotesException => false }
            }
            var votesNeeded = votesNeededByLowerThanCToPassC
            import scala.util.control.Breaks._
            breakable { for (_<- 1 to 2) { // may improve with 2 passes
              val stateAtStartOfPass = votesNeeded
              for ((changingCandidate,currentNeeded)<-votesNeeded) {
                val actuallyNeeded = BinarySearch.findLowest(works(votesNeeded,changingCandidate),0,currentNeeded)
                votesNeeded=votesNeeded.map{case (who,howmany) => if (who==changingCandidate) (who,actuallyNeeded) else (who,howmany)}
              }      
              if (votesNeeded == stateAtStartOfPass) break // don't waste time with another pass
            }}
            if (!works(votesNeeded,-1)(-1)) throw new CannotFindTamperableVotesException// sanity check that we are not returning an illegal state
            votesNeeded
        } else votesNeededByLowerThanCToPassC
      }
      val fudgeFactorForRoundingWhenDeterminingTamperMargins = if (marginOptions.shouldOptimizeBinarySearchRerunElection) 20 else 0 // edit this by hand if needed.
      def searchWithIndirectionHardToDetect(naiveMargin:Int,allowFromC:Boolean) : Margin = {
          //println("searchWithIndirectionHardToDetect("+allowFromC+")")
          val reallyAvailableFromCtamperable : List[CanGiveAwayVotes] = if (allowFromC) availableFromCtamperable else Nil
          val assumeTakenFromC = reallyAvailableFromCtamperable.map{_.votes.votes}.sum min naiveMargin
          val tallyToMoveEveryoneElseTo : Tally = tallys(c)-assumeTakenFromC+fudgeFactorForRoundingWhenDeterminingTamperMargins+1 
          // now need to make everyone else continuing >=tallyToMoveEveryoneElseTo.
          val surplusVotes : List[(CandidateIndex,Tally)] = continuingCandidates.orderedList.map{nc=>(nc,tallys(nc)-tallyToMoveEveryoneElseTo)}
         // val surplusVotesFromPeopleAheadOfC : List[(CandidateIndex,Tally)] = continuingCandidates.orderedList.takeWhile {_ != c}.map{nc=>(nc,tallys(nc)-tallyToMoveEveryoneElseTo)}
          val availableFromOthers : List[CanGiveAwayVotes] = surplusVotes.flatMap{
            case (who,surplus) if surplus>fudgeFactorForRoundingWhenDeterminingTamperMargins && who!=c =>
              val untruncated = ballots(who).getTamperableVotes(data.candidates(who).position-1,marginOptions.shouldIncludeSpecificVotes,useATL)
              val buffer = new collection.mutable.ListBuffer[TamperableVotes]
              var remainingSurplus = surplus-fudgeFactorForRoundingWhenDeterminingTamperMargins
              for (t<-untruncated) if (remainingSurplus>0) {
                val nt = if (remainingSurplus>=t.votes) t else t.scaleVotesBackTo(remainingSurplus)
                remainingSurplus-=nt.votes
                if (nt.votes>0) buffer+=nt
              }
              for (tamperable<-buffer.toList) yield new CanGiveAwayVotes(who,tamperable)
            case _ => Nil
          }
          val availableFromOthersSorted = availableFromOthers.sortBy(- _.votes.tv)
          val allVotesAvailable = reallyAvailableFromCtamperable++availableFromOthersSorted
          //println("SWI round="+countNumber+" c="+c+" "+data.candidates(c).name+" surplusVotes="+surplusVotes)
          val votesNeededByLowerThanCToPassC1 : List[(CandidateIndex,Tally)]  = moveToFront(surplusVotes.dropWhile(_._1 !=c).tail.map{pair=>(pair._1,0 max -pair._2)})({pair=> !canUseATL(pair._1)})// .filter{_._2<0})
          val extraNeededToCompensateForFewerTakenFromCThanExpected = 0 max Math.ceil((assumeTakenFromC-votesNeededByLowerThanCToPassC1.map{_._2}.sum+fudgeFactorForRoundingWhenDeterminingTamperMargins+1).toDouble/(1+votesNeededByLowerThanCToPassC1.length)).toInt
          //println("extraNeededToCompensateForFewerTakenFromCThanExpected="+extraNeededToCompensateForFewerTakenFromCThanExpected)
          val extraJustAvailable = ((allVotesAvailable.map { _.votes.votes }.sum-votesNeededByLowerThanCToPassC1.map{_._2}.sum)*0.5/(1 max votesNeededByLowerThanCToPassC1.length)).toInt max 0
          val extraUsed = 0 // extraNeededToCompensateForFewerTakenFromCThanExpected max extraJustAvailable
          // could possibly increase as much as you can - binary search on getTransfers - could produce better results
          val votesNeededByLowerThanCToPassC : List[(CandidateIndex,Tally)]  = votesNeededByLowerThanCToPassC1.map{pair=>(pair._1,extraUsed+pair._2)}
          //println("SWI round="+countNumber+" c="+c+" "+data.candidates(c).name+" allVotesAvailable="+allVotesAvailable+" votesNeededByLowerThanCToPassC="+votesNeededByLowerThanCToPassC)
          val votesNeededOptimized = binarySearchOptimizeVotesNeeded(allVotesAvailable,votesNeededByLowerThanCToPassC)
          val optimizedMargin = getTransfers(allVotesAvailable,votesNeededOptimized).toMargin(countNumber)
          if (optimizedMargin.papers<=0) {
            println("Had optimizedMargin.papers="+optimizedMargin.papers+" with votesNeededOptimized="+votesNeededOptimized+" votesNeededByLowerThanCToPassC="+votesNeededByLowerThanCToPassC+" allVotesAvailable="+allVotesAvailable)
          }
          if (marginOptions.optimizeLowTransferValuesFromNewLoser && allowFromC) { // try, one at a time (while things are improving) taking the lowest transfer value from the potential transfers from C. 
            var bestMargin = optimizedMargin
            var fromC = reallyAvailableFromCtamperable
            var extraVotesNeeded = 0
            //println("Trying optimizeLowTransferValuesFromNewLoser fromC="+fromC+" extraVotesNeeded="+extraVotesNeeded)
            while (!fromC.isEmpty) {
              extraVotesNeeded+=fromC.last.votes.votes
              fromC = fromC.take(fromC.length-1)
              val allVotesAvailable1 = fromC++availableFromOthersSorted
              try {
                //println("Trying optimizeLowTransferValuesFromNewLoser fromC="+fromC+" extraVotesNeeded="+extraVotesNeeded+" allVotesAvailable1="+allVotesAvailable1)
                val votesNeededOptimized1 = binarySearchOptimizeVotesNeeded(allVotesAvailable1,votesNeededOptimized.map{case (cand,tally)=>(cand,tally+extraVotesNeeded+40)}) // consider reducing 40 // candidates may need extra votes to pass
                //println("Trying optimizeLowTransferValuesFromNewLoser fromC="+fromC+" extraVotesNeeded="+extraVotesNeeded+" votesNeededOptimized1="+votesNeededOptimized1)
                val optimizedMargin1 = getTransfers(allVotesAvailable1,votesNeededOptimized1).toMargin(countNumber)
                //println("Trying optimizeLowTransferValuesFromNewLoser fromC="+fromC+" extraVotesNeeded="+extraVotesNeeded+" optimizedMargin1.papers="+optimizedMargin1.papers+" votesNeededOptimized1="+votesNeededOptimized1)
                if (optimizedMargin1.papers<bestMargin.papers) bestMargin=optimizedMargin1 else fromC=Nil // either got better, or break out of loop as it is not going to get any better.
              } catch { case _ : CannotFindTamperableVotesException => fromC=Nil } // break out of loop
            }
            bestMargin
          } else optimizedMargin
      }
      val margin = BinarySearch.findLowest(isExcluded,0,tallys(c)-tallys(continuingLowToHigh(0))+1)
      //for (papers<-ballots(c).numPapersToGenerateVotes(margin,false,data.candidates(c).position-1)) 
      if (useATL) {
        try { // try tampering that would be easily detected
          report.addMarginInfo(c,computeRecipientsDetailed(margin,false),MarginTypes.AllowAnything)
        } catch { 
          case _:CannotFindTamperableVotesException => // this does happen, and is not all that rare. println("Did not expect to fail at finding tamperable votes when all tamperable. May be a rare rounding artifact")
        }  
      }
      val simpleWorked = 
        try { // try tampering that would be hard to detect
          report.addMarginInfo(c,computeRecipientsDetailed(margin,true),if (useATL) MarginTypes.No1PrefsATL else MarginTypes.No1Prefs)
          true
        } catch { case _:CannotFindTamperableVotesException => false }
      if (marginOptions.optimizeLowTransferValuesFromNewLoser || !simpleWorked) { // try a less efficient method - give away all the votes you can, and then take the remainder from elsewhere.
          try {
              report.addMarginInfo(c,searchWithIndirectionHardToDetect(margin,true),if (useATL) MarginTypes.No1PrefsATL else MarginTypes.No1Prefs)
          } catch { case _:CannotFindTamperableVotesException => }
      }
      if (marginOptions.tryIndirectExclusion) { 
        try { // try tampering that would be hard to detect and is indirect - doesn't give any preferences to new winner or take any from new loser.
          val indirectMargin = searchWithIndirectionHardToDetect(margin,false)
          report.addMarginInfo(c,indirectMargin,if (useATL) MarginTypes.NoFromLoserATL else MarginTypes.NoFromLoser)
          val gaveVotesToWinner : Boolean = {
            val newWinners = helper.rerunElectionModifiedData(indirectMargin, "", false, false,None).toSet
            indirectMargin.tamperings.exists{t=>newWinners.contains(t.whoTo)}
          }
          if (!gaveVotesToWinner) report.addMarginInfo(c,indirectMargin,if (useATL) MarginTypes.NoFromLoserOrToWinnerATL else MarginTypes.NoFromLoserOrToWinner)
        } catch { case _:CannotFindTamperableVotesException =>  }
      }
    }
}
class CannotFindTamperableVotesException extends Exception
class TamperedVoteNoPapers(val candidateFrom:CandidateIndex,val candidateTo:CandidateIndex,val numVotes:Tally)


object BinarySearch {
  /** Find the lowest integer in the inclusive range low...high that satisfies p. E.g if p is _>5, this would return 6. Assumes p is monotonic */
  def findLowest(p:Int=>Boolean,low:Int,high:Int) : Int = {
    if (low==high) low else {
      val mid=(low+high)/2
      if (p(mid)) findLowest(p,low,mid) else findLowest(p,mid+1,high)
    }
  }
}

