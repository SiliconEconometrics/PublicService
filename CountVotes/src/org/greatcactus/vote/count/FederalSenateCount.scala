package org.greatcactus.vote.count
import FederalSenateCount._
import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenTraversableOnce
import java.util.Arrays

object FederalSenateCount2013App extends App {

  def run(state:String,toBeElected:Int,aecDeemedOrder:Seq[Int],ticketRoundingChoices:Map[String,Int],/** For double dissolution, use to determine which senators get 6 years */ secondRoundNumElected:Option[Int]=None) {
    FederalSenateCount.run("2013", FederalElectionData.load2013(state), state, toBeElected, aecDeemedOrder, ticketRoundingChoices, secondRoundNumElected)
  }
  //run("VIC",12,DeducedAEC2013Orders.vic,DeducedAEC2013TicketSplits.vic,Some(6))
  //run("NT",2,DeducedAEC2013Orders.nt,DeducedAEC2013TicketSplits.nt)
  run("VIC",6,DeducedAEC2013Orders.vic,DeducedAEC2013TicketSplits.vic)
  //run("NSW",6,DeducedAEC2013Orders.nsw,DeducedAEC2013TicketSplits.nsw)
  //run("SA",6,DeducedAEC2013Orders.sa,DeducedAEC2013TicketSplits.sa)
}

object FederalSenateCount2016App extends App {

  def run(state:String,toBeElected:Int,aecDeemedOrder:Seq[Int],doHalf:Boolean=true) {
    FederalSenateCount.run("2016", FederalElectionData.load2016(state), state, toBeElected, aecDeemedOrder, Map.empty, if (doHalf) Some(toBeElected/2) else None)
  }
  
  
  //run("VIC",12,DeducedAEC2016Orders.vic)
  run("NT",2,DeducedAEC2016Orders.nt,false)
}

//tiebreaking orders, deduced from tie resolutions in actual AEC count.
//The first one is the one who is treated as if they have a higher tally. 
//Individual separated lists indicated that shorter concatenated lists don't need to be ordered.
object DeducedAEC2013Orders {
  val vic = List(88,85,23,54)
  val nt = List()
  val sa = List(53,41)
  val wa = List(29,15)
  val nsw = List(54,52,82)++List(72,68,104)++List(7,56)++List(96,12,20)
  val act = List()
  val tas = List(44,40,38)++List(50,17)
  val qld = List(59,25)
}
object DeducedAEC2016Orders {
  val vic = List()
  val nt = List()
  val sa = List()
  val wa = List()
  val nsw = List()++List()
  val act = List()
  val tas = List()
  val qld = List()
}

// // Where the odd ones out go in the case where the number of ticket votes is not divisible by the num of tickets.
object DeducedAEC2013TicketSplits {
  val vic : Map[String,Int] = Map("E"->0,"F"->0,"L"->0,"AF"->0,"AI"->0,"AK"->2) // AK is the only one that matters
  val nt  : Map[String,Int] = Map("H"->1)
  val sa  : Map[String,Int] = Map("A"->1,"V"->0) // only A matters
  val wa  : Map[String,Int] = Map("E"->2,"P"->0)
  val nsw : Map[String,Int] = Map("AI"->0,"AR"->0) // only AI matters
  val act : Map[String,Int] = Map()
  val tas : Map[String,Int] = Map("S"->0) // doesn't matter
  val qld : Map[String,Int] = Map("H"->0,"S"->1,"Y"->0) // only S matters
}

object CheckEffectOfOddVotersForTickets {
  def check(data:ElectionData,report:ElectionResultReport) {
    for (i<-0 until data.groupInfo.length) if (data.groupInfo(i).tickets.length>1) { // if (data.ticketRoundingChoices(i)>1) {
      val suffix = if (data.ticketRoundingChoices(i)>1) "" else " but there are no odd votes anyway"
      val group = data.groupInfo(i)
      val tickets = group.tickets
      val votes = for (t<-tickets) yield new DVote(0,1,t,null)
      def differs(round:Int) : Boolean = {
        // println("Continuing candidates "+report.history(i).end.continuingCandidates.toList.sorted.mkString(",")+" elected "+report.history(i).end.electedCandidates.toList.sorted.mkString(",")+" excluded "+report.history(i).end.excludedCandidates.toList.sorted.mkString(","))
        val nvotes : Array[CandidateIndex] = for (v<-votes) yield v.skipNotContinuingCandidates(report.history(round).end.continuingCandidates).current
        //println("Group "+group.groupId+" round "+round+" prefs "+nvotes.mkString(","))
        nvotes.toSet.size>1
      }
      val roundOpt : Option[CountNumber] = (0 until report.history.length).find(differs)
      roundOpt match {
        case Some(round) => println("Group "+group.groupId+" multiple tickets matter first in round "+(round+1)+suffix)
        case None => println("Group "+group.groupId+" multiple tickets never matter"+suffix)
      }
    }
  }
}


object FederalSenateCount {
  type CandidateIndex = Int
  type CountNumber = Int
  type TransferValue = Double
  type Tally = Int
  
  def run(year:String,rawvotes:ElectionData,state:String,toBeElected:Int,aecDeemedOrder:Seq[Int],ticketRoundingChoices:Map[String,Int],/** For double dissolution, use to determine which senators get 6 years */ secondRoundNumElected:Option[Int]=None) {
    val reportDir = new java.io.File("Federal"+year+"Reports/"+state)
    rawvotes.printStatus()
    val worker = new FederalSenateCountHelper(rawvotes,toBeElected,ticketRoundingChoices,aecDeemedOrder)
    worker.run()
    ElectionReport.saveReports(reportDir,worker.report,rawvotes)
    for (secondRound<-secondRoundNumElected) {
      val worker2a = new FederalSenateCountHelper(rawvotes,secondRound,ticketRoundingChoices,aecDeemedOrder)
      worker2a.run()
      ElectionReport.saveReports(new java.io.File(reportDir,"Electing"+secondRound),worker2a.report,rawvotes)
      val worker2b = new FederalSenateCountHelper(rawvotes,secondRound,ticketRoundingChoices,aecDeemedOrder,(0 until rawvotes.numCandidates).toSet--worker.report.electedCandidates)
      worker2b.run()
      ElectionReport.saveReports(new java.io.File(reportDir,"Electing"+secondRound+"OutOf"+toBeElected),worker2b.report,rawvotes)
    }
  }

}
 // A stack of votes that all have the same transfer value and would be physically in the same stack 
// for paper counting  (usually same prefs[upto] except in the first count and when batch eliminating).
class PlainVotes {
  private val allVotes = new ArrayBuffer[DVote]
  private var countsOriginating : Set[CountNumber] = Set.empty   //which counts the vote came from
  private var numVotes = 0    // total pieces of paper
  private var numVotesATL = 0
  private var roundedTally = 0   
  /** Split up votes by next preference, returning the split and the number of exhausted votes */
  def splitByNextContinuing(continuingCandidates : ContinuingCandidates) : (Array[PlainVotes],Tally) = {
    val res = Array.fill(continuingCandidates.numCandidates)(new PlainVotes)
    var numExhausted = 0
    //println("Splitting "+numVotes)
    for (v<-allVotes) {
      val next = v.skipNotContinuingCandidates(continuingCandidates.set)
      if (next.isExhausted) { 
        numExhausted+=1
        //println("Exhausted "+v)
      } else res(next.current).add(next)
    }
    (res,numExhausted)
  }
  def add(v:DVote) {
    allVotes+=v
    numVotes+=v.numVoters.toInt
    if (v.src.isATL) numVotesATL+=v.numVoters.toInt
  }
  def add(votes:Array[DVote]) {
    for (v<-votes) add(v)
  }
  def numBallots : Int = numVotes
  def numBallotsATL : Int = numVotesATL
  def add(moreVotes:PlainVotes,countIndex:CountNumber,roundedTally:Int) {
    countsOriginating=countsOriginating+countIndex
    allVotes++=moreVotes.allVotes
    numVotes+=moreVotes.numVotes
    numVotesATL+=moreVotes.numVotesATL
    this.roundedTally+=roundedTally
  }
  def addWithoutChangingCountIndex(moreVotes:PlainVotes) {
    countsOriginating=countsOriginating++moreVotes.countsOriginating
    allVotes++=moreVotes.allVotes
    numVotes+=moreVotes.numVotes
    numVotesATL+=moreVotes.numVotesATL
    this.roundedTally+=moreVotes.roundedTally
  }
  def whereCameFrom : List[CountNumber] = countsOriginating.toList.sorted
  def getRoundedTally = roundedTally
  /** Number that are hard to detect tampering with at this point - eg first preference, 1 ATL */
  def numDeepIntoPreferences(owningCandidatePositionInParty:Int) : Int = {
    var res = 0
    for (v<-allVotes) {
      val isTamperable = (v.upto!=0) /* first preference */ && (v.src match { // not first preference
         case _:BTL => true
         case _:SATL => false
         case a:ATL => v.upto != owningCandidatePositionInParty // not the first ATL preference        
      })
      if (isTamperable) res+=v.numVoters.toInt
    }
    res
  }
}
class WeightedVotes {
  private val map = new collection.mutable.HashMap[TransferValue,PlainVotes]
  def asPlain : PlainVotes = {
    val res = new PlainVotes
    for (v<-map.values) res.addWithoutChangingCountIndex(v)
    res
  }
  private def pv(transferValue:TransferValue) : PlainVotes = map.getOrElseUpdate(transferValue,new PlainVotes)
  def add(moreVotes:PlainVotes,transferValue:TransferValue,countIndex:CountNumber,roundedTally:Int) {
    if (moreVotes.numBallots>0) pv(transferValue).add(moreVotes,countIndex,roundedTally)
  }
  def numBallots : Int = map.values.toList.map{_.numBallots}.sum
  def numBallotsATL : Int = map.values.toList.map{_.numBallotsATL}.sum
  def computeExactTally : Double = (for ((weight,votes)<-map) yield weight*votes.numBallots).sum
  def roundedTally : Tally = map.values.map{_.getRoundedTally}.sum
  def lostToRounding = computeExactTally-roundedTally
  def + (other:WeightedVotes) : WeightedVotes = {
    var res = new WeightedVotes
    res+=this
    res+=other
    res
  }
  def += (other:WeightedVotes) = {
    for ((weight,votes)<-other.map) pv(weight).addWithoutChangingCountIndex(votes)
  }
  def sortedByWeight : List[(TransferValue,PlainVotes)] = map.toList.sortBy{- _._1}
  def removeTransferValue(tv:TransferValue) { map-=tv }
  def clear() { map.clear() }
  /** Minimum number of papers one needs to generate a given number of votes. Use higher transfer values first. Doesn't perfectly deal with rounding. */
  def numPapersToGenerateVotes(wantedVotes:Tally,requireTamperable:Boolean,owningCandidatePositionInParty:Int) : Option[Int] = {
    var togo = wantedVotes
    var res = 0
    for ((tv,pv)<-sortedByWeight) {
        if (togo>0) {
          val available = if (requireTamperable) pv.numDeepIntoPreferences(owningCandidatePositionInParty) else  pv.numBallots
          val used = ((togo/tv).ceil min available).toInt
          togo-=(used*tv).floor.toInt
          res+=used
        }
    }
    if (togo==0) Some(res) else None
  }
}


/** aecDeemedPreferences is the order in which the AEC should resolve ties... first means going higher in orders. Only needs to contain actually used ones. */
class ContinuingCandidates(aecDeemedOrder:Seq[Int],val numCandidates:Int) {
  /** Ordered highest to lowest */
  var orderedList : List[CandidateIndex] = aecDeemedOrder.toList++((0 until numCandidates).toSet--aecDeemedOrder)
  var set : Set[CandidateIndex] = orderedList.toSet
  
  var aecDeemedOrderMatters : List[Set[CandidateIndex]] = List((0 until numCandidates).toSet)
  /** If candidate c is ambiguous order, return all that are ambiguous wrt it */
  def couldAECHaveToMakeDecision(c:CandidateIndex) : Option[Set[CandidateIndex]] = aecDeemedOrderMatters.find { _.contains(c) }
  
  def -=(candidate:CandidateIndex) {
    set-=candidate
    orderedList=orderedList.filter{_ != candidate}
  }
  def --=(candidates:GenTraversableOnce[CandidateIndex]) {
    set= set--candidates
    val removeset = candidates.toSet
    orderedList=orderedList.filter{!removeset.contains(_)}
  }
  /** Reorder orderedList, with ties broken by current order */
  def reorder(tallys:Array[Int]) {
    orderedList = orderedList.sortBy { -tallys(_) } // relies on it being a stable sort to break ties correctly.
    // deal with aecDeemedOrderMatters
    aecDeemedOrderMatters=aecDeemedOrderMatters.flatMap{s=>
      s.groupBy { tallys(_)  }.map{_._2}.filter { _.size> 1 }
    }
  }
  def top(n:Int) : List[CandidateIndex] = orderedList.take(n)
  def head:CandidateIndex = orderedList.head
  /** Get this candidate and ones lower in the list */
  def candidateAndLower(candidate:CandidateIndex) : List[CandidateIndex] = orderedList.dropWhile { _ != candidate }
  def length = orderedList.length
}

/**
 * Do the work of counting for the federal senate, based on 
 * http://www.austlii.edu.au/au/legis/cth/consol_act/cea1918233/s273.html
 */
class FederalSenateCountHelper(val data:ElectionData,candidatesToBeElected:Int,ticketRoundingChoices:Map[String,Int],aecDeemedOrder:Seq[Int],ineligibleCandidates:Set[Int]=Set.empty) {
  val numCandidates : Int = data.candidates.length
  val continuingCandidates : ContinuingCandidates = new ContinuingCandidates(aecDeemedOrder,numCandidates)
  continuingCandidates--=ineligibleCandidates
  var remainingVacancies : Int = candidatesToBeElected
  var currentCountNumber : CountNumber = 1
  val candidatesToHaveSurplusDistributed = new collection.mutable.Queue[CandidateIndex]
  // Step (8), distribute votes to first preferences, and determine quota
  val originalVotes : PlainVotes = {
    val votes : Array[DVote] = for (v<-data.makeVotes(ticketRoundingChoices)) yield new DVote(0,v.numVoters,v.preferences,v.src).skipNotContinuingCandidates(continuingCandidates.set)
    val res = new PlainVotes
    res.add(votes)
    res
  }
  val ballots : Array[WeightedVotes] = originalVotes.splitByNextContinuing(continuingCandidates)._1.map{pv=>{val res = new WeightedVotes; res.add(pv,1.0,1,pv.numBallots);res }}
  val tallys : Array[Tally] = ballots.map{_.numBallots}
  val firstPreferences : Array[Tally] = Arrays.copyOf(tallys,tallys.length)
  val report = new ElectionResultReport(data.candidates,ineligibleCandidates)
  report.setTallyFunction{i=>tallys(i)}
  report.setPapersFunction({i=>ballots(i).numBallots},{i=>ballots(i).numBallotsATL})
  
  val numFormalVotes = tallys.sum 
  val quota : Int = numFormalVotes/(1+candidatesToBeElected)+1
  report.quota=quota
  report.initialCountDone()
  def shortfall(candidate:CandidateIndex) : Int = quota-tallys(candidate)
  def leadingShortfall(orderedCandidates:List[CandidateIndex]) : Int = shortfall(orderedCandidates.head)
  def computeVacancyShortfall(orderedCandidates:List[CandidateIndex]) : Int = orderedCandidates.take(remainingVacancies).map{shortfall _}.sum
  def unadjustedNotionalVote(orderedCandidates:List[CandidateIndex],candidate:CandidateIndex) : Int = {
    var found = false
    var res = 0
    for (c<-orderedCandidates) {
      if (c==candidate) found=true
      if (found) res+=tallys(c)
    }
    res
  }
  def declareElected(c:CandidateIndex,reason:String) {
        continuingCandidates-=c
        if (tallys(c)>quota) candidatesToHaveSurplusDistributed.enqueue(c)
        remainingVacancies-=1
        report.declareElected(c,reason)    
  }
  def endCountReorderAndCheckElected() {
    continuingCandidates.reorder(tallys)
    // (18)  Notwithstanding any other provision of this section, where the number of continuing candidates is equal to the number of remaining unfilled vacancies, those candidates shall be elected.
    if (continuingCandidates.length==remainingVacancies) for (c<-continuingCandidates.orderedList) declareElected(c,"Remaining")
    //  (17)  In respect of the last vacancy for which two continuing candidates remain, the continuing candidate who has the larger number of votes shall be elected notwithstanding that that number is below the quota, and if those candidates have an equal number of votes the Australian Electoral Officer for the State shall have a casting vote but shall not otherwise vote at the election.
    if (continuingCandidates.length==2 && remainingVacancies==1 && candidatesToHaveSurplusDistributed.isEmpty) declareElected(continuingCandidates.head,"Highest of remaining 2")
    // note if you don't have the candidatesToHaveSurplusDistributed.isEmpty you can get different results. This may be a bug in the legislation.
    for (c<-continuingCandidates.orderedList) {
      if (tallys(c)>=quota) {
        declareElected(c,"Reached Quota")
        for (disambiguate<-continuingCandidates.couldAECHaveToMakeDecision(c)) report.addECDecision(disambiguate)
      }
    }
    report.finishCount()
    currentCountNumber+=1
  }
  endCountReorderAndCheckElected()
  // Step (9) If not finished, transfer surplus of elected candidates.
  def transferExcess(candidate:CandidateIndex) {
  // 9a compute transfer value
    val rawvotes : PlainVotes = ballots(candidate).asPlain
    val surplus = tallys(candidate)-quota
    val transferValue = surplus.toDouble/rawvotes.numBallots 
    // 9b distribute
    val (distributedToCandidate : Array[PlainVotes],numExhausted:Tally) = rawvotes.splitByNextContinuing(continuingCandidates)
    report.declareCandidateDistributed(candidate, surplus, rawvotes.numBallots, transferValue,Nil,  rawvotes.numBallots, 0, roundDownNoTellRounding(numExhausted*transferValue),false)
    report.addExhaustedVotes(roundDown(numExhausted*transferValue))
    report.addExhaustedPapers(numExhausted)
    tallys(candidate)=quota
    ballots(candidate).clear()
    if (transferValue>0) { // (25)
      for (nextChoice<-continuingCandidates.orderedList) {
        val giveVotes : PlainVotes = distributedToCandidate(nextChoice)
        val tally = roundDown(transferValue*giveVotes.numBallots)
        ballots(nextChoice).add(giveVotes, transferValue, currentCountNumber,tally)
        tallys(nextChoice)+=tally
      }
    }
    endCountReorderAndCheckElected()
  }

  def roundDownNoTellRounding(v:Double) : Tally = (v+1e-15).toInt
  def roundDown(v:Double) : Tally = {
    val intv = (v+1e-15).toInt
    report.addLostDueToRounding(v-intv)
    intv
  }
  // step (13) exclusions
  
  // step (13AA)
  def excludeCandidates(candidates:List[CandidateIndex]) {
            
    var choiceWasArbitary : Option[Set[CandidateIndex]] = continuingCandidates.couldAECHaveToMakeDecision(candidates.head)

    continuingCandidates--=candidates
    // (13AA)(a) transfer TV=1 votes with TV 1, can be done
    val summedVotes : WeightedVotes = candidates.map{ballots(_)}.reduce{_ + _}
    for ((transferValue:TransferValue,votes:PlainVotes)<-summedVotes.sortedByWeight) if (remainingVacancies>0) { // will do highest first; (a) is a special case of (b)
      report.declareCandidatesExcluded(candidates, votes.whereCameFrom,transferValue)
      for (disambiguate<-choiceWasArbitary) report.addECDecision(disambiguate)
      choiceWasArbitary=None // only report on first page
      for (c<-candidates) {
        val oldLostToRounding = ballots(c).lostToRounding
        ballots(c).removeTransferValue(transferValue)
        tallys(c)=ballots(c).roundedTally
        val newLostToRounding = ballots(c).lostToRounding
        //println(s"oldLostToRounding=$oldLostToRounding newLostToRounding=$newLostToRounding "+data.candidates(c).name)
        report.addLostDueToRounding(newLostToRounding-oldLostToRounding)
      }

      val (split : Array[PlainVotes],numExhausted:Tally) = votes.splitByNextContinuing(continuingCandidates)
      report.addExhaustedVotes(roundDown(numExhausted*transferValue))
      report.addExhaustedPapers(numExhausted)
      for (c<-continuingCandidates.orderedList) {
        val togo : PlainVotes = split(c)
        val tally = roundDown(togo.numBallots*transferValue) // (13AA)(b)(ii)
        tallys(c)+=tally
        ballots(c).add(togo,transferValue,currentCountNumber,tally) // (13AA)(b)(iii)
      }
      endCountReorderAndCheckElected()
    }
    if (summedVotes.numBallots==0) endCountReorderAndCheckElected()
  }
  // Step 13(A)
  def candidatesForExclusion(orderedCandidates:List[CandidateIndex],isReal:Boolean) : List[CandidateIndex] = {
    val notionalVotes : Array[Tally] = new Array[Tally](numCandidates)
    val reverseOrder : List[CandidateIndex] = orderedCandidates.reverse
    if (true) { // compute notional votes
      var last = 0 // Should be adjustment for adjusted notional vote, (13C) but I can't see how this could ever be non-zero 
      for (c<-reverseOrder) {
         last+=tallys(c)
         notionalVotes(c)=last
       }
    }
    val vacancyShortFall = computeVacancyShortfall(orderedCandidates)
     // 13(A) (a)  a continuing candidate (in this subsection called Candidate A ) shall be identified, if possible, who, of the continuing candidates who each have a number of notional votes equal to or greater than the vacancy shortfall, stands lower or lowest in the poll;
     // val candidateA : Option[CandidateIndex] = reverseOrder.find { notionalVotes(_)>vacancyShortFall }
     
    val candidateBOpt : Option[CandidateIndex] = {
       // (i)  stands lower in the poll than Candidate A, or if Candidate A cannot be identified, has a number of notional votes that is fewer than the vacancy shortfall;
       // mathematically equivalent to has a number of notional votes that is fewer than the vacancy shortfall
       def satisfiesI(c:CandidateIndex) : Boolean =   notionalVotes(c)<vacancyShortFall
       // (ii)  has a number of notional votes that is fewer than the number of votes of the candidate standing immediately higher than him or her in the poll; and
       // mathematically equivalent to has > 0 votes
       def satisfiesII(c:CandidateIndex,higher:CandidateIndex) : Boolean = notionalVotes(c)<tallys(higher)
       // (iii)  if 2 or more candidates satisfy subparagraphs (i) and (ii)--is the candidate who of those candidates stands higher or highest in the poll;
       val ordered = orderedCandidates.toArray
       def satisfiesBoth(candidateRank:Int) : Boolean = satisfiesI(ordered(candidateRank))&&satisfiesII(ordered(candidateRank),ordered(candidateRank-1))
       val resIndex = (1 until ordered.length).find{satisfiesBoth}
       resIndex.map{ordered}
    }
    // println(orderedCandidates)
    val toExclude13A : List[CandidateIndex] = candidateBOpt match {
      case Some(candidateB) if notionalVotes(candidateB)<leadingShortfall(orderedCandidates) => // 13(A) (c)  in a case where Candidate B has been identified and has a number of notional votes fewer than the leading shortfall--Candidate B and any other continuing candidates who stand lower in the poll than that candidate may be excluded in a bulk exclusion; and
        orderedCandidates.dropWhile { _ != candidateB } // continuingCandidates.candidateAndLower(candidateB)
      case Some(candidateB) => // 13(A) (d)  in a case where Candidate B has been identified and has a number of notional votes equal to or greater than the leading shortfall:
        val candidateCopt : Option[CandidateIndex] = { // (i)  a continuing candidate (in this subsection called Candidate C ) shall be identified who:
          def satisfiesA(c:CandidateIndex) = notionalVotes(c)<leadingShortfall(orderedCandidates) // (A)  has a number of notional votes that is fewer than the leading shortfall; and
          val opt = orderedCandidates.find{satisfiesA} // (B)  if 2 or more candidates satisfy sub-subparagraph (A)--is the candidate who of those candidates stands higher or highest in the poll; and
     //     opt.getOrElse(throw new Exception("Legislation requires candidate C to exist"))
          opt
        }
        candidateCopt match {
          case Some(candidateC) => orderedCandidates.dropWhile { _ != candidateC } // continuingCandidates.candidateAndLower(candidateC) //  (ii)  Candidate C and all other continuing candidates who stand lower in the poll than that candidate may be excluded in a bulk exclusion.
          case None =>
            if (isReal) println("Legislation [s273, 13(A)(d)(i)] requires candidate C to exist, but it does not in count "+currentCountNumber+". Candidate B is "+data.candidates(candidateB).name)
            List(reverseOrder.head)
        }
      case None => List(reverseOrder.head) // lowest candidate, 13(a)
    }
     // (13B)  Where, apart from this subsection, the number of continuing candidates after a bulk exclusion under subsection (13A) would be fewer than the number of remaining unfilled vacancies, subsection (13A) shall operate to exclude only the number of candidates, beginning with the candidate who stands lowest in the poll, that would leave sufficient continuing candidates to fill the remaining unfilled vacancies.
    val maxExcludable = orderedCandidates.length-remainingVacancies
    if (toExclude13A.length>maxExcludable) toExclude13A.takeRight(maxExcludable) else toExclude13A
  }
  def candidatesForExclusionWithMarginComputation : List[CandidateIndex] = {
    val continuingLowToHigh = continuingCandidates.orderedList.reverse.toArray
    val excludeList = candidatesForExclusion(continuingCandidates.orderedList,true)
    // compute margin info
    val excludeCutoff = excludeList.map{tallys(_)}.max
    for (c<-continuingCandidates.orderedList) if (!excludeList.contains(c)) {
      def isExcluded(votesLost:Int) : Boolean = { // see if candidate c is excluded if s/he loses votesLost votes.
        //println(s"trying to exclude $votesLost for candidate $c tally "+tallys(c))
        val recipients = computeRecipients(votesLost)
        // change universe to have candidate c lose votesLost votes
        tallys(c)-=votesLost
        for (recipient<-recipients) tallys(recipient.who)+=recipient.votes
        val newOrdered = continuingCandidates.orderedList.sortBy { -tallys(_) } // TODO propagate changes backwards - tie resolution for c not handled fully yet
        val nowExcluded = candidatesForExclusion(newOrdered,false)
        //println("Ordered : "+newOrdered+" nowExcluded="+nowExcluded.mkString(",")); Thread.sleep(300)
        // unchange universe
        tallys(c)+=votesLost
        for (recipient<-recipients) tallys(recipient.who)-=recipient.votes
        nowExcluded.contains(c)
      }
      def computeRecipients(votesLost:Int) : Array[Recipient] = { // work out who gets the votes
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
        val result = for (i<-0 until numGivenTo if votesGivenTo(i)>0) yield new Recipient(continuingLowToHigh(i),votesGivenTo(i))
        result.toArray
      }
      val margin = BinarySearch.findLowest(isExcluded,0,tallys(c)-tallys(continuingLowToHigh(0))+1)
      for (papers<-ballots(c).numPapersToGenerateVotes(margin,false,data.candidates(c).position-1))
        report.addMarginInfo(c,margin,papers,computeRecipients(margin),false)
      for (papers<-ballots(c).numPapersToGenerateVotes(margin,true,data.candidates(c).position-1))
        report.addMarginInfo(c,margin,papers,computeRecipients(margin),true)
    }
    excludeList    
  }
  def distributeOrExclude() {
    if (candidatesToHaveSurplusDistributed.isEmpty) excludeCandidates(candidatesForExclusionWithMarginComputation)
    else transferExcess(candidatesToHaveSurplusDistributed.dequeue())
  }
  
  def run() {
    while (remainingVacancies>0) distributeOrExclude()  
    report.freeReferencesWhenAllDone()
    CheckEffectOfOddVotersForTickets.check(data, report)
  }
  
  
  
}

object BinarySearch {
  /** Find the lowest integer in the inclusive range low...high that satisfies p. E.g if p is _>5, this would return 6. Assumes p is monotonic */
  def findLowest(p:Int=>Boolean,low:Int,high:Int) : Int = {
    if (low==high) low else {
      val mid=(low+high)/2
      if (p(mid)) findLowest(p,low,mid) else findLowest(p,mid+1,high)
    }
  }
}

