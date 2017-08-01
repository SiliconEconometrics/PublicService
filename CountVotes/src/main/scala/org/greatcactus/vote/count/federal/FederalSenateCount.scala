/*
    Copyright 2016-2017 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.federal

import org.greatcactus.vote.count._
import org.greatcactus.vote.count.weighted._
import org.greatcactus.vote.count.MainDataTypes._
import FederalSenateCount._

import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenTraversableOnce
import java.util.Arrays

import org.greatcactus.vote.count.federal.FederalSenateCount2016App.run

import scala.collection.mutable.ListBuffer

object FederalSenateCount2013App extends App {

  def run(state:String,toBeElected:Int,aecDeemedOrder:Seq[Int],ticketRoundingChoices:Map[String,Int],/** For double dissolution, use to determine which senators get 6 years */ secondRoundNumElected:Option[Int]=None) {
    FederalSenateCount.run("2013", FederalElectionData.load2013(state), state, toBeElected, aecDeemedOrder, ticketRoundingChoices, secondRoundNumElected)
  }
  //run("VIC",12,DeducedAEC2013Orders.vic,DeducedAEC2013TicketSplits.vic,Some(6))
  run("NT",2,DeducedAEC2013Orders.nt,DeducedAEC2013TicketSplits.nt)
  run("ACT",2,DeducedAEC2013Orders.act,DeducedAEC2013TicketSplits.act)
  //run("VIC",6,DeducedAEC2013Orders.vic,DeducedAEC2013TicketSplits.vic)
  //run("NSW",6,DeducedAEC2013Orders.nsw,DeducedAEC2013TicketSplits.nsw)
  //run("SA",6,DeducedAEC2013Orders.sa,DeducedAEC2013TicketSplits.sa)
}

object FederalSenateCount2016App extends App {

  def run(state:String,toBeElected:Int,aecDeemedOrder:Seq[Int],doHalf:Boolean=true,ineligible:Set[Int]=Set.empty) {
    val data = FederalElectionData.load2016(state)
    FederalSenateCount.run("2016", data, state+ineligible.map{i=>" no "+data.candidates(i).name}.mkString(""), toBeElected, aecDeemedOrder, Map.empty, if (doHalf) Some(toBeElected/2) else None,ineligible,None,true,true,true)
  }
  
  /* Do everything */

  run("NSW",12,DeducedAEC2016Orders.nsw)
  run("QLD",12,DeducedAEC2016Orders.qld)
  run("VIC",12,DeducedAEC2016Orders.vic)
  run("NT",2,DeducedAEC2016Orders.nt,false)
  run("ACT",2,DeducedAEC2016Orders.act,false)
  run("TAS",12,DeducedAEC2016Orders.tas)
  run("WA",12,DeducedAEC2016Orders.wa)
  run("SA",12,DeducedAEC2016Orders.sa)
  run("WA",12,DeducedAEC2016Orders.wa,true,Set(25,45)) // Scott Ludlum and Rod Culleton excluded
  run("QLD",12,DeducedAEC2016Orders.qld,true,Set(89))  // Larissa Waters.
  // see what would happen if HOULT, Michelle (candidate id 13) or COHEN, Nicky (candidate 14) were ineligible for reason of citizenship
  //run("TAS",12,DeducedAEC2016Orders.tas,true,Set(14))
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
  val senators : Map[String,Int] = Map("NSW"->12,"QLD"->12,"VIC"->12,"NT"->2,"ACT"->2,"TAS"->12,"WA"->12,"SA"->12)
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
  // below flags are a mess and really should be made neater.
  val defaultMarginInformation = WhatMarginInformationToCompute.none 
  val whenRerunningModifiedElectionsDoForAllCandidatesNotJustElectedOnes = false // slows it down even more
  val doMarginOptimization = false // really slows it down
  val doIndirectExclusion = true // also slows it down
  val useATLMargins = true
  /** Whether we should try to reduce low transfer values from the person being excluded */ val optimizeLowTransferValuesFromNewLoser=true // you guessed it. This will slow it down too.
  
  def run(year:String,rawvotes:ElectionData,state:String,toBeElected:Int,aecDeemedOrder:Seq[Int],ticketRoundingChoices:Map[String,Int],/** For double dissolution, use to determine which senators get 6 years */ secondRoundNumElected:Option[Int]=None,ineligible:Set[Int]=Set.empty,explicitSaver:Option[ReportSaver]=None,prohibitMultipleEliminations:Boolean=false,finishExclusionEvenIfAllWillBeElected:Boolean=false,finishSuplusDistributionEvenIfEveryoneWillGetElected:Boolean=false) {
    val reportDir = explicitSaver.getOrElse(new ReportSaverDirectory(new java.io.File("Federal"+year+"Reports/"+state)))
    rawvotes.printStatus()
    val mainMarginInfo = if (doMarginOptimization) {
      val preworker = new FederalSenateCountHelper(rawvotes,toBeElected,ticketRoundingChoices,aecDeemedOrder,WhatMarginInformationToCompute.none,false,ineligible,prohibitMultipleEliminations,finishExclusionEvenIfAllWillBeElected,finishSuplusDistributionEvenIfEveryoneWillGetElected)
      preworker.run(None)
      new WhatMarginInformationToCompute(true,true,Some(preworker.report.electedCandidates.toSet),true,doIndirectExclusion,optimizeLowTransferValuesFromNewLoser,useATLMargins)
    } else defaultMarginInformation
    val worker = new FederalSenateCountHelper(rawvotes,toBeElected,ticketRoundingChoices,aecDeemedOrder,mainMarginInfo,true,ineligible,prohibitMultipleEliminations,finishExclusionEvenIfAllWillBeElected,finishSuplusDistributionEvenIfEveryoneWillGetElected)
    worker.run(Some(reportDir))
    ElectionReport.saveReports(reportDir,worker.report,rawvotes)
    for (secondRound<-secondRoundNumElected) {
      val worker2a = new FederalSenateCountHelper(rawvotes,secondRound,ticketRoundingChoices,aecDeemedOrder,defaultMarginInformation,true,ineligible,prohibitMultipleEliminations,finishExclusionEvenIfAllWillBeElected,finishSuplusDistributionEvenIfEveryoneWillGetElected)
      worker2a.run(None)
      ElectionReport.saveReports(reportDir.subdir("Electing"+secondRound),worker2a.report,rawvotes)
      val worker2b = new FederalSenateCountHelper(rawvotes,secondRound,ticketRoundingChoices,aecDeemedOrder,defaultMarginInformation,true,(0 until rawvotes.numCandidates).toSet--worker.report.electedCandidates,prohibitMultipleEliminations,finishExclusionEvenIfAllWillBeElected,finishSuplusDistributionEvenIfEveryoneWillGetElected)
      worker2b.run(None)
      ElectionReport.saveReports(reportDir.subdir("Electing"+secondRound+"OutOf"+toBeElected),worker2b.report,rawvotes)
    }
  }

}

/**
 * Do the work of counting for the federal senate, based on 
 * http://www.austlii.edu.au/au/legis/cth/consol_act/cea1918233/s273.html
 */
class FederalSenateCountHelper(data:ElectionData,candidatesToBeElected:Int,ticketRoundingChoices:Map[String,Int],aecDeemedOrder:Seq[CandidateIndex],marginOptions:WhatMarginInformationToCompute,printDebugMessages:Boolean,ineligibleCandidates:Set[CandidateIndex],prohibitMultipleEliminations:Boolean,override val finishExclusionEvenIfAllWillBeElected:Boolean,override val finishSuplusDistributionEvenIfEveryoneWillGetElected : Boolean)
     extends WeightedCountHelper(data,candidatesToBeElected,ticketRoundingChoices:Map[String,Int],aecDeemedOrder,printDebugMessages,ineligibleCandidates) {
  
  override def shouldSeparateBallotsBySourceCountNumber : Boolean = false
  override def getCandidateToDistribute : CandidateIndex = getCandidateToDistributeOrderElected

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
  // Step (9) If not finished, transfer surplus of elected candidates.
  def transferExcess(candidate:CandidateIndex) {
  // 9a compute transfer value
    val rawvotes : PlainVotes = ballots(candidate).asPlain
    val surplus = tallys(candidate)-quota
    val transferValue = surplus.toDouble/rawvotes.numBallots 
    val transferValueDescription = new TransferValueComputationSingleValue(surplus,rawvotes.numBallots,0,transferValue)
    // 9b distribute
    val (distributedToCandidate : Array[PlainVotes],numExhausted:Tally) = rawvotes.splitByNextContinuing(continuingCandidates)
    val exhaustedTally = roundDownRecordRounding(numExhausted*transferValue)
    report.declareCandidateDistributed(candidate, surplus, rawvotes.numBallots, transferValueDescription,Nil,  rawvotes.numBallots, 0, exhaustedTally,false)
    report.addExhaustedVotes(exhaustedTally)
    report.addExhaustedPapers(numExhausted)
    tallys(candidate)=quota
    ballots(candidate).clear()
    if (transferValue>0) { // (25)
      for (nextChoice<-continuingCandidates.orderedList) {
        val giveVotes : PlainVotes = distributedToCandidate(nextChoice)
        val tally = roundDownRecordRounding(transferValue*giveVotes.numBallots)
        ballots(nextChoice).add(giveVotes, transferValue, currentCountNumber,tally)
        tallys(nextChoice)+=tally
      }
    }
    clearRoundingPending()
    endCountReorderAndCheckElected(false)
    report.finishMajorCount()
  }

  // step (13) exclusions
  
  // step (13AA)
  override def sortedForExclusion(votes:WeightedVotes) = votes.sortedByWeight // will do highest first; (a) is a special case of (b)
  override def finishExclusionEvenIfAllVacanciesFilled : Boolean = false

  // Step 13(A)
  def candidatesForExclusion(orderedCandidates:List[CandidateIndex],isReal:Boolean) : List[CandidateIndex] = if (prohibitMultipleEliminations) List(orderedCandidates.last) else {
    val notionalVotes : Array[Tally] = new Array[Tally](numCandidates)
    val reverseOrder : List[CandidateIndex] = orderedCandidates.reverse
    if (true) { // compute notional votes
      var last = 0 // Should be adjustment for adjusted notional vote, (13C) but I can't see how this could ever be non-zero 
      for (c<-reverseOrder) {
         last+=tallys(c)
         notionalVotes(c)=last
       }
    }
    val debugMultipleExclusion : Boolean = false // isReal && report.history.size==9
    // if (isReal) println("Count "+report.history.size+" debugMultipleExclusion="+debugMultipleExclusion)
    
    val vacancyShortFall = computeVacancyShortfall(orderedCandidates)
     // 13(A) (a)  a continuing candidate (in this subsection called Candidate A ) shall be identified, if possible, who, of the continuing candidates who each have a number of notional votes equal to or greater than the vacancy shortfall, stands lower or lowest in the poll;
    if (debugMultipleExclusion) {
      for (i<-0 until (reverseOrder.length min 100)) {
        val c = reverseOrder(i)
        println(data.candidates(c).name+"\t notional votes "+notionalVotes(c)) 
      }
      println("Vacancy Shortfall "+vacancyShortFall)
      println("Leading Shortfall "+leadingShortfall(orderedCandidates))
      val candidateA : Option[CandidateIndex] = reverseOrder.find { notionalVotes(_)>vacancyShortFall }
      for (a<-candidateA) println("Candidate A "+data.candidates(a).name)
      
    }
     
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
    if (debugMultipleExclusion) {
       for (b<-candidateBOpt) println("Candidate B "+data.candidates(b).name)
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
        if (debugMultipleExclusion) {
          for (c<-candidateCopt) println("Candidate C "+data.candidates(c).name)
        }

        candidateCopt match {
          case Some(candidateC) => orderedCandidates.dropWhile { _ != candidateC } // continuingCandidates.candidateAndLower(candidateC) //  (ii)  Candidate C and all other continuing candidates who stand lower in the poll than that candidate may be excluded in a bulk exclusion.
          case None =>
            if (isReal && printDebugMessages) println("Legislation [s273, 13(A)(d)(i)] requires candidate C to exist, but it does not in count "+currentCountNumber+". Candidate B is "+data.candidates(candidateB).name)
            List(reverseOrder.head)
        }
      case None => List(reverseOrder.head) // lowest candidate, 13(a)
    }
     // (13B)  Where, apart from this subsection, the number of continuing candidates after a bulk exclusion under subsection (13A) would be fewer than the number of remaining unfilled vacancies, subsection (13A) shall operate to exclude only the number of candidates, beginning with the candidate who stands lowest in the poll, that would leave sufficient continuing candidates to fill the remaining unfilled vacancies.
    val maxExcludable = orderedCandidates.length-remainingVacancies
    if (toExclude13A.length>maxExcludable) toExclude13A.takeRight(maxExcludable) else toExclude13A
  }
  def candidatesForExclusionWithMarginComputation(afterStepCount:Boolean) : List[CandidateIndex] = {
    val excludeList = candidatesForExclusion(continuingCandidates.orderedList,true)
    if (marginOptions.wantAnyMarginInfo) new TamperingMarginCalculation(this,excludeList,marginOptions,afterStepCount) // do tampering computation
    excludeList    
  }
  
  /** Do tampers and rerun to check they are right. */
  def rerunModifiedVersions(marginReportLocation:Option[ReportSaver]) {
      for (marginType<-MarginTypes.allTypes;candidate<-0 until numCandidates;margin<-report.margins(marginType.n)(candidate)) if (whenRerunningModifiedElectionsDoForAllCandidatesNotJustElectedOnes || report.electedCandidates.contains(candidate)) {
        val modifiedElected = rerunElectionModifiedData(margin,data.candidates(candidate).name,true,true,marginReportLocation.map{_.subdir(marginType.filenamebase)})
        val originalElected = report.electedCandidates.toList
        report.addMarginTamperableEffectInfo(candidate,new ElectionChanged(originalElected,modifiedElected),marginType)
      }
  }

  /** Rerun the election, with some changes specified by margin. Return the candidates elected. */
  def rerunElectionModifiedData(margin:Margin,name:String,saveReports:Boolean,saveDatafile:Boolean,marginReportLocation:Option[ReportSaver]) : List[CandidateIndex] = {
        val newdata = data.tamper(margin,"_tamper_exclude_"+name)
        val newworker = new FederalSenateCountHelper(newdata,candidatesToBeElected,ticketRoundingChoices,aecDeemedOrder,WhatMarginInformationToCompute.none,false,ineligibleCandidates,prohibitMultipleEliminations,finishExclusionEvenIfAllWillBeElected,finishSuplusDistributionEvenIfEveryoneWillGetElected)
        newworker.run(None)
        for (baseDir<-marginReportLocation) {
          val dir = baseDir.subdir(name)
          if (saveReports) ElectionReport.saveReports(dir,newworker.report,newdata)
          if (saveDatafile) dir.write(newdata.name+".txt",w=>ElectionDataFastIO.savePickled(newdata,w))
        }
        newworker.report.electedCandidates.toList
  }
  
  def run(marginReportLocation:Option[ReportSaver]) {
    run()
    if (printDebugMessages) CheckEffectOfOddVotersForTickets.check(data, report)
    if (marginOptions.shouldIncludeSpecificVotes)  rerunModifiedVersions(marginReportLocation.map{_.subdir("Tampering")})
  }
  
  
}
