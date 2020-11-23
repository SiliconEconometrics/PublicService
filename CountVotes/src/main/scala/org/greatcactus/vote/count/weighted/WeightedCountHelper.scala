/*
    Copyright 2016-2020 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.weighted

import org.greatcactus.vote.count._
import org.greatcactus.vote.count.MainDataTypes._
import org.greatcactus.vote.count.ballots.{DVote, ElectionData, ElectionDataFastIO}
import org.greatcactus.vote.count.margin.{Margin, NormalElectionOutcome, Tampering}


/**
 * Do the work of counting for preferential multi elected candidate jurisdictions where excess over quota is distributed in a weighted manner. 
 * http://www.austlii.edu.au/au/legis/cth/consol_act/cea1918233/s273.html
 */
abstract class WeightedCountHelper(val data:ElectionData,candidatesToBeElected:NumberOfCandidates,ticketRoundingChoices:Map[String,Int],ecDeemedOrder:Seq[CandidateIndex],val printDebugMessages:Boolean,ineligibleCandidates:Set[CandidateIndex]=Set.empty) {

  def voteMultipleToDealWithFractionalVotes : Tally = 1 // if you want to round votes to say, 6 decimal places, set this to 1 000 000 and all tallys will be multiplied by that value.
  var wantToComputeMargins:Boolean = false
  val numCandidates : NumberOfCandidates = data.candidates.length
  val continuingCandidates : ContinuingCandidates = new ContinuingCandidates(ecDeemedOrder,numCandidates)
  continuingCandidates--=ineligibleCandidates
  var remainingVacancies : NumberOfCandidates = candidatesToBeElected
  var currentCountNumber : CountNumber = 1
  val candidatesToHaveSurplusDistributed = new collection.mutable.Queue[CandidateIndex]
  // Distribute votes to first preferences
  val originalVotes : PlainVotes = {
    val votes : Array[DVote] = for (v<-data.makeVotes(ticketRoundingChoices)) yield new DVote(0,v.numVoters*voteMultipleToDealWithFractionalVotes,v.preferences,v.src).skipNotContinuingCandidates(continuingCandidates.set)
    //DVote.saveMichelleFormat(new java.io.File("Michele/"+data.meta.electionName.longFileName+".txt"),votes,data)
    val res = new PlainVotes
    res.add(votes)
    res
  }
  def shouldSeparateBallotsBySourceCountNumber : HowSplitByCountNumber
  val ballots : Array[WeightedVotes] = originalVotes.splitByNextContinuing(continuingCandidates)._1.map{pv=>{val res = new WeightedVotes(shouldSeparateBallotsBySourceCountNumber); res.add(pv,TransferValueOne,1,1,pv.numBallots);res }}
  val tallys : Array[Tally] = ballots.map{_.numBallots}
  val firstPreferences : Array[Tally] = java.util.Arrays.copyOf(tallys,tallys.length)
  val report = new ElectionResultReport(data.candidates,ineligibleCandidates,printDebugMessages)
  report.setTallyFunction{i=>tallys(i).toDouble/voteMultipleToDealWithFractionalVotes}
  report.setPapersFunction({i=>ballots(i).numBallots/voteMultipleToDealWithFractionalVotes},{i=>ballots(i).numBallotsATL/voteMultipleToDealWithFractionalVotes})
  def reportsShouldUseMajorCountNumber = false
  report.useMajorCountNo = reportsShouldUseMajorCountNumber

  // determine quota
  val numFormalVotes: Tally = tallys.sum
  def computeQuota(numFormalVotes:Tally,candidatesToBeElected:NumberOfCandidates): Tally = voteMultipleToDealWithFractionalVotes* ((numFormalVotes/voteMultipleToDealWithFractionalVotes)/(1+candidatesToBeElected)+1)
  val quota : Tally = computeQuota(numFormalVotes,candidatesToBeElected)
  report.quota=quota/voteMultipleToDealWithFractionalVotes
  report.initialCountDone()

  /** In order for a candidate to have a surplus, the candidate must have a tally > the quota. But sometimes small surpluses are disregarded. Require tally>quota+amountNeededAboveQuotaToBeASurplus for it to be considered a surplus. See ACT 2020 for where this is used */
  def amountNeededAboveQuotaToBeASurplus : Tally = 0

  def declareElected(c:CandidateIndex,reason:String) {
        // println("Elected "+c+" because "+reason)
        continuingCandidates-=c
        if (tallys(c)>quota+amountNeededAboveQuotaToBeASurplus) candidatesToHaveSurplusDistributed.enqueue(c)
        remainingVacancies-=1
        report.declareElected(c,reason)    
  }
  /** 3 way ties are not well defined in most legislation. Set this if require a countback count where all are different. */
  def orderSurplusesBasedOnUniqueNumbersInCountback : Boolean = false
  def applyHighestOfLastTwoIfInTheMiddleOfAMultistageEliminationOrRedistribution = false
  def stopIfTwoContinuingCandidatesAndOneVacancy : Boolean = true // stop early if there are two continuing candidates, and one vacancy, and elect the higher.
 
  def endCountReorderAndCheckElected(/* true if in the middle of a multi-part transfer where highest-of-last-two should not be applied */ exclusionOrDistributionInProgress:Boolean): Unit = {
    continuingCandidates.reorder(tallys)
    // (18)  Notwithstanding any other provision of this section, where the number of continuing candidates is equal to the number of remaining unfilled vacancies, those candidates shall be elected.
    // if (continuingCandidates.length==remainingVacancies && !(finishExclusionEvenIfAllWillBeElected && exclusionOrDistributionInProgress) && !(finishSuplusDistributionEvenIfEveryoneWillGetElected&& (candidatesToHaveSurplusDistributed.nonEmpty||continuingCandidates.orderedList.exists(tallys(_) >= quota))) ) for (c<-continuingCandidates.orderedList) declareElected(c,"Remaining")
    //  (17)  In respect of the last vacancy for which two continuing candidates remain, the continuing candidate who has the larger number of votes shall be elected notwithstanding that that number is below the quota, and if those candidates have an equal number of votes the Australian Electoral Officer for the State shall have a casting vote but shall not otherwise vote at the election.
    if (stopIfTwoContinuingCandidatesAndOneVacancy && continuingCandidates.length==2 && remainingVacancies==1 && candidatesToHaveSurplusDistributed.isEmpty && (applyHighestOfLastTwoIfInTheMiddleOfAMultistageEliminationOrRedistribution | !exclusionOrDistributionInProgress)) { // note if you don't have the candidatesToHaveSurplusDistributed.isEmpty you can get different results. This may be a bug in the legislation.
      if (wantToComputeMargins) {
        Tampering.lookForOportunitiesToTamperThisRound(this,candidatesForExclusion.candidateToExclude,normalElectionOutcome,afterStepCount = true)
      }
      declareElected(continuingCandidates.head,"Highest of remaining 2")
    }
    val reachedQuota = continuingCandidates.orderedList.filter{tallys(_)>=quota}
    val reachedQuotaOrdered = if (orderSurplusesBasedOnUniqueNumbersInCountback) {
      reachedQuota.groupBy(tallys(_)).toList.sortBy(- _._1).flatMap{case (_,equal) => orderCandidatesBasedOnCountbackRequiringUniqueCounts(equal)}
    } else {
      for (c<-reachedQuota;disambiguate<-continuingCandidates.couldAECHaveToMakeDecision(c)) report.addECDecision(disambiguate)
      reachedQuota
    }
    for (c<-reachedQuotaOrdered) {
        declareElected(c,"Reached Quota")
    }
    // (18)  Notwithstanding any other provision of this section, where the number of continuing candidates is equal to the number of remaining unfilled vacancies, those candidates shall be elected.
    if (continuingCandidates.length==remainingVacancies && !(finishExclusionEvenIfAllWillBeElected && exclusionOrDistributionInProgress) && !(finishSuplusDistributionEvenIfEveryoneWillGetElected&& (candidatesToHaveSurplusDistributed.nonEmpty||continuingCandidates.orderedList.exists(tallys(_) >= quota))) ) for (c<-continuingCandidates.orderedList) declareElected(c,"Remaining")
    report.finishCount()
    if (report.history.length!=currentCountNumber) throw new IllegalArgumentException("Report history length = "+report.history.length+" currentCountNumber="+currentCountNumber)
    currentCountNumber+=1
  }
  endCountReorderAndCheckElected(false)
  def transferExcess(candidate:CandidateIndex)

  /** Transfer a surplus to other candidates in one transfer by giving each vote an equal transfer value. Used by AEC and VEC. */
  def transferSurplusAllWithSameTransferValue(candidate:CandidateIndex): Unit = { // AEC Step (9) If not finished, transfer surplus of elected candidates.
    // AEC 9a compute transfer value
    val rawvotes: PlainVotes = ballots(candidate).asPlain
    val surplus = tallys(candidate) - quota
    val transferValue = MainDataTypes.transferValue(surplus,rawvotes.numBallots)
    transferSurplusAllWithSameTransferValue(candidate,rawvotes,surplus,transferValue,0,0)
  }

  def roundTransferValuesTo6DigitsWhenOverridingCurrentTransferValue : Boolean = false // bug in Elections ACT 2020 code.
  /** Transfer a surplus to other candidates in one transfer by giving each vote an equal transfer value. Used by Elections ACT. */
  def transferSurplusAllWithSameTransferValueLastParcel(candidate:CandidateIndex): Unit = {
    val lastParcel = ballots(candidate).getLastParcel
    val (rawvotes,numExhaustedSetAside) = lastParcel.moreVotes.removeExhausted(continuingCandidates)
    val surplus = tallys(candidate) - quota
    val anticipatedTransferValue = if (rawvotes.numBallots>0) MainDataTypes.transferValue(surplus,rawvotes.numBallots) else MainDataTypes.transferValue(2,1) // convert divide by zero into cap in line below.
    val transferValue : TransferValue = if (lastParcel.transferValue < anticipatedTransferValue) {
      if (roundTransferValuesTo6DigitsWhenOverridingCurrentTransferValue) { // why would anyone do this????
         RationalTransferValue(Math.round(lastParcel.transferValue.toDouble*1000000),1000000)
      } else lastParcel.transferValue
    } else anticipatedTransferValue
    val lostDueToCappedTransferValue = surplus.toDouble-transferValue.toDouble*rawvotes.numBallots
    transferSurplusAllWithSameTransferValue(candidate,rawvotes,surplus,transferValue,numExhaustedSetAside,lostDueToCappedTransferValue)
  }

  /** Transfer a surplus to other candidates in one transfer where each vote has an equal transfer value. Used by AEC, VEC, Elections ACT. */
  def transferSurplusAllWithSameTransferValue(candidate:CandidateIndex,rawvotes: PlainVotes,surplus:Tally,transferValue: TransferValue,numExhaustedSetAside:Tally,lostDueToCappedTransferValue:Double): Unit = {// AEC Step (9) If not finished, transfer surplus of elected candidates.
    val transferValueDescription = new TransferValueComputationSingleValue(surplus,rawvotes.numBallots+numExhaustedSetAside,numExhaustedSetAside,transferValue.toDouble)
    // AEC 9b distribute
    val (distributedToCandidate : Array[PlainVotes],numExhausted:Tally) = rawvotes.splitByNextContinuing(continuingCandidates)
    val exhaustedTally = roundDownRecordRounding(transferValue,numExhausted)
    report.declareCandidateDistributed(candidate, surplus.toDouble/voteMultipleToDealWithFractionalVotes, rawvotes.numBallots/voteMultipleToDealWithFractionalVotes, transferValueDescription,Nil,  rawvotes.numBallots/voteMultipleToDealWithFractionalVotes, 0, exhaustedTally.toDouble/voteMultipleToDealWithFractionalVotes,distributedMakesSense = false)
    if (lostDueToCappedTransferValue>1e-10 && printDebugMessages) System.out.println("Lost "+lostDueToCappedTransferValue/voteMultipleToDealWithFractionalVotes+" due to capping transfer value to "+transferValue.toDouble)
    report.addLostDueToRounding(lostDueToCappedTransferValue/voteMultipleToDealWithFractionalVotes) // Elections ACT puts this into rounding.
    report.addExhaustedVotes(exhaustedTally/voteMultipleToDealWithFractionalVotes)
    report.addExhaustedPapers(numExhausted/voteMultipleToDealWithFractionalVotes)
    tallys(candidate)=quota
    ballots(candidate).clear()
    if (transferValue.numerator>0) { // (AEC 25)
      for (nextChoice<-continuingCandidates.orderedList) {
        val giveVotes : PlainVotes = distributedToCandidate(nextChoice)
        val tally = roundDownRecordRounding(transferValue,giveVotes.numBallots)
        // println("Transfer value = "+transferValue.numerator+"/"+transferValue.denominator+"  numBallots="+giveVotes.numBallots+" tally="+tally)
        ballots(nextChoice).add(giveVotes, transferValue, currentCountNumber,currentCountNumber,tally)
        tallys(nextChoice)+=tally
      }
    }
    clearRoundingPending()
    endCountReorderAndCheckElected(false)
    report.finishMajorCount()
  }


  var roundingPending : Double = 0.0

  /** If set to true, then when rounding down, actually round to nearest. */
  def roundToNearestInsteadOfDown = false;

  def roundDownRecordRounding(transferValue:TransferValue,ballots:PaperCount): Tally = {
    var (tally,rounding) = transferValue.mul(ballots)
    if (roundToNearestInsteadOfDown && rounding>0.5) {
      tally+=1
      rounding-=1
    }
    roundingPending+=rounding
    tally
  }
  def roundDownRecordRoundingReverseDirection(transferValue:TransferValue,ballots:PaperCount): Tally = {
    val (tally,rounding) = transferValue.mul(ballots)
    roundingPending-=rounding
    tally
  }
  def clearRoundingPending() {
    report.addLostDueToRounding(roundingPending/voteMultipleToDealWithFractionalVotes)
    roundingPending=0.0
  }

  def sortedForExclusion(votes:WeightedVotes) : List[((TransferValue,CountNumber),PlainVotes)] 
  def finishExclusionEvenIfAllVacanciesFilled : Boolean
  def finishExclusionEvenIfAllWillBeElected : Boolean
  def interruptExclusionAtStartOfExclusionIfAllWillBeElected : Boolean = false // interpretation used by AEC in 2019. Section 18 is checked before an exclusion is done.
  def finishSuplusDistributionEvenIfEveryoneWillGetElected : Boolean           // interpretation used by AEC in 2016.
  def finishSurplusDistributionEvenIfEveryoneIsElected : Boolean = false        // interpretation used by Elections ACT in 2020

  def ignoreVoteMultipleToDealWithFractionalVotesForExhaustedVotesWhenCandidateExcluded : Boolean = false // but done by ACT in 2020.
  
  // step (13AA) federal
  /** Perform the exclusion of given candidates */
  def excludeCandidates(candidateToExclude:CandidateToExclude) {

    val candidates:List[CandidateIndex] = candidateToExclude.candidateToExclude
    var choiceWasArbitary : Option[Set[CandidateIndex]] = candidateToExclude.tiesBetweenBrokenByECChoice // continuingCandidates.couldAECHaveToMakeDecision(candidates.head)

    continuingCandidates--=candidates
    // 18 can interrupt exclusion at this point.
    val dontBotherDoingAnyWork = continuingCandidates.length==remainingVacancies && interruptExclusionAtStartOfExclusionIfAllWillBeElected

    // (13AA)(a) transfer TV=1 votes with TV 1, can be done
    val summedVotes : WeightedVotes = candidates.map{ballots(_)}.reduce{_ + _}
    val orderedWork = if (dontBotherDoingAnyWork) Nil else sortedForExclusion(summedVotes)
    val (lastTV,lastFC) = orderedWork.lastOption.map{_._1}.getOrElse((0,0))
    for (((transferValue:TransferValue,fromCount),votes:PlainVotes)<-orderedWork) if (remainingVacancies>0 || finishExclusionEvenIfAllVacanciesFilled) {
      report.declareCandidatesExcluded(candidates, votes.whereCameFrom,transferValue.toDouble)
      if (shouldSeparateBallotsBySourceCountNumber eq FullySplitByCountNumber) report.fromCountReference(fromCount)
      for (disambiguate<-choiceWasArbitary) report.addECDecision(disambiguate)
      choiceWasArbitary=None // only report on first page
      for (c<-candidates) {
        val oldLostToRounding = ballots(c).lostToRounding
        ballots(c).removeTransferValueAndCountNumber(transferValue, fromCount)
        tallys(c)=ballots(c).roundedTally
        val newLostToRounding = ballots(c).lostToRounding
        //println(s"oldLostToRounding=$oldLostToRounding newLostToRounding=$newLostToRounding "+data.candidates(c).name)
        report.addLostDueToRounding((newLostToRounding-oldLostToRounding)/voteMultipleToDealWithFractionalVotes)
      }

      val (split : Array[PlainVotes],numExhausted:Tally) = votes.splitByNextContinuing(continuingCandidates)
      var exhaustedVotes = roundDownRecordRounding(transferValue,numExhausted).toDouble/voteMultipleToDealWithFractionalVotes;
      if (ignoreVoteMultipleToDealWithFractionalVotesForExhaustedVotesWhenCandidateExcluded) {
        val intExhaustedVotes = Math.floor(exhaustedVotes)
        roundingPending+=(exhaustedVotes-intExhaustedVotes)*voteMultipleToDealWithFractionalVotes
        exhaustedVotes=intExhaustedVotes
      }
      report.addExhaustedVotes(exhaustedVotes)
      report.addExhaustedPapers(numExhausted/voteMultipleToDealWithFractionalVotes)
      for (c<-continuingCandidates.orderedList) {
        val togo : PlainVotes = split(c)
        val tally = roundDownRecordRounding(transferValue,togo.numBallots) // (13AA)(b)(ii)
        //println("TV "+transferValue+" c="+c+" numBallots="+togo.numBallots+" tally="+tally);
        tallys(c)+=tally
        ballots(c).add(togo,transferValue,currentCountNumber,fromCount,tally) // (13AA)(b)(iii)
      }
      clearRoundingPending()
      val isLastIteration = transferValue==lastTV && lastFC == fromCount
      endCountReorderAndCheckElected(!isLastIteration)
    }
    if (orderedWork.isEmpty) {
      report.declareCandidatesExcluded(candidates,Nil,0)
      endCountReorderAndCheckElected(false)
    }
    report.finishMajorCount()
  }

  /** Get the list of candidates to exclude */
  def candidatesForExclusion : CandidateToExclude
  /** Used when looking for margins. Computes the candidate to exclude if the ordered candidates and tallys were different. It's not a disaster if this is not perfectly accurate, although it is helpful. */
  def hypotheticalCandidatesForExclusion(newOrdered: List[CandidateIndex], newTallys: Array[Tally]) : List[CandidateIndex] = List(newOrdered.last)

  def getCandidateToDistribute : CandidateIndex
  def getCandidateToDistributeOrderElected : CandidateIndex = candidatesToHaveSurplusDistributed.dequeue()
  def getCandidateToDistributeHighestCountWithCountback : CandidateIndex = {
    val contenders = candidatesToHaveSurplusDistributed.toList
    val topscore = contenders.map{tallys(_)}.max
    val equal : List[CandidateIndex] = contenders.filter{tallys(_)==topscore}
    val candidate : CandidateIndex = equal match {
      case Nil => throw new IllegalArgumentException("No candidates with tally "+topscore)
      case unique::Nil => unique
      case _ => orderCandidatesBasedOnCountbackRequiringUniqueCounts(equal).head
    }
    candidatesToHaveSurplusDistributed.dequeueFirst(_ == candidate)
    candidate
  }
  def orderCandidatesBasedOnCountbackRequiringUniqueCounts(equal:List[CandidateIndex]) : List[CandidateIndex] = if (equal.length<=1) equal else {
    report.searchRecentHistoryForCandidatesWithHigherCountsAndOrder(equal) match {
        case Some(ordered) => ordered
        case None => // need to choose randomly
          report.addECDecision(equal.toSet)
          if (equal.forall(ecDeemedOrder.contains(_))) ecDeemedOrder.filter(equal.contains(_)).toList
          else throw new IllegalArgumentException("Need to choose order between "+equal)
      }
  }
    
  def distributeOrExclude() {
    //println("remaining vacancies : "+remainingVacancies+" candidates available : "+continuingCandidates.orderedList.mkString(","))
    if (candidatesToHaveSurplusDistributed.isEmpty) {
      if (continuingCandidates.orderedList.isEmpty) {
        println(data.meta.electionName.electorate + " : No one to have surplus distributed, and no continuing candidates. Remaining vacancies = "+remainingVacancies+" of "+candidatesToBeElected)
        ElectionReport.saveReports(new java.io.File("distributeOrExcludeError"),report,data.meta)
      }
      val toExclude = candidatesForExclusion
      if (wantToComputeMargins) {
        Tampering.lookForOportunitiesToTamperThisRound(this,toExclude.candidateToExclude,normalElectionOutcome,afterStepCount = false)
      }
      excludeCandidates(toExclude)
    } else transferExcess(getCandidateToDistribute)
  }

  /** Save reports of best margins. */
  def rerunModifiedVersions(marginReportLocation:ReportSaver) {
    for (((delta,properties),margin)<-report.marginsRecorder.best) {
      val name = properties.toString+delta.desc(data.candidates)
      rerunElectionWithModifiedData(Some(margin),name,Some(SaveReportsForNewRunOfElection(marginReportLocation.subdir(name),saveReports = true,saveDatafile = true)))
    }
  }


  def run(marginReportLocation:Option[ReportSaver]=None): Unit = {
    this.wantToComputeMargins = marginReportLocation.isDefined
    if (printDebugMessages) println("Running count for "+data.meta.electionName.shortPrintName)
    while (remainingVacancies>0 || finishSurplusDistributionEvenIfEveryoneIsElected&&candidatesToHaveSurplusDistributed.nonEmpty&&continuingCandidates.orderedList.nonEmpty) distributeOrExclude()
    report.freeReferencesWhenAllDone()
    afterRunningStatistics()
    for (mrl<-marginReportLocation) rerunModifiedVersions(mrl.subdir("Tampering"))
  }

  /** IF there is some analysis to do after running the election, do it here. */
  def afterRunningStatistics(): Unit = { }

  /**
    * Find a candidate to exclude, with tie resolution based on a countback, where all candidates have to have a different value
    * for the countback to work.
    * @return
    */
  def getCandidateToExcludeWithExplicitCountbackRequiringAllDifferent : CandidateToExclude = {
    val excludeScore = tallys(continuingCandidates.orderedList.last)
    val tied = continuingCandidates.orderedList.filter{tallys(_)==excludeScore}
    if (tied.length==1) new CandidateToExclude(tied,None)
    else {
      // do a count back
      for (r<-report.history.reverseIterator) {
        val subtallys = tied.map{r.totalAtEnd(_)}
        if (subtallys.toSet.size==tied.length) { // all have different values
          val min = subtallys.min
          println("Found "+tied.length+" different values, count = "+report.getPrettyCountName)
          for (c<-tied) if (r.totalAtEnd(c)==min) return new CandidateToExclude(List(c),None)
          throw new IllegalStateException("Should not get here")
        }
      }
      // choose randomly (or at least based on the EC choice)
      val randomlyChosen = ecDeemedOrder.reverseIterator.find(tied.contains).getOrElse({
        println("EC deemed order does not distinguish between "+tied.mkString(","))
        tied.head
      })
      new CandidateToExclude(List(randomlyChosen),Some(tied.toSet))
    }
  }

  /**
    * Get the candidate to exclude, with ties broken by the last best count. Used by ACT , or federal elections when multiple exclusions are not allowed (or the AEC chooses to not implement them).
    * Lower candidates are discarded even if some candidates have the same values.
    */
  def getCandidateToExcludeBasedOnLastBestCount : CandidateToExclude = {
    val who = continuingCandidates.orderedList.last
    new CandidateToExclude(List(who),continuingCandidates.couldAECHaveToMakeDecision(who))
  }
  class CandidateToExclude(val candidateToExclude: List[CandidateIndex],val tiesBetweenBrokenByECChoice : Option[Set[CandidateIndex]])

  // TODO remove ??? and implement everywhere used.
  /** Get a new worker that could be used to recount the election, with the provided data which may or may not be the same as the current data. */
  def newWorker(newdata:ElectionData):WeightedCountHelper = ???

  /** Rerun the election, with some changes specified by margin. Return the candidates elected. */
  def rerunElectionWithModifiedData(changes:Option[Margin], name:String,reportLocation:Option[SaveReportsForNewRunOfElection]) : NormalElectionOutcome = {
    val newdata = changes match {
      case Some(margin) => data.tamper(margin,"_tamper_exclude_"+name)
      case None => data
    }
    val newworker = newWorker(newdata)
    newworker.run(None)
    for (SaveReportsForNewRunOfElection(baseDir:ReportSaver,saveReports:Boolean, saveDatafile:Boolean)<-reportLocation) {
      val dir = baseDir.subdir(name)
      if (saveReports) ElectionReport.saveReports(dir,newworker.report,newdata.meta)
      if (saveDatafile) dir.write(newdata.meta.electionName.shortFileName+".txt",w=>ElectionDataFastIO.savePickled(newdata,w))
    }
    val winners = newworker.report.electedCandidates.toList
    val stillIn = newworker.continuingCandidates.orderedList.filterNot(winners.contains)
    new NormalElectionOutcome(winners,stillIn)
  }

  lazy val normalElectionOutcome: NormalElectionOutcome = rerunElectionWithModifiedData(None,"normaladvance",None)

}

case class SaveReportsForNewRunOfElection(baseDir:ReportSaver,saveReports:Boolean, saveDatafile:Boolean)