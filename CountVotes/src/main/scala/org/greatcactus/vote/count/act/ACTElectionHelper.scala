/*
    Copyright 2020 Silicon Econometrics Pty. Ltd.

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


package org.greatcactus.vote.count.act


import org.greatcactus.vote.count.MainDataTypes._
import org.greatcactus.vote.count._
import org.greatcactus.vote.count.ballots.ElectionData
import org.greatcactus.vote.count.weighted._

class ACTElectionHelper(data:ElectionData, candidatesToBeElected:Int, aecDeemedOrder:Seq[CandidateIndex], printDebugMessages:Boolean, ineligibleCandidates:Set[CandidateIndex]=Set.empty,override val voteMultipleToDealWithFractionalVotes : Tally = 1,emulateACT2020behaviour:Boolean=false)
  extends WeightedCountHelper(data,candidatesToBeElected,ticketRoundingChoices=Map.empty,aecDeemedOrder,printDebugMessages,ineligibleCandidates) {

  override def reportsShouldUseMajorCountNumber = false // not done in 2016
  /*
    * In 2020 Murrumbidge, Count 24. Ed Cocks is being eliminated. He has some votes with TV 1 distributed in count 23, and some with TV 0.374... distributed in count 24. There are 7 papers exhausted. So there are 7*TV=2.618... votes lost to exhaustion.
    * 2 go the the exhausted column, 0.618... to the loss to fraction column.
    */
  override def ignoreVoteMultipleToDealWithFractionalVotesForExhaustedVotesWhenCandidateExcluded : Boolean = emulateACT2020behaviour // done by ACT in 2020.
  override def roundToNearestInsteadOfDown:Boolean = emulateACT2020behaviour
  override def finishSurplusDistributionEvenIfEveryoneIsElected : Boolean = emulateACT2020behaviour       // interpretation used by Elections ACT in 2020
  override def roundTransferValuesTo6DigitsWhenOverridingCurrentTransferValue : Boolean = emulateACT2020behaviour // bug in Elections ACT 2020 code.
  override def shouldSeparateBallotsBySourceCountNumber : HowSplitByCountNumber = if (emulateACT2020behaviour) SplitByCountTVCameFrom else DoNotSplitByCountNumber // Elections ACT did what looks like a terrible bug in 2020. TODO make option
  override def finishExclusionEvenIfAllWillBeElected : Boolean = true
  override def finishSuplusDistributionEvenIfEveryoneWillGetElected : Boolean = true // 4.2, 4(2)(b)

  override def getCandidateToDistribute : CandidateIndex = getCandidateToDistributeOrderElected // 4.2, 7 (3)
  override def orderSurplusesBasedOnUniqueNumbersInCountback : Boolean = false // 4.2, 7 (3) (c)

  override def transferExcess(candidate: CandidateIndex): Unit = transferSurplusAllWithSameTransferValueLastParcel(candidate) // 4.1 and 4.2

  override def sortedForExclusion(votes:WeightedVotes): List[((TransferValue, CountNumber), PlainVotes)] = votes.sortedByWeightThenCountNumber // 4.2 9 (3) not split by count number anyway.
  override def finishExclusionEvenIfAllVacanciesFilled : Boolean = false

  override def candidatesForExclusion : CandidateToExclude = getCandidateToExcludeBasedOnLastBestCount // 4.2 8 (2)

  override def stopIfTwoContinuingCandidatesAndOneVacancy : Boolean = false
  /** In order for a candidate to have a surplus, the candidate must have a tally > the quota. But sometimes small surpluses are disregarded. Require tally>quota+amountNeededAboveQuotaToBeASurplus for it to be considered a surplus.
    * ACT legislation says surplus must be 1 or more. This is non-trivial as votes are computed to 6 decimal places.
    *
    * 4.1 1 : "surplus, in relation to a successful candidate, means the candidate’s
    * total votes less the quota, if the resulting number of votes is 1 or
    * greater."
    * */
  override def amountNeededAboveQuotaToBeASurplus : Tally = voteMultipleToDealWithFractionalVotes-1

  override def newWorker(newdata:ElectionData):WeightedCountHelper = new ACTElectionHelper(newdata,candidatesToBeElected,aecDeemedOrder,false,ineligibleCandidates,voteMultipleToDealWithFractionalVotes)

}