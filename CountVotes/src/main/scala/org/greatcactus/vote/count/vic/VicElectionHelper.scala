/*
    Copyright 2017-2019 Silicon Econometrics Pty. Ltd.

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


package org.greatcactus.vote.count.vic

import org.greatcactus.vote.count.MainDataTypes._
import org.greatcactus.vote.count._
import org.greatcactus.vote.count.ballots.ElectionData
import org.greatcactus.vote.count.weighted._

class VicElectionHelper(data:ElectionData, candidatesToBeElected:Int, ticketRoundingChoices:Map[String,Int], aecDeemedOrder:Seq[CandidateIndex], printDebugMessages:Boolean, ineligibleCandidates:Set[CandidateIndex]=Set.empty)
     extends WeightedCountHelper(data,candidatesToBeElected,ticketRoundingChoices:Map[String,Int],aecDeemedOrder,printDebugMessages,ineligibleCandidates) {
  
  override def reportsShouldUseMajorCountNumber = false
  
  override def shouldSeparateBallotsBySourceCountNumber : HowSplitByCountNumber = if (useVECinterpretationOf28c) OnlySplitByCountNumberIfOne else FullySplitByCountNumber
  override def finishExclusionEvenIfAllWillBeElected : Boolean = false
  override def finishSuplusDistributionEvenIfEveryoneWillGetElected : Boolean = false

  override def getCandidateToDistribute : CandidateIndex = getCandidateToDistributeOrderElected
  override def orderSurplusesBasedOnUniqueNumbersInCountback : Boolean = true

  override def transferExcess(candidate: CandidateIndex): Unit = transferSurplusAllWithSameTransferValue(candidate)


  /*
  There is problem for the interpretation of the legislation. In particular, in Schedule 114A, 28(c) :
    [ For the purposes of this section each of the following constitutes a separate transfer ]
      a transfer in accordance with subsection (12)(b) of all the votes of an excluded candidate that were transferred to that candidate from a particular candidate.
   Whereas what the VEC did in 2014 was the (actually marginally more reasonable) merge of all votes from any candidate that had the same transfer value. Apart from count 1, which is utterly unambiguously separated in 28(b)
   I have replicated the VEC logic.
   */

  def useVECinterpretationOf28c : Boolean = true

  def forceUselessFirstPreferenceDistributionIfNonePresent = true
  override def sortedForExclusion(votes:WeightedVotes): List[((TransferValue, CountNumber), PlainVotes)] = {
    val expected = votes.sortedByWeightThenCountNumber
    if (forceUselessFirstPreferenceDistributionIfNonePresent && (expected.isEmpty || expected.head._1._2 != 1) )
        ((1.0,1),new PlainVotes)::expected
    else expected
  }
  override def finishExclusionEvenIfAllVacanciesFilled : Boolean = false
 
  override def candidatesForExclusion : CandidateToExclude = getCandidateToExcludeWithExplicitCountbackRequiringAllDifferent
  

  
}