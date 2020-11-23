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

/*
 * I have very little confidence in this code so far as it has been barely tested for sanity,
 * not against any actual results, and it has come from modifying the AEC code, so I could
 * easily have thought that the WAEC and AEC do things the same way.
 * 
 * TLDR: Don't trust it.
 */
package org.greatcactus.vote.count.wa

import org.greatcactus.vote.count._
import org.greatcactus.vote.count.weighted._
import org.greatcactus.vote.count.MainDataTypes._
import org.greatcactus.vote.count.ballots.ElectionData

class WAElectionHelper (data:ElectionData,candidatesToBeElected:Int,ticketRoundingChoices:Map[String,Int],aecDeemedOrder:Seq[CandidateIndex],printDebugMessages:Boolean,ineligibleCandidates:Set[CandidateIndex]=Set.empty)
     extends WeightedCountHelper(data,candidatesToBeElected,ticketRoundingChoices:Map[String,Int],aecDeemedOrder,printDebugMessages,ineligibleCandidates) {
  
  override def reportsShouldUseMajorCountNumber = true
  
  override def shouldSeparateBallotsBySourceCountNumber : HowSplitByCountNumber = FullySplitByCountNumber
  override def finishExclusionEvenIfAllWillBeElected : Boolean = false
  override def finishSuplusDistributionEvenIfEveryoneWillGetElected : Boolean = false
  /*
   * There is a bug in clause 15: 
   * Consider candidates A,B,C and their tallies, with a quota of 100
   * 
   * Count A   B   C
   * 1     3   2   1
   * 2     50  2   50
   * 3     101 101 101
   * 
   * According to (15), after count 3, they should be distributed in the order A B C, as turn 1 is ehere each has a different number of votes.
   * However, after count 4, clause 15 may come into play again (with A distributed and not counted), C should be done before B, because of count 2 they have different surpluses.
   * There are several ways to resolve this:
   *   (a) Rehold election if this occurs
   *   (b) do order A B C and say the first invocation has precedence
   *   (c) do order A C B and say the second invocation has precedence
   *   (d) interpret it as meaning a consistent ordering, whereby they don't have to all be different, but as soon as one is different in the countback it is dealt with.
   * I am implementing (b) somewhat arbitrarily. To get option (d), set orderSurplusesBasedOnUniqueNumbersInCountback to false.
   */
  override def getCandidateToDistribute : CandidateIndex = getCandidateToDistributeOrderElected
  override def orderSurplusesBasedOnUniqueNumbersInCountback : Boolean = true

  // Step (5) If not finished, transfer surplus of elected candidates.
  override def transferExcess(candidate:CandidateIndex) {
    println("Transfer excess for candidate "+candidate)
  // 5a compute transfer value
//    val rawvotes : PlainVotes = ballots(candidate).asPlain
    val surplus = tallys(candidate)-quota
    val transferValueMultiple : TransferValue = MainDataTypes.transferValue(surplus,tallys(candidate))
    val transferValueMultipleDescription = new TransferValueComputationSingleValue(surplus,tallys(candidate),0,transferValueMultiple.toDouble)
    // 5c distribute
    var cumExhausted : Tally = 0
    var needToEndCount = false
    var cumVotesTrans : Tally = 0
    for (((tv,fromCount),plain)<-ballots(candidate).sortedByCountNumber) { // split up by old transfer value, 5c(ii), [ and also by from count number???? See big comment above excludeCandidate ]
      if (needToEndCount) {    
        report.finishCount()
        if (report.history.length!=currentCountNumber) throw new IllegalArgumentException("Report history length = "+report.history.length+" currentCountNumber="+currentCountNumber)
        currentCountNumber+=1
        needToEndCount=false
      }
      val transferValue = tv * transferValueMultiple
      val transferValueDescription = new TransferValueDescriptionMultipleComputation(tv.toDouble,transferValueMultipleDescription,transferValue.toDouble)
      val (distributedToCandidate : Array[PlainVotes],numExhausted:Tally) = plain.splitByNextContinuing(continuingCandidates)
      val exhaustedVotes = roundDownRecordRounding(transferValue,numExhausted)
      cumExhausted+=exhaustedVotes
      report.addExhaustedVotes(exhaustedVotes)
      report.addExhaustedPapers(numExhausted)
      if (transferValue.numerator>0) { // (18)
        val numVotesTrans = roundDownRecordRoundingReverseDirection(transferValue,plain.numBallots)
        report.declareCandidateDistributed(candidate,numVotesTrans,plain.numBallots, transferValueDescription,Nil,plain.numBallots, 0, exhaustedVotes,distributedMakesSense = false)
        report.fromCountReference(fromCount)
        for (nextChoice<-continuingCandidates.orderedList) { // 5c(i)
          val giveVotes : PlainVotes = distributedToCandidate(nextChoice)
          val tally = roundDownRecordRounding(transferValue,giveVotes.numBallots)
          ballots(nextChoice).add(giveVotes, transferValue, currentCountNumber,0,tally)
          tallys(nextChoice)+=tally
        }
        clearRoundingPending()
        needToEndCount=true
        tallys(candidate)-=numVotesTrans
        cumVotesTrans+=numVotesTrans
      }
      ballots(candidate).removeTransferValueAndCountNumber(tv,fromCount)
    }
    if (!needToEndCount) report.declareCandidateDistributed(candidate, 0, 0, transferValueMultipleDescription,Nil,  ballots(candidate).numBallots, 0, cumExhausted,distributedMakesSense = false)
    ballots(candidate).clear()
    val extraLostVotes = tallys(candidate)-quota
    tallys(candidate)=quota
    report.addExtraLostDueToRounding(extraLostVotes) // The votes that the candidate being distributed "has" is irrelevant, not covered by legislation, but it is still helpful to make something up to balance the books. Legislation does not cover rounding here; I include all rounding compensation in the last transfer.
    endCountReorderAndCheckElected(false)
    report.finishMajorCount()
  }

  /*
  There is problem for the interpretation of the legislation. In particular, in Schedule 1, section 8(b). I think it has a bug.
  Suppose a candidate E got excluded. During a surplus redistribution, suppose E got votes (with 2 different transfer values, but it is still one transfer by clause 19). 
  Then applying 8(b)(i) is difficult, as there are multiple transfer values.
  A reasonable way to interpret it, I would guess, is that you treat them separately, and then add up the results. Rounding before or after is not at all clear (a minor point).  
  Another interpretation is that these are separate transfers (somewhat countered by clause 19), which could have a big effect if it splits up when someone could get elected (clause 9) and cease being a continuing candidate for the rest of the elimination.
  
  Looking at what the WAEC did in the 2013 election, they are treated as separate transfers. In particular, in the Agri region, during the exclusion of "HULL, Ray", there
  were two votes that came in with transfer value 1 from the exclusion of Steven Fuhrman (12.1 and 12.3). They were treated as separate counts (17.3 and 17.4). This is
  even stronger than my second interpretation, and (to me) contradicts clause 19 saying 
    "a transfer in accordance with clause 8(b) of all the votes of an excluded candidate that were transferred to him from a particular candidate"
  is one of the things that 
    "constitutes a separate transfer"
    
  This seems wrong to me, but I am not a lawyer, and it does the big advantages of being simple and self consistent, even if not consistent with the legislation (which seems to be buggy on this point anyway). If you just interpret "from a particular candidate" as "in a particular count" it is self consistent.
  So, even though it seems wrong, that's what I have implemented.   I have also interpreted this to mean that the same thing applies to the surplus transfer. But I could be wrong in that - I didn't find evidence either way (I didn't look that hard).

   */
  
  def forceUselessFirstPreferenceDistributionIfNonePresent = true
  override def sortedForExclusion(votes:WeightedVotes): List[((TransferValue, CountNumber), PlainVotes)] = {
    val expected = votes.sortedByCountNumber // will do highest first; (a) is a special case of (b)
    if (forceUselessFirstPreferenceDistributionIfNonePresent && (expected.isEmpty || expected.head._1._2 != 1) )
        ((TransferValueOne,1),new PlainVotes)::expected
    else expected
  }
  override def finishExclusionEvenIfAllVacanciesFilled : Boolean = false // legislation seems to say this should be true, but WAEC doesn't do it, and it can never change results, so no reason to be upset.


  override def candidatesForExclusion : CandidateToExclude = getCandidateToExcludeWithExplicitCountbackRequiringAllDifferent
  

  
}