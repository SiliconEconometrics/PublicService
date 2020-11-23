/*
    Copyright 2015-2020 Silicon Econometrics Pty. Ltd.

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
 * Data structures appropriate for a weighted count (everywhere but NSW)
 */

package org.greatcactus.vote.count.weighted
import org.greatcactus.vote.count.MainDataTypes._
import org.greatcactus.vote.count.ballots.{ActualListOfTamperableVotes, DVote}

import scala.collection.mutable.ArrayBuffer
import scala.collection.GenTraversableOnce

/*
object WeightedDataStructures {
  
}*/

 /** A stack of votes that all have the same transfer value and would be physically in the same stack 
     for paper counting  (usually same prefs[upto] except in the first count and when batch eliminating).
   */
class PlainVotes {
  private val allVotes = new ArrayBuffer[DVote]
  private var countsOriginating : Set[CountNumber] = Set.empty   //which counts the vote came from
  private var numVotes : PaperCount = 0    // total pieces of paper
  private var numVotesATL : PaperCount = 0
  private var roundedTally : Tally = 0
  /** Split up votes by next preference, returning the split and the number of exhausted votes */
  def splitByNextContinuing(continuingCandidates : ContinuingCandidates) : (Array[PlainVotes],Tally) = {
    val res = Array.fill(continuingCandidates.numCandidates)(new PlainVotes)
    var numExhausted : Tally = 0
    //println("Splitting "+numVotes)
    for (v<-allVotes) {
      val next = v.skipNotContinuingCandidates(continuingCandidates.set)
      if (next.isExhausted) { 
        numExhausted+=next.numVoters.toTally
        //println("Exhausted "+v)
      } else res(next.current).add(next)
    }
    (res,numExhausted)
  }
   /** Remove votes that don't have a valid candidate to continue to. Also return the number of exhausted votes. */
   def removeExhausted(continuingCandidates : ContinuingCandidates) : (PlainVotes,Tally) = {
     val res = new PlainVotes
     var numExhausted : Tally = 0
     for (v<-allVotes) {
       val next = v.skipNotContinuingCandidates(continuingCandidates.set)
       if (next.isExhausted) {
         numExhausted+=next.numVoters.toTally
         //println("Exhausted "+v)
       } else res.add(next)
     }
     (res,numExhausted)
   }
  def add(v:DVote) {
    allVotes+=v
    numVotes+=v.numVoters.toPaperCount
    if (v.src.isATL) numVotesATL+=v.numVoters.toPaperCount
  }
  def add(votes:Array[DVote]) {
    for (v<-votes) add(v)
  }
  def numBallots : PaperCount = numVotes
  def numBallotsATL : PaperCount = numVotesATL
  def numBallotsBTL : PaperCount = numVotes-numVotesATL
  
  def add(moreVotes:PlainVotes,countIndex:CountNumber,roundedTally:Tally) {
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
  def getRoundedTally: Tally = roundedTally

  private def getTamperable(owningCandidatePositionInParty:NumberOfCandidates,isATL:Boolean,allow1Prefs:Boolean) : Array[DVote] = {
    allVotes.filter(v=>{
      val okATL = isATL == v.src.isATL
      val ok1Pref = allow1Prefs || !v.isStillOnFirstPreference(owningCandidatePositionInParty)
      okATL && ok1Pref
    }).to[Array]
  }
  /** Number that are hard to detect tampering with at this point - eg first preference, 1 ATL */
  def numTamperablePapers(owningCandidatePositionInParty:NumberOfCandidates,isATL:Boolean,allow1Prefs:Boolean) : PaperCount = {
    getTamperable(owningCandidatePositionInParty,isATL,allow1Prefs).map{_.numVoters.toPaperCount}.sum
  }
  def getActualListOfTamperableVotes(owningCandidatePositionInParty:NumberOfCandidates,isATL:Boolean,allow1Prefs:Boolean) : ActualListOfTamperableVotes = {
    val okvotes = getTamperable(owningCandidatePositionInParty,isATL,allow1Prefs)
    new ActualListOfTamperableVotes(okvotes.map{_.src},Nil)
  }
  //def getActualListOfAllVotes(wantATL:Boolean) : ActualListOfTamperableVotes = new ActualListOfTamperableVotes(allVotes.map{_.src}.toArray.filter{_.isATL == wantATL},Nil)
  def papersLostToRounding(tv:TransferValue): Tally = Math.floor(tv.toDouble*numBallots).toTally-getRoundedTally
}
sealed abstract class HowSplitByCountNumber { def key(num:CountNumber,tvCreatedOnIfKnown:CountNumber) : CountNumber }
object DoNotSplitByCountNumber extends HowSplitByCountNumber  { override def key(num:CountNumber,tvCreatedOnIfKnown:CountNumber) : CountNumber = 0 }
object FullySplitByCountNumber extends HowSplitByCountNumber  { override def key(num:CountNumber,tvCreatedOnIfKnown:CountNumber) : CountNumber = num }
object OnlySplitByCountNumberIfOne extends HowSplitByCountNumber  { override def key(num:CountNumber,tvCreatedOnIfKnown:CountNumber) : CountNumber = if (num==1) 1 else 2 }
/** Split by the count the transfer value was created (or 1 for first pref count) */
object SplitByCountTVCameFrom extends HowSplitByCountNumber  { override def key(num:CountNumber,tvCreatedOnIfKnown:CountNumber) : CountNumber = tvCreatedOnIfKnown }


class LastParcel(val moreVotes:PlainVotes,val transferValue:TransferValue,val countIndex:CountNumber,val roundedTally:Tally)

class WeightedVotes(val splitByCountNumber:HowSplitByCountNumber) {
  private var lastParcel : Option[LastParcel] = None
  def getLastParcel : LastParcel = lastParcel.get
  private val map = new collection.mutable.HashMap[(TransferValue,CountNumber),PlainVotes]
  def asPlain : PlainVotes = {
    val res = new PlainVotes
    for (v<-map.values) res.addWithoutChangingCountIndex(v)
    res
  }
  private def pv(transferValue:TransferValue,countIndex:CountNumber,tvCreatedOnIfKnown:CountNumber) : PlainVotes = map.getOrElseUpdate((transferValue,splitByCountNumber.key(countIndex,tvCreatedOnIfKnown)),new PlainVotes)
  def add(moreVotes:PlainVotes,transferValue:TransferValue,countIndex:CountNumber,tvCreatedOnIfKnown:CountNumber,roundedTally:Tally) {
    if (moreVotes.numBallots>0) pv(transferValue,countIndex,tvCreatedOnIfKnown).add(moreVotes,countIndex,roundedTally)
    lastParcel = Some(new LastParcel(moreVotes,transferValue,countIndex,roundedTally))
  }
  def numBallots : PaperCount = map.values.toList.map{_.numBallots}.sum
  def numBallotsATL : PaperCount = map.values.toList.map{_.numBallotsATL}.sum
  def computeToDoublePrecision : Double = (for (((weight,_),votes)<-map) yield weight.toDouble*votes.numBallots).sum
  def roundedTally : Tally = map.values.map{_.getRoundedTally}.sum
  def lostToRounding: Double = computeToDoublePrecision-roundedTally
  def + (other:WeightedVotes) : WeightedVotes = {
    var res = new WeightedVotes(splitByCountNumber)
    res+=this
    res+=other
    res
  }
  def += (other:WeightedVotes): Unit = {
    for (((weight,countInd),votes)<-other.map) pv(weight,countInd,countInd).addWithoutChangingCountIndex(votes)
  }
  def sortedByCountNumber : List[((TransferValue,CountNumber),PlainVotes)]  = map.toList.sortBy{_._1._2}
  def sortedByWeight : List[((TransferValue,CountNumber),PlainVotes)] = map.toList.sortBy{ _._1._1}.reverse
  def sortedByWeightThenCountNumber : List[((TransferValue,CountNumber),PlainVotes)] = sortedByCountNumber.reverse.sortBy{_._1._1}.reverse
  def removeTransferValue(tv:TransferValue) {
    val toremove = map.keys.filter{_._1==tv}
    map--=toremove
  }
  def removeTransferValueAndCountNumber(tv:TransferValue,countIndex:CountNumber) { 
    map-= ((tv,countIndex))
  }
  def clear() { map.clear() }

  def getTamperableVotes(owningCandidatePositionInParty:NumberOfCandidates,wantActualVotes:Boolean,allowATL:Boolean,allow1Prefs:Boolean,scaleNumbers:Tally=1) : List[TamperableVotes] = {
    def get(atl:Boolean) : List[TamperableVotes] = (for (((tv,_),votes)<-map) yield
      new TamperableVotes(tv.toDouble,votes.numTamperablePapers(owningCandidatePositionInParty,atl,allow1Prefs)/scaleNumbers,if (wantActualVotes) Some(votes.getActualListOfTamperableVotes(owningCandidatePositionInParty,atl,allow1Prefs)) else None,votes.papersLostToRounding(tv),atl,allow1Prefs)).toList
    val btl = get(false)
    val unsorted = if (allowATL) btl++get(true) else btl
    unsorted.filter{_.papers>0}.sortBy { - _.tv }
  }
}


class TamperableVotes(val tv:Double,val papers:PaperCountUnscaled,val src:Option[ActualListOfTamperableVotes],val maxPapersLostToRounding:TallyUnscaled,val isAboveTheLine:Boolean,val requiresAlteringFirstPreference:Boolean) {
  val votes:Tally = (Math.floor(tv*papers).toTally-maxPapersLostToRounding) max 0
  def scaleVotesBackTo(wantedVotes:Tally): TamperableVotes = {
    val newPapers = Math.ceil(wantedVotes/tv).toTally
    new TamperableVotes(tv,newPapers,src.map{_.split(newPapers)._1},maxPapersLostToRounding,isAboveTheLine,requiresAlteringFirstPreference)
  }
  override def toString: String = papers.toString+"->"+votes
}


/** aecDeemedPreferences is the order in which the AEC should resolve ties... first means going higher in orders. Only needs to contain actually used ones. */
class ContinuingCandidates(aecDeemedOrder:Seq[CandidateIndex],val numCandidates:NumberOfCandidates) {
  /** Ordered highest to lowest */
  var orderedList : List[CandidateIndex] = aecDeemedOrder.toList++((0 until numCandidates).toSet--aecDeemedOrder)
  var set : Set[CandidateIndex] = orderedList.toSet
  
  var aecDeemedOrderMatters : List[Set[CandidateIndex]] = List((0 until numCandidates).toSet)
  /** If candidate c is ambiguous order, return all that are ambiguous wrt it */
  def couldAECHaveToMakeDecision(c:CandidateIndex) : Option[Set[CandidateIndex]] = aecDeemedOrderMatters.find { _.contains(c) }
  
  def -=(candidate:CandidateIndex) {
    set-=candidate
    orderedList=orderedList.filter{_ != candidate}
    aecDeemedOrderMatters=aecDeemedOrderMatters.map{_.filter(_ != candidate)}.filter { _.size> 1 }
  }
  def --=(candidates:GenTraversableOnce[CandidateIndex]) {
    set= set--candidates
    val removeset = candidates.toSet
    orderedList=orderedList.filter{!removeset.contains(_)}
    aecDeemedOrderMatters=aecDeemedOrderMatters.map{_.filter(!removeset.contains(_))}.filter { _.size> 1 }
  }
  /** Reorder orderedList, with ties broken by current order */
  def reorder(tallys:Array[Tally]) {
    orderedList = orderedList.sortBy { -tallys(_) } // relies on it being a stable sort to break ties correctly.
    // deal with aecDeemedOrderMatters
    aecDeemedOrderMatters=aecDeemedOrderMatters.flatMap{s=>
      s.groupBy { tallys(_)  }.values.filter { _.size> 1 }
    }
  }
  def top(n:NumberOfCandidates) : List[CandidateIndex] = orderedList.take(n)
  def head:CandidateIndex = orderedList.head
  /** Get this candidate and ones lower in the list */
  def candidateAndLower(candidate:CandidateIndex) : List[CandidateIndex] = orderedList.dropWhile { _ != candidate }
  def length: Tally = orderedList.length
}

