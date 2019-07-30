/*
    Copyright 2015-2017 Silicon Econometrics Pty. Ltd.

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
        numExhausted+=next.numVoters.toInt
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
  def numBallotsBTL : Int = numVotes-numVotesATL
  
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
  def getRoundedTally: Tally = roundedTally

  private def getTamperable(owningCandidatePositionInParty:Int,isATL:Boolean,allow1Prefs:Boolean) : Array[DVote] = {
    allVotes.filter(v=>{
      val okATL = isATL == v.src.isATL
      val ok1Pref = allow1Prefs || !v.isStillOnFirstPreference(owningCandidatePositionInParty)
      okATL && ok1Pref
    }).to[Array]
  }
  /** Number that are hard to detect tampering with at this point - eg first preference, 1 ATL */
  def numTamperablePapers(owningCandidatePositionInParty:Int,isATL:Boolean,allow1Prefs:Boolean) : Int = {
    getTamperable(owningCandidatePositionInParty,isATL,allow1Prefs).map{_.numVoters.toInt}.sum
  }
  def getActualListOfTamperableVotes(owningCandidatePositionInParty:Int,isATL:Boolean,allow1Prefs:Boolean) : ActualListOfTamperableVotes = {
    val okvotes = getTamperable(owningCandidatePositionInParty,isATL,allow1Prefs)
    new ActualListOfTamperableVotes(okvotes.map{_.src},Nil)
  }
  //def getActualListOfAllVotes(wantATL:Boolean) : ActualListOfTamperableVotes = new ActualListOfTamperableVotes(allVotes.map{_.src}.toArray.filter{_.isATL == wantATL},Nil)
  def papersLostToRounding(tv:TransferValue): Int = Math.floor(tv*numBallots).toInt-getRoundedTally
}
sealed abstract class HowSplitByCountNumber { def key(num:CountNumber) : Int }
object DoNotSplitByCountNumber extends HowSplitByCountNumber  { override def key(num:CountNumber) : Int = 0 }
object FullySplitByCountNumber extends HowSplitByCountNumber  { override def key(num:CountNumber) : Int = num }
object OnlySplitByCountNumberIfOne extends HowSplitByCountNumber  { override def key(num:CountNumber) : Int = if (num==1) 1 else 2 }

class WeightedVotes(val splitByCountNumber:HowSplitByCountNumber) {
  private val map = new collection.mutable.HashMap[(TransferValue,CountNumber),PlainVotes]
  def asPlain : PlainVotes = {
    val res = new PlainVotes
    for (v<-map.values) res.addWithoutChangingCountIndex(v)
    res
  }
  private def pv(transferValue:TransferValue,countIndex:CountNumber) : PlainVotes = map.getOrElseUpdate((transferValue,splitByCountNumber.key(countIndex)),new PlainVotes)
  def add(moreVotes:PlainVotes,transferValue:TransferValue,countIndex:CountNumber,roundedTally:Int) {
    if (moreVotes.numBallots>0) pv(transferValue,countIndex).add(moreVotes,countIndex,roundedTally)
  }
  def numBallots : Int = map.values.toList.map{_.numBallots}.sum
  def numBallotsATL : Int = map.values.toList.map{_.numBallotsATL}.sum
  def computeExactTally : Double = (for (((weight,_),votes)<-map) yield weight*votes.numBallots).sum
  def roundedTally : Tally = map.values.map{_.getRoundedTally}.sum
  def lostToRounding: TransferValue = computeExactTally-roundedTally
  def + (other:WeightedVotes) : WeightedVotes = {
    var res = new WeightedVotes(splitByCountNumber)
    res+=this
    res+=other
    res
  }
  def += (other:WeightedVotes): Unit = {
    for (((weight,countInd),votes)<-other.map) pv(weight,countInd).addWithoutChangingCountIndex(votes)
  }
  def sortedByCountNumber : List[((TransferValue,CountNumber),PlainVotes)]  = map.toList.sortBy{_._1._2}
  def sortedByWeight : List[((TransferValue,CountNumber),PlainVotes)] = map.toList.sortBy{- _._1._1}
  def sortedByWeightThenCountNumber : List[((TransferValue,CountNumber),PlainVotes)] = sortedByCountNumber.sortBy{- _._1._1}
  def removeTransferValue(tv:TransferValue) {
    val toremove = map.keys.filter{_._1==tv}
    map--=toremove
  }
  def removeTransferValueAndCountNumber(tv:TransferValue,countIndex:CountNumber) { 
    map-= ((tv,countIndex))
  }
  def clear() { map.clear() }
  /*
  /** Minimum number of papers one needs to generate a given number of votes. Use higher transfer values first. Doesn't perfectly deal with rounding. */
  def numPapersToGenerateVotes(wantedVotes:Tally,requireTamperable:Boolean,owningCandidatePositionInParty:Int) : Option[Int] = {
    var togo = wantedVotes
    var res = 0
    for ((tv,pv)<-sortedByWeight) {
        if (togo>0) {
          val available = if (requireTamperable) pv.numTamperablePapers(owningCandidatePositionInParty) else  pv.numBallots
          val used = ((togo/tv).ceil min available).toInt
          togo-=(used*tv).floor.toInt
          res+=used
        }
    }
    if (togo==0) Some(res) else None
  }*/
  def getTamperableVotes(owningCandidatePositionInParty:Int,wantActualVotes:Boolean,allowATL:Boolean,allow1Prefs:Boolean) : List[TamperableVotes] = {
    def get(atl:Boolean) : List[TamperableVotes] = (for (((tv,_),votes)<-map) yield
      new TamperableVotes(tv,votes.numTamperablePapers(owningCandidatePositionInParty,atl,allow1Prefs),if (wantActualVotes) Some(votes.getActualListOfTamperableVotes(owningCandidatePositionInParty,atl,allow1Prefs)) else None,votes.papersLostToRounding(tv),atl,allow1Prefs)).toList
    val btl = get(false)
    val unsorted = if (allowATL) btl++get(true) else btl
    unsorted.filter{_.papers>0}.sortBy { - _.tv }
  }
/*
  def getTamperableVotesConsideringEverythingTamperable(wantActualVotes:Boolean,allowATL:Boolean) : List[TamperableVotes] = {
    val unsorted = for (((tv,_),votes)<-map;atl<-List(false,true)) yield new TamperableVotes(tv,if (atl) votes.numBallotsATL else votes.numBallotsBTL,if (wantActualVotes) Some(votes.getActualListOfAllVotes(atl)) else None,votes.papersLostToRounding(tv),atl,true)
    unsorted.toList.sortBy { - _.tv }
  }*/
}


class TamperableVotes(val tv:Double,val papers:Int,val src:Option[ActualListOfTamperableVotes],val maxPapersLostToRounding:Int,val isAboveTheLine:Boolean,val requiresAlteringFirstPreference:Boolean) {
  val votes:Tally = (Math.floor(tv*papers).toInt-maxPapersLostToRounding) max 0
  def scaleVotesBackTo(wantedVotes:Int): TamperableVotes = {
    val newPapers = Math.ceil(wantedVotes/tv).toInt
    new TamperableVotes(tv,newPapers,src.map{_.split(newPapers)._1},maxPapersLostToRounding,isAboveTheLine,requiresAlteringFirstPreference)
  }
  override def toString: String = papers.toString+"->"+votes
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
    aecDeemedOrderMatters=aecDeemedOrderMatters.map{_.filter(_ != candidate)}.filter { _.size> 1 }
  }
  def --=(candidates:GenTraversableOnce[CandidateIndex]) {
    set= set--candidates
    val removeset = candidates.toSet
    orderedList=orderedList.filter{!removeset.contains(_)}
    aecDeemedOrderMatters=aecDeemedOrderMatters.map{_.filter(!removeset.contains(_))}.filter { _.size> 1 }
  }
  /** Reorder orderedList, with ties broken by current order */
  def reorder(tallys:Array[Int]) {
    orderedList = orderedList.sortBy { -tallys(_) } // relies on it being a stable sort to break ties correctly.
    // deal with aecDeemedOrderMatters
    aecDeemedOrderMatters=aecDeemedOrderMatters.flatMap{s=>
      s.groupBy { tallys(_)  }.values.filter { _.size> 1 }
    }
  }
  def top(n:Int) : List[CandidateIndex] = orderedList.take(n)
  def head:CandidateIndex = orderedList.head
  /** Get this candidate and ones lower in the list */
  def candidateAndLower(candidate:CandidateIndex) : List[CandidateIndex] = orderedList.dropWhile { _ != candidate }
  def length: Tally = orderedList.length
}

