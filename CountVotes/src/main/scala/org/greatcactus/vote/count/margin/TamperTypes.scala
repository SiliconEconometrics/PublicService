/*
    Copyright 2016-2019 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.margin

import org.greatcactus.vote.count.MainDataTypes.{CandidateIndex, Tally}
import org.greatcactus.vote.count.ballots.{ActualListOfTamperableVotes, Candidate, ElectionMetadata}
import org.greatcactus.vote.count.weighted.TamperableVotes

import scala.collection.mutable.ListBuffer

/*
 * This file contains many types used in Tampering.scala
 */


/**
  * A description of the effect of a changed election - different people got elected.
  */
case class ElectionChanged(originalElected:List[CandidateIndex],newElected:List[CandidateIndex]) {
  val newWinners : Set[Int] = newElected.toSet--originalElected
  val newLosers : Set[Int] = originalElected.toSet--newElected
  def descListWithGroups(meta:ElectionMetadata) : List[String] = newLosers.toList.map{"- "+meta.candidateNameWithGroup(_)}++newWinners.toList.map{"+ "+meta.candidateNameWithGroup(_)}
  def descList(candidates:Array[Candidate]) : List[String] = newLosers.toList.map{"- "+candidates(_).name}++newWinners.toList.map{"+ "+candidates(_).name}
  def desc(candidates:Array[Candidate]) : String = descList(candidates).mkString(" ")
}

/** A somewhat concrete description of some votes that were tampered with, with a common theme of who they are coming from and who to.
  * In practice, the total list of tampered votes may consist of multiple of these. */
class TamperedVote(val whoFrom:Int,val whoTo:Int,val numPapers:Int,val numVotes:Int,val src:Option[ActualListOfTamperableVotes],val isATL:Boolean,val hasFirstPrefChanges: Boolean) {
  if (numPapers<0) throw new IllegalArgumentException
  def desc(candidates:Array[Candidate]) : String = candidates(whoFrom).name+"\u2192"+candidates(whoTo).name+":"+numPapers+(if (numPapers!=numVotes) "("+numVotes+" votes)" else "")+" "+(if (isATL) "ATL" else "BTL") // \u2192 is right arrow
  override def toString: String = whoFrom+"\u2192"+whoTo+":"+votedesc // \u2192 is right arrow
  def votedesc : String = numPapers.toString+(if (numPapers!=numVotes) "("+numVotes+" votes)" else "")
}


class Margin(/* The vote counting step this refers to */val step:Int,/** The actual tampered votes */ val tamperings:Array[TamperedVote]) {
  def hasPreferencesTakenFrom(newLosers: Set[CandidateIndex]): Boolean = tamperings.exists(t=>newLosers(t.whoFrom))
  def hasPreferencesGivenTo(newWinners: Set[CandidateIndex]): Boolean = tamperings.exists(t=>newWinners(t.whoTo))

  var hasFirstPrefChanges: Boolean = tamperings.exists(_.hasFirstPrefChanges)
  def hasATL:Boolean = tamperings.exists(_.isATL)

  val votes: Int = tamperings.map { _.numVotes}.sum
  val papers: Int = tamperings.map {_.numPapers}.sum

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

/** A quick summary of the election outcome */
class NormalElectionOutcome(val winners:List[CandidateIndex],/** Candidates who did not win, but were present in the final round. Usually just one of these. */ val alsoInLastRound:List[CandidateIndex]) {
  def everyoneInLastRound : List[CandidateIndex] = winners++alsoInLastRound
}

/** The properties that a Margin instance has. This is in principal computable from Margin and the effect. When a margin is computed somehow, the appropriate type is determined, and that
  * is used for reporting. The current method seems rather messy - perhaps we should have a function to compute the margintype, (although we need to know the effect), and then
  * allow all 16 cross product of boolean properties.
  *
  * THIS IS OBSOLETE.
  * */
/*
object MarginTypes {
  class MarginType(val n:Int,val name:String,val filenamebase:String,val allowFirstPrefChanges:Boolean,val allowPreferencesTakenFromNewLoser:Boolean,val allowPreferencesGivenToNewWinner:Boolean,val allowAboveTheLine:Boolean) {
    def filename:String = filenamebase+".html"
  }

  val AllowAnything = new MarginType(0,"allowing any changes","marginsAllow1PrefsChanges",true,true,true,true)
  val No1Prefs = new MarginType(1,"BTL only, not letting first preferences change","marginsNo1Prefs",false,true,true,false)
  val NoFromLoser = new MarginType(2,"BTL only, no first preferences changed, and no preferences taken from new loser","marginsNoFromLoser",false,false,true,false)
  val NoFromLoserOrToWinner = new MarginType(3,"BTL only, no first preferences changed, and no preferences taken from new loser or given to new winner","marginsNoToWinner",false,false,false,false)
  val No1PrefsATL = new MarginType(4,"not letting first preferences change","marginsNo1PrefsATL",false,true,true,true)
  val NoFromLoserATL = new MarginType(5,"no first preferences changed, and no preferences taken from new loser","marginsNoFromLoserATL",false,false,true,true)
  val NoFromLoserOrToWinnerATL = new MarginType(6,"no first preferences changed, and no preferences taken from new loser or given to new winner","marginsNoToWinnerATL",false,false,false,true)

  val allTypes = List(AllowAnything,No1Prefs,NoFromLoser,NoFromLoserOrToWinner,No1PrefsATL,NoFromLoserATL,NoFromLoserOrToWinnerATL)
  val numTypes: Int = allTypes.length
}
*/

/** The properties that an actual found tampering has. A tampering is more subtle the fewer of these variables are true. */
sealed class MarginProperties(val hasFirstPrefChanges:Boolean,val hasATL:Boolean,val hasPreferencesTakenFromNewLoser:Boolean,val hasPreferencesGivenToNewWinner:Boolean) {
  private def score(b:Boolean,s:Int) : Int = if (b) s else 0
  val code: CandidateIndex = score(hasFirstPrefChanges,1)+score(hasATL,2)+score(hasPreferencesTakenFromNewLoser,4)+score(hasPreferencesGivenToNewWinner,8)

  override def hashCode(): CandidateIndex = code
  override def equals(obj: Any): Boolean = obj match { case p:MarginProperties => p.code == code ; case _ => false }

  private def ps(b:Boolean,s:String) : Option[String] = if (b) Some(s) else None
  def propertyList:List[String] = List(ps(hasFirstPrefChanges,"Modifies 1 Pref"),ps(hasATL,"Uses ATL"),ps(hasPreferencesTakenFromNewLoser,"Takes From Loser"),ps(hasPreferencesGivenToNewWinner,"Gives To Winner")).flatten

  private def ts(b:Boolean,s:String) : String = if (b) s else ""
  override def toString: String = ts(hasFirstPrefChanges,"Modifies1Pref")+ts(hasATL,"UsesATL")+ts(hasPreferencesTakenFromNewLoser,"HasFromLoser")+ts(hasPreferencesGivenToNewWinner,"HasToWinner")
}

object MarginProperties {
  /** Compute the properties of a margin */
  def apply(margin:Margin,change:ElectionChanged) : MarginProperties = {
    new MarginProperties(
          hasFirstPrefChanges = margin.hasFirstPrefChanges,
          hasATL = margin.hasATL,
          hasPreferencesTakenFromNewLoser = margin.hasPreferencesTakenFrom(change.newLosers),
          hasPreferencesGivenToNewWinner = margin.hasPreferencesGivenTo(change.newWinners)
    )
  }
  val numMarginTypes = 16
}

/** Record, for each margin type, for each possible election change, the closest margin so far.
  * (that is, the smallest number of papers changed).
  *
  * Note that there is little point storing a less subtle margin if a better more subtle margin is present,
  * but that is currently resolved later.
  */
class BestMarginsRecorder {
  var best : Map[(ElectionChanged,MarginProperties),Margin] = Map.empty
  def add(margin:Margin,delta:ElectionChanged) {
    if (margin.papers<=0) throw new IllegalArgumentException
    val properties = MarginProperties(margin,delta)
//    if (properties.hasFirstPrefChanges && properties.hasATL) throw new IllegalArgumentException("Both ATL and 1 pref changes mean you can do anything!")
    val key = (delta,properties)
    if (!best.get(key).exists(_.papers<margin.papers)) best+=key->margin
  }


}
case class CanGiveAwayVotes(from:CandidateIndex,votes:TamperableVotes)

class TransferResolution(val tamperedVotes:List[TamperedVote],val remaining:List[CanGiveAwayVotes]) {
  def toMargin(count:Int) = new Margin(count,tamperedVotes.toArray)
}


object TransferResolution {

  /** Given some votes that can be transferred, and some votes that are needed, work out which votes to actually
    * transfer. Assumes that surplusVotesAvailable is listed in order of preferred use, most preferred first.
    * Throws CannotFindTamperableVotesException if not possible. */
  private def getTransfers1(surplusVotesAvailable:List[CanGiveAwayVotes], neededVotes:List[(CandidateIndex,Tally)]) : TransferResolution = neededVotes match {
    case Nil => new TransferResolution(Nil,surplusVotesAvailable) // all done
    case (_,0)::nt => getTransfers1(surplusVotesAvailable,nt) // 0 votes wanted
    case (candidateNeedingVotes,numberVotesNeeded)::nt =>
      if (numberVotesNeeded<0) throw new IllegalArgumentException
      if (surplusVotesAvailable.isEmpty) throw new CannotFindTamperableVotesException
      val CanGiveAwayVotes(surplusWho,surplus) = surplusVotesAvailable.head
      val willGiveVotes = surplus.votes min numberVotesNeeded
      val willGivePapers = Math.ceil((willGiveVotes+surplus.maxPapersLostToRounding)/surplus.tv).toInt
      val (actualVotesWillGivePapers:List[ActualListOfTamperableVotes],remainingActualVotes:List[ActualListOfTamperableVotes]) = surplus.src.map{_.split(willGivePapers)}.unzip // really option, not list.
      val tampering = new TamperedVote(surplusWho,candidateNeedingVotes,willGivePapers,willGiveVotes,actualVotesWillGivePapers.headOption,surplus.isAboveTheLine,surplus.requiresAlteringFirstPreference)
      val remainingNeeded = numberVotesNeeded-willGiveVotes
      val newNeeded = if (remainingNeeded==0) nt else (candidateNeedingVotes,remainingNeeded)::nt
      val remainingAvailable = new TamperableVotes(surplus.tv,surplus.papers-willGivePapers,remainingActualVotes.headOption,surplus.maxPapersLostToRounding,surplus.isAboveTheLine,surplus.requiresAlteringFirstPreference)
      val newAvailable = if (remainingAvailable.votes>0) CanGiveAwayVotes(surplusWho,remainingAvailable)::surplusVotesAvailable.tail else surplusVotesAvailable.tail
      val rest = getTransfers1(newAvailable,newNeeded)
      if (tampering.numPapers>0) new TransferResolution(tampering::rest.tamperedVotes,rest.remaining) else rest
  }

  /** Given some votes that can be transferred, and some votes that are needed, work out which votes to actually transfer.
    * Makes sure that candidates who cannot receive ATL votes do not do so.
    * Gives preference to ballots with high transfer values, and (overridingly) ballots coming from a preferred candidate.
    * The reason for the overwhelming advantage for a preferred candidate is that the level everyone gets reduced to can get stuffed up.
    * Throws CannotFindTamperableVotesException if not possible. */

  def getTransfers(surplusVotesAvailable:List[CanGiveAwayVotes],neededVotes:List[(CandidateIndex,Tally)],candidatesWhoCanHAveTheirTallyIncreasedByATLVotes:Set[CandidateIndex],preferredCandidateToTransferAwayFrom:Set[CandidateIndex]) : TransferResolution =  {
    val (canHaveATL,requireBTL) = neededVotes.partition{p=>candidatesWhoCanHAveTheirTallyIncreasedByATLVotes(p._1)}
    val (availableATL,availableBTL) = surplusVotesAvailable.partition { _.votes.isAboveTheLine }
    def effectOfTransferringAVote(g:CanGiveAwayVotes) : Double = { // how good transferring a vote is. High transfer value means good, as does from a candidate wanting exclusion.
      val base = -g.votes.tv
      if (preferredCandidateToTransferAwayFrom(g.from)) -10 - base else base //make sure that the preferred candidate is always first.
    }
    // first do those that require BTLs with just BTLs
    val btls = getTransfers1(availableBTL,requireBTL) // I am pretty sure that sortBy(effectOfTransferringAVote) is not necessary as they are guaranteed to start in order.
    val atls = getTransfers1((btls.remaining++availableATL).sortBy(effectOfTransferringAVote),canHaveATL)
    //if (availableATL.length>=0) {
    // println("ATL Have available "+availableATL+" may use "+canHaveATL+" BTL have available "+availableBTL+" required by "+requireBTL)
    //}
    new TransferResolution(btls.tamperedVotes++atls.tamperedVotes,atls.remaining)
  }

  class CannotFindTamperableVotesException extends Exception

}