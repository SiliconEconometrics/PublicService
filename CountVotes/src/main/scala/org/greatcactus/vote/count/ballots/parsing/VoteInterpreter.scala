/*
    Copyright 2015-2019 Silicon Econometrics Pty. Ltd.

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

package org.greatcactus.vote.count.ballots.parsing

import org.greatcactus.vote.count.MainDataTypes.{CandidateIndex, GroupIndex, NumberOfCandidates, PaperCountUnscaled, PreferenceNumber}
import org.greatcactus.vote.count.ballots._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
  * Somewhat aging helper to interpret the EC's files of votes. Used to create ElectionData.
  */
class VoteInterpreter(groups:Array[GroupInformation], val numCandidates:NumberOfCandidates) {
    val btl = new scala.collection.mutable.HashMap[Long,BelowTheLineBuilder]
    val atl = new scala.collection.mutable.HashMap[Int,AboveTheLineBuilder]
    def numGroups : Int = groups.length
    val satlCounts : Array[PaperCountUnscaled] = new Array[PaperCountUnscaled](numGroups)
    val extraBTLs = new ArrayBuffer[BTL]
    val extraATLs = new ArrayBuffer[String]
    val groupLookup = Map(groups.map{_.groupId}.zipWithIndex : _*)
    var officialResults : Option[Array[CandidateIndex]] = None
    var numInformal : PaperCountUnscaled = 0

    def addInformal(n:PaperCountUnscaled): Unit = { numInformal+=n }
    def addBTL(btl:BTL): Unit = { extraBTLs+=btl }
    def addBTL(ballotID:Long,candidate:Candidate,preferenceNumber:PreferenceNumber): Unit = {
              val v = btl.getOrElseUpdate(ballotID,new BelowTheLineBuilder(ballotID,numCandidates))
              v.addVote(candidate,preferenceNumber)
    }
    def addRATL(spaceSepVotes:String): Unit = { extraATLs+=spaceSepVotes }
    def addRATL(ballotID:Int,groupCode:String,preferenceNumber:PreferenceNumber): Unit = {
              val v = atl.getOrElseUpdate(ballotID,new AboveTheLineBuilder(numGroups))
              v.addVote(groupCode,preferenceNumber)
    }
    def addSATL(groupIndex:GroupIndex,n:PaperCountUnscaled=1): Unit = {
      satlCounts(groupIndex)+=n
    }
    def addSATL(group:String): Unit = {
      addSATL(groupLookup(group))
    }
    def getData(orderedCandidates:Array[Candidate],name:ElectionName,downloadLocations:Array[String]) : ElectionData = {
      //println("SATLs : "+satlCounts.sum)
      //println("RATLs : "+atl.size)
      //println("BTLs : "+btl.size)
      //println("candidates : "+orderedCandidates.length)

      val candidateToIndex : Map[Candidate,CandidateIndex]=Map.empty++orderedCandidates.zipWithIndex
      val satls = for ((count,index)<-satlCounts.zipWithIndex) yield new SATL(groups(index).groupId,count)
      val canonatls = {
        val strings = (for (v<-atl.values) yield v.get).toList++extraATLs
        val bunched = for ((s,l)<-strings.groupBy { a => a }) yield new ATL(s.split(' '),l.length)
        bunched.toArray
      }
      val btls = (for (v<-btl.values) yield v.get(candidateToIndex)).toArray++extraBTLs
      val cleanBtls = BTLCandidateList.findMultiples(btls)
      new ElectionData(new ElectionMetadata(name,orderedCandidates,groups,officialResults,downloadLocations),satls,canonatls,cleanBtls,numInformal)
    }
}




class AboveTheLineVote(val ballotID:Int,/** groups voted for, in order */val groups:List[GroupIndex])

/** Helper to build vote lists, taking duplicates into account (votes below and including duplicate numbers are ignored */
class GeneralVoteBuilder[T <: AnyRef : ClassTag ](numPossibilities:Int) {
  val preferences : Array[T] = new Array[T](numPossibilities) // preferences(0) is who is given the first preference, 1 second, etc.
  var duplicate: Int = Integer.MAX_VALUE // the index of the smallest duplicated preference

  /** record a (1 based) vote preferenceNumberGiven cast for whofor */
  def addVote(whofor:T,preferenceNumberGiven:PreferenceNumber) {
    if (preferenceNumberGiven>0 && preferenceNumberGiven<=numPossibilities) {
      if (preferences(preferenceNumberGiven-1)!=null) {
        //println("Found duplicate preference "+preferenceNumberGiven+" for "+whofor+" and "+preferences(preferenceNumberGiven-1))
        duplicate = duplicate min (preferenceNumberGiven-1)
      }
      preferences(preferenceNumberGiven-1)=whofor
    } else println("Found bogus preference "+preferenceNumberGiven+" for "+whofor)
  }

  /** Get the first consecutive set of preferences not containing duplicates */
  def unduplicated : Array[T] = {
    val nonnull = preferences.takeWhile(_ !=null)
    if (nonnull.length>duplicate) nonnull.slice(0,duplicate) else nonnull
  }
}

class AboveTheLineBuilder(numGroups:Int) extends GeneralVoteBuilder[String](numGroups) {
  def get : String = unduplicated.mkString(" ")
}

class BelowTheLineBuilder(val ballotID:Long,numCandidates:NumberOfCandidates) extends GeneralVoteBuilder[Candidate](numCandidates) {
  def get(candidateIndex: Candidate=>CandidateIndex) : BTL = new BTL(unduplicated.map(candidateIndex),1)
}

class BelowTheLineBuilderByCandidateID(numCandidates:NumberOfCandidates) extends GeneralVoteBuilder[Integer](numCandidates) {
  def get : BTL = new BTL(unduplicated.map{_.toInt},1)
}

object BelowTheLineBuilderByCandidateID {
  def ofPreferencesInCandidateOrderNotAssumingFormality(prefs:Array[String],numCandidates:NumberOfCandidates) : BTL = {
    val builder = new BelowTheLineBuilderByCandidateID(numCandidates)
    for (who<-prefs.indices) {
      val written = prefs(who)
      if (written.nonEmpty) {
        try {
          builder.addVote(who,written.toInt)
        } catch { case _:NumberFormatException => }
      }
    }
    builder.get
  }

}

/** BTL without the numVoters. Used for normalization */
final class BTLCandidateList(/** candidate ids listed in preference order */ val candidates:Array[CandidateIndex]) {

  override def equals(other: Any): Boolean = other match {
    case that: BTLCandidateList => java.util.Arrays.equals(candidates,that.candidates)
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(candidates)
}

object BTLCandidateList {
  def findMultiples(possiblyRedundant:Array[BTL]) : Array[BTL] = {
    val map = new mutable.HashMap[BTLCandidateList,PaperCountUnscaled]()
    for (b<-possiblyRedundant) {
      val list = new BTLCandidateList(b.candidates)
      val existing = map.getOrElse(list,0:PaperCountUnscaled)
      map.put(list,b.numVoters+existing)
    }
    val res = for ((candidates,n)<-map) yield new BTL(candidates.candidates,n)
    res.toArray
  }
}