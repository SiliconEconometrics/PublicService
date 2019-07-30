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

package org.greatcactus.vote.count.ballots

import java.io.IOException

import org.greatcactus.vote.count.MainDataTypes.CandidateIndex
import org.greatcactus.vote.count.ballots.ElectionDataFastIO.parseCommaSeparatedIntegers
import org.greatcactus.vote.count.ballots.GroupInformation.{GroupID, GroupIndex}


/** Information about the election itself... enough to generate a picture of a ballot paper */

sealed class ElectionMetadata(
                            val electionName : ElectionName,
                            val candidates : Array[Candidate],
                            val groupInfo : Array[GroupInformation],
                             /** If present, a list of candidate ids for the official winners, in order of election */
                            val officialResults : Option[Array[Int]],
                             /** Description of where to download stuff */
                            val downloadLocation : Array[String]
                          ) {
                            val groupFromID : Map[GroupID,GroupInformation] = Map.empty++groupInfo.map{g=>g.groupId->g}
                            val groupIndexFromID : Map[GroupID,GroupIndex] = Map.empty++(for (i<-groupInfo.indices) yield groupInfo(i).groupId->i)
                            lazy val groupNameFromID : Map[GroupID,String] = Map.empty++groupInfo.map{g=>g.groupId->g.groupName}+(""->"Ungrouped")
                            lazy val candidateIndexFromName : Map[String,CandidateIndex] = Map.empty++candidates.map{_.name}.zipWithIndex
                            lazy val groupIndexFromCandidateIndex : Array[GroupIndex] = candidates.map{c=>groupIndexFromID(c.group)}
                            lazy val numCandidatesInGroup : Array[Int] = groupInfo.map{gi=>candidates.count(_.group==gi.groupId)}

                            val numCandidates : Int = candidates.length
                            def numGroups : Int = groupInfo.length

                            /** A similar election, but with the candidates indices permuted. Used for testing that it produces the same answers. */
                            def reverseCandidateOrder : ElectionMetadata = new ElectionMetadata(electionName,candidates.reverse,groupInfo,officialResults.map{_.map{numCandidates-1-_}},downloadLocation)

                            def prettyGroupName(groupID:GroupID) : String = groupNameFromID.getOrElse(groupID,groupID)
                            def candidateNameWithGroup(c:CandidateIndex) : String = candidates(c).name+" ("+prettyGroupName(candidates(c).group)+")"
                          }

sealed class ElectionName(val year:String,
                          /** Who runs it, e.g. AEC */ val runningAuthority:String,
                          /** Overall election name, e.g Federal */  val overallElectionName:String,
                          /** Region within that, e.g. NSW */ val electorate:String,
                          val modifications:Array[String]=Array()) {
  def line : String = year+"\t"+runningAuthority+"\t"+overallElectionName+"\t"+electorate

  override def toString : String = year+" "+overallElectionName+" : "+electorate+modifications.map{" ("+_+")"}.mkString("")

  def withModification(modification:String) : ElectionName = new ElectionName(year,runningAuthority,overallElectionName,electorate,modifications:+modification)

  /** Suitable name for a file comprising electorate plus modifications */
  def shortFileName : String = (electorate+:modifications).mkString("_")
  def shortPrintName : String = (electorate+:modifications).mkString(" ")

  def longFileName = overallElectionName+" "+year+" "+shortFileName
}

object ElectionName {
  /** Opposite of line */
  def ofLine(s:String) : ElectionName = {
    val ss = s.split('\t')
    if (ss.length<4) throw new IOException("Too few fields for election name "+s)
    new ElectionName(ss(0),ss(1),ss(2),ss(3),ss.drop(4))
  }
}

sealed class Candidate(val name:String,/* column abreviation eg A or UG */ val group:GroupID,/* starting from 1 */ val position:Int) extends Dumpable {
  def line : String = name+"\t"+group+"\t"+position
}

object Candidate {
  /** Opposite of line */
  def ofLine(s:String) : Candidate = {
    val ss = s.split('\t')
    if (ss.length!=3) throw new IOException("Wrong number of fields for candidate "+s)
    new Candidate(ss(0),ss(1),ss(2).toInt)
  }
}


class GroupInformation(
                        val groupId:String,
                        val groupName:String,
                        val shortName:Option[String],
                        val tickets : Array[Array[Int]] // outer array is ticket number, inner array is preferences list (actual value is a 0 based candidate number)
                      ) extends Dumpable {
  def line : String = groupId+"\t"+groupName+"\t"+shortName.getOrElse("")+"\t"+tickets.map{_.mkString(",")}.mkString("\t")
}

object GroupInformation {
  /** A short, unique identifier for the group a candidate is in. Typically the column A...Z, AA... */
  type GroupID = String
  /** Integer from 0 to numGroups-1 indicating the position in the groupInfo array. */
  type GroupIndex = Int
  /** Opposite of line */
  def ofLine(s:String) : GroupInformation = {
    val ss = s.split('\t')
    new GroupInformation(ss(0),if (ss.length==1) "" else ss(1),if (ss.length<3 || ss(2).isEmpty) None else Some(ss(2)),ss.drop(3).map(parseCommaSeparatedIntegers))
  }

}