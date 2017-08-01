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
package org.greatcactus.vote.count.wa

import java.io.File
import scala.io.Source
import java.util.zip.ZipInputStream
import java.io.FileInputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import scala.util.Random
import java.io.InputStreamReader
import java.io.InputStream
import java.io.BufferedReader
import scala.collection.mutable.ArrayBuffer
import scala.xml.XML
import org.greatcactus.vote.count._

object WAElectionData {

  val xmlSourceDir = new File("""C:\Users\Andrew\git\waec-2017-result-feed\waec_media""")
  val namePrefix = "11 March 2017 State General Election - "
  
  def loadAllRaw() : List[WARegion] = {
    def getXML(name:String) = XML.loadFile(new File(xmlSourceDir,namePrefix+name+".xml"))
    val candidatesXML = getXML("CANDIDATE SETUP")
    val regions : Map[String, WARegionWork] = Map((
      for (region<- candidatesXML \\ "ElectionRegion") yield {
        val regionname = (region \ "@Name").text
        //println("Got region "+regionname)
        val enrolment = (region \ "@Enrolment").text.toInt
        val lc = region \ "LC"
        val vacancies = (lc \ "@Vacancies").text.toInt
        val groupsWithoutTickets = for (g<- lc \ "Group") yield new GroupInformation((g \ "@BallotPaperOrder").text,(g \ "@BallotPaperName").text,Some((g \ "@WAECAbbreviation").text),Array())
        val candidates = for (c<- lc \ "Candidate") yield new Candidate((c \ "@BallotPaperName").text,(c \ "@GroupBallotPaperOrder").text,(c \ "@BallotPaperOrder").text.toInt-1)
        //for (c<-candidates) println(c.name)
        regionname -> new WARegionWork(regionname,vacancies,enrolment,candidates.toArray,groupsWithoutTickets.toArray)
      }
    ).toList :_*)
    if (true) { // add tickets
      val ticketsXML = getXML("GROUP AND TICKET SETUP") 
      for (region<- ticketsXML \\ "ElectionRegion")  {
        val regionname = (region \ "@Name").text
        //println("Got pass2 region "+regionname)
        val work = regions(regionname)
        val candidateIndex = Map((for ((c,ind)<-work.candidates.zipWithIndex) yield c.name->ind).toList :_*)
        for (g<- region \ "LC" \ "Group") {
          val groupIndS = (g \ "@BallotPaperOrder").text
          val ticket = new Array[Int](work.candidates.length)
          for (tp<-g \ "TicketPreference") ticket((tp \ "@Preference").text.toInt-1) =  candidateIndex((tp \ "@CandidateBallotPaperName").text)
          //println("Found ticket for "+groupIndS+" length "+ticket.length+" : "+ticket)
          //println(g)
          if (ticket.length==0) throw new Exception
          work.addTicket(groupIndS, ticket)
        }
      }
    }
    if (true) { // add votes
      val votesXML = getXML("LC VERBOSE RESULTS")
      for (region<- votesXML \\ "ElectionRegion")  {
        val regionname = (region \ "@Name").text
        //println("Got pass3 region "+regionname)
        val work = regions(regionname)
        val district = region \ "LC" \ "DistrictVotes"
        val districtname = (district \ "@DistrictName").text
        if (districtname!=regionname) throw new IllegalArgumentException("District "+districtname+" region "+regionname)
        for (gv<-district \ "GroupVotes") {
          work.addSATL((gv \ "@GroupBallotPaperOrder").text, (gv \ "@TicketVotes").text.toInt)
        }
      }      
    }
    regions.values.map{_.toRegion}.toList
  }
  
  class WARegionWork(val name:String,val vacancies:Int,val enrolment:Int,val candidates:Array[Candidate],val groups:Array[GroupInformation]) {
    val helper = new VoteInterpreter(groups,candidates.length)
    def groupLookup(groupId:String) : Int = helper.groupLookup(groupId)
    def addSATL(groupId:String,n:Int) { helper.addSATL(groupLookup(groupId), n)}
    def addTicket(groupId:String,ticket:Array[Int]) {
      val groupInd = groupLookup(groupId)
      val oldg = groups(groupInd)
      groups(groupInd) = new GroupInformation(oldg.groupId,oldg.groupName,oldg.shortName,oldg.tickets:+(ticket.toArray))
      // println(groups(groupInd).line)
    }
    def toRegion = new WARegion(helper.getData(candidates, name,"2017"),vacancies,enrolment)
  }
  

}

class WARegion(val data:ElectionData,val vacancies:Int,val enrolment:Int)
