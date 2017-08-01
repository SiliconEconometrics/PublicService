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
package org.greatcactus.vote.count.nsw
import org.greatcactus.vote.count._

import java.io.File
import scala.io.Source
import java.util.zip.ZipInputStream
import java.io.FileInputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import scala.util.Random
import org.jsoup.nodes.Document

object NSWLocal2012IO {
  def loadRaw(zipedFile : File) : ElectionData = {
      val zipFile = new ZipFile(zipedFile)
      import collection.JavaConverters._
      var name = ""
      def lines(filesubname:String) : List[String] = {
        zipFile.entries().asScala.find { _.getName.contains(filesubname) } match {
          case Some(entry) => 
            val filename = entry.getName
            name = filename.substring(0,filename.indexOf(filesubname))
            val s = Source.fromInputStream(zipFile.getInputStream(entry))
            val res = s.getLines().toList
            s.close()
            res
          case None => throw new Exception("Could not find "+filesubname+" in "+zipedFile)
        }
      }
      // process candidates
      val candidates = new collection.mutable.HashMap[String,Candidate] // candidate_id to candidate
      val orderedCandidates = new collection.mutable.ArrayBuffer[Candidate]
      for (l<-lines("CANDIDATES").tail) {
        val ll = l.split('\t')
        if (ll.length!=4) throw new Exception("Do not understand candidate line "+l)
        val candidate = new Candidate(ll(1),ll(2),ll(3).toInt)
        candidates+= ll(0) -> candidate
        orderedCandidates+=candidate
      }
      // process votes - interpret all as below the line as that's what is in the data file.
      val helper = new VoteInterpreter(Array(),orderedCandidates.length)
      for (l<-lines("PREFERENCES").tail) {
        val ll = l.split('\t')
        if (ll.length!=6) throw new Exception("Do not understand candidate line "+l)
        val ballotID = ll(1).toInt
        val preferenceNumber = ll(3).toInt
        val candidateid = ll(4)
        val votetype = ll(5)
        helper.addBTL(ballotID, candidates.get(candidateid).get, preferenceNumber)
      }
      if (name.endsWith("_")) name=name.substring(0,name.length-1)
      helper.getData(orderedCandidates.toArray,name,"2012")
  }
  
}

object NSWLocal2016IO {
  
  import scala.collection.JavaConverters._
  import scala.collection.mutable.ArrayBuffer
  
  def parseDOP1ForCandidates(doc:Document) : (Array[Candidate],Array[GroupInformation]) = {
    val groups = new ArrayBuffer[GroupInformation]
    val candidates = new ArrayBuffer[Candidate]
    for (table<-doc.select("table").asScala) {
      val rows = table.select("tr").asScala
      val headings : Map[String,Int] = Map(rows.head.select("th").asScala.map{_.text.trim}.zipWithIndex :_*)
      val groupCol = headings.get("Group")
      var group = ""
      var groupPosition = 0
      for (candidateCol<-headings.get("Candidates in Ballot Order")) {
         for (row<-rows.tail) {
           val entries : IndexedSeq[String] = row.select("td").asScala.toIndexedSeq.map{_.text.trim}
           val name = entries(candidateCol)
           val groupStart = groupCol.map{entries(_)}.getOrElse("")
           if (groupStart.length>0) { group=groupStart; groupPosition = 0; groups+=new GroupInformation(groupStart,name,None,Array()) }
           else if (name=="UNGROUPED CANDIDATES") { group=""; groupPosition = 0 }
           else if (name!="Formal Votes"&&name!="Informal Ballot Papers"&&name!="Total Votes / Ballot Papers"&&name!="Group Total"&&name!="Exhausted") {
             candidates+=new Candidate(name,group,groupPosition)
             if (group.length>0) groupPosition+=1
           }
         }
      }
    }
    (candidates.toArray,groups.toArray)
  }
  
  def loadRaw(zipedFile : File,orderedCandidates:Array[Candidate],orderedGroups:Array[GroupInformation],name:String) : ElectionData = {
      val zipFile = new ZipFile(zipedFile)
      import collection.JavaConverters._
      val lines = Source.fromInputStream(zipFile.getInputStream(zipFile.entries().asScala.toList.head)).getLines().toList
      // process candidates
      val candidates  : Map[String,Candidate] = Map(orderedCandidates.map{c=>(c.name,c)} : _*) // candidate_id to candidate
      // process votes - interpret all as below the line as that's what is in the data file.
      val helper = new VoteInterpreter(orderedGroups,orderedCandidates.length)
      for (l<-lines.tail) {
        val ll = l.split('\t')
        if (ll.length<10) throw new Exception("Do not understand candidate line "+l)
        if (ll(9)=="Formal" && ll(5).length>0) {
          if (ll.length!=11) throw new Exception("Do not understand candidate line "+l)
          val ballotID = ll(3).toInt
          val preferenceNumber = ll(5).toInt
          val groupCode = ll(7)
          val candidatename = ll(6)
          ll(10) match {
            case "SATL" => helper.addSATL(groupCode)
            case "RATL" => helper.addRATL(ballotID, groupCode, preferenceNumber)
            case "BTL" => helper.addBTL(ballotID, candidates(candidatename), preferenceNumber)
          }
        }
      }
      helper.getData(orderedCandidates,name,"2016")
  }

}