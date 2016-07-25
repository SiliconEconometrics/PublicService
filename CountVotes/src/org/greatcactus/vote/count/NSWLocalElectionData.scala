/*
    Copyright 2015-2016 Silicon Econometrics Pty. Ltd.

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
package org.greatcactus.vote.count

import java.io.File
import scala.io.Source
import java.util.zip.ZipInputStream
import java.io.FileInputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import scala.util.Random

object NSWLocal2012IO {
  def loadRaw(zipedFile : File) : ElectionData = {
      val zipFile = new ZipFile(zipedFile)
      import collection.JavaConversions._
      var name = ""
      def lines(filesubname:String) : List[String] = {
        zipFile.entries().toList.find { _.getName.contains(filesubname) } match {
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
      helper.getData(orderedCandidates.toArray,name)
  }
  
}
