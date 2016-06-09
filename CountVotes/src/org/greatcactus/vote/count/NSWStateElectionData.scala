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

import java.io.BufferedReader
import java.io.FileReader
import java.io.PrintWriter
import java.io.FileWriter
import java.io.InputStream
import java.io.File
import java.util.zip.ZipFile
import java.io.InputStreamReader


object RunNSWStateElectionProbabilistically extends App {
  
  val numTotalRuns = 100
  val numThreads = 4
  
  val votedata = NSWStateElectionData.load(true,true)
  val stochasticReport = new StochasticReportOptions(new File(NSWStateElectionData.reportDir,"Stochastic"),true)
  
  ProbabilisticWork.runProbabilisticly(numTotalRuns, numThreads, votedata,true,21,NSWLegislativeCouncilRules,None,Set.empty,Some(stochasticReport))
}

object NSWStateElectionData {
  /** Where the data is stored. This is the file that can be downloaded from the NSWEC VTR website */
  val zipedFile = new File("Elections/NSW/State2015/SGE2015 LC Pref Data Statewide.zip")
  /** Where the cache (quick load) files should be stored */
  val cacheDir = new File("Cache/NSWState2015")
  /** Where reports should be stored */
  val reportDir = new File("State2015Reports")
  
  val groups = "ABCDEFGHIJKLMNOPQRSTX"
  val numGroups = groups.length
  val groupIndex : Map[Char,Int] = Map.empty++(groups.zipWithIndex)
  
  val numCandidates = 394
  def fastfile(useIvote:Boolean,useNormal:Boolean) : File = new File(cacheDir,"Fast "+(if (useIvote) "ivote " else "")+(if (useNormal) "normal " else "")+"Preferences.txt")
  
  def load(useIvote:Boolean,useNormal:Boolean) : ElectionData = try { loadPickled(useIvote,useNormal) } catch { case e:Exception => e.printStackTrace(); println("Loading manually"); val res = loadRaw(useIvote:Boolean,useNormal:Boolean); savePickled(res,useIvote:Boolean,useNormal:Boolean); res }
  def savePickled(data:ElectionData,useIvote:Boolean,useNormal:Boolean) {
    val file = fastfile(useIvote:Boolean,useNormal:Boolean)
    file.getParentFile.mkdirs()
    val w = new PrintWriter(new FileWriter(file))
    def go[T <: Dumpable](a:Array[T]) {
      w.println(a.length)
      for (e<-a) w.println(e.line)
    }
    go(data.candidates)
    go(data.satls)
    go(data.ratls)
    go(data.btls)
    w.close()  
  }
  
  def loadPickled(useIvote:Boolean,useNormal:Boolean) : ElectionData = {
    val r = new BufferedReader(new FileReader(fastfile(useIvote:Boolean,useNormal:Boolean)))
    //val pickle = BinaryPickle(is)
    import scala.reflect._
    def read[T](f:String=>T)(implicit tag : ClassTag[T]) : Array[T] = {
      val len = r.readLine().toInt
      val res = new Array[T](len)
      for (i<-0 until len) res(i)=f(r.readLine())
      res
    }
    val candidates = read[Candidate]{s=>val ss = s.split('\t'); new Candidate(ss(0),ss(1),ss(2).toInt)}
    val satls = read[SATL]{s=>val ss=s.split('\t'); new SATL(ss(0).charAt(0),ss(1).toInt)}
    val ratls = read[ATL]{s=>val ss=s.split('\t'); new ATL(ss(0).toCharArray(),ss(1).toInt)}
    val btls = read[BTL]{s=>new BTL(s.split(',').map{_.toInt})}
    val res = new ElectionData("NSW",candidates,satls,ratls,btls) //  pickle.unpickle[ElectionData]
    r.close()
    res
  }
    
  def loadRaw(useIvote:Boolean,useNormal:Boolean) : ElectionData = {
    val zipFile = new ZipFile(zipedFile)
    val entry = zipFile.getEntry("SGE2015 LC Pref Data_NA_State.txt")
    loadRaw(useIvote,useNormal,zipFile.getInputStream(entry))
  }
  def loadRaw(useIvote:Boolean,useNormal:Boolean,is:InputStream) : ElectionData = {
    val r = new BufferedReader(new InputStreamReader(is))
    var line : String = r.readLine();
    val candidates = new scala.collection.mutable.HashMap[String,Candidate]
    val helper = new VoteInterpreter(groups,numCandidates,numGroups)
    try { while (line!=null) {
        val fields = line.split('\t')
        if (fields.length!=12) {
          if (fields.length!=11 || fields(10)!="Informal") println("Wrong number of fields for "+line)
        }
        else if (fields(10)=="Formal" && !fields(6).isEmpty) {
          val ballotID = fields(4).toInt
          val preferenceNumber = fields(6).toInt
          def candidate() : Candidate = {
              val candidateName = fields(7)
              candidates.getOrElseUpdate(candidateName,new Candidate(candidateName,fields(8),fields(9).toInt))
          }
          val isIvote = fields(3)=="iVote"
          if (if (isIvote) useIvote else useNormal) fields(11) match {
            case "BTL" => helper.addBTL(ballotID, candidate, preferenceNumber)
            case "SATL" => helper.addSATL(groupIndex(fields(8).charAt(0))) 
            case "RATL" => helper.addRATL(ballotID, fields(8).charAt(0), preferenceNumber)
          } else { if (fields(11)=="BTL") candidate() }
        }
        line=r.readLine();  
    }} catch {
      case e : Exception =>
        println(line)
        e.printStackTrace()
    }
    
    def groupLT(s1:String,s2:String) : Boolean = {
      if (s1=="") false else if (s2=="") true else s1<s2
    }
    val orderedCandidates = candidates.values.toList.sortWith((c1,c2)=>groupLT(c1.group,c2.group) || (c1.group==c2.group&&c1.position<c2.position)).toArray
    helper.getData(orderedCandidates,"NSW")
  }

}