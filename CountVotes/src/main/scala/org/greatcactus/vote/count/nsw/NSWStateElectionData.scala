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

package org.greatcactus.vote.count.nsw

import java.io.BufferedReader
import java.io.InputStream
import java.io.File
import java.util.zip.ZipFile
import java.io.InputStreamReader

import org.greatcactus.vote.count.IOUtil
import org.greatcactus.vote.count.ballots._
import org.greatcactus.vote.count.ballots.parsing.VoteInterpreter
import org.jsoup.Jsoup
import org.jsoup.nodes.Document


object RunNSWStateElectionProbabilistically extends App {
  
  val numTotalRuns = 1000
  val numThreads = 4
  
  val votedata = NSWStateElectionData2019.load(useIvote = true,useNormal = true)
  val stochasticReport = new StochasticReportOptions(new File(NSWStateElectionData2019.reportDir,"Stochastic"),true)
  
  ProbabilisticWork.runProbabilisticly(numTotalRuns, numThreads, votedata,printProgress = true,21,NSWLegislativeCouncilRules,None,Set.empty,Some(stochasticReport))
}

object CheckNSWStateDOP extends App {
 // NSWStateElectionData2019.downloadDOP(Array())
  val data = NSWStateElectionData2019.load(false,true)
  data.printStatus()
}

// 2011 data files are at http://www.pastvtr.elections.nsw.gov.au/SGE2011/lc_prefdata.htm
object NSWStateElectionData2015 extends NSWStateElectionData("2015","ABCDEFGHIJKLMNOPQRSTUVWX","http://pastvtr.elections.nsw.gov.au/SGE2015/lc-home.htm")
object NSWStateElectionData2019 extends NSWStateElectionData("2019","ABCDEFGHIJKLMNOPQRST","https://vtr.elections.nsw.gov.au/lc/home")

class NSWStateElectionData(year:String,val groups:String,val downloadPage:String) extends NSWLocalData(year.toInt,"State") {
  /** Where the data is stored. This is the file that can be downloaded from the NSWEC VTR website */
  val zipedFile = new File("Elections/NSW/State"+year+"/SGE"+year+" LC Pref Data Statewide.zip")
  /** Where the cache (quick load) files should be stored */
  val cacheDir = new File("Cache/NSWState"+year)
  /** Where reports should be stored */
  val reportDir = new File("NSWState"+year+"Reports")
  
  val numGroups : Int = groups.length
  val groupIndex : Map[Char,Int] = Map.empty++ groups.zipWithIndex
  
  val numCandidates = 394
  def fastfile(useIvote:Boolean,useNormal:Boolean) : File = new File(cacheDir,"Fast "+(if (useIvote) "ivote " else "")+(if (useNormal) "normal " else "")+"Preferences.txt")
  
  def load(useIvote:Boolean,useNormal:Boolean) : ElectionData = try { loadPickled(useIvote,useNormal) } catch { case e:Exception => e.printStackTrace(); println("Loading manually"); val res = loadRaw(useIvote:Boolean,useNormal:Boolean); savePickled(res,useIvote:Boolean,useNormal:Boolean); res }
  def savePickled(data:ElectionData,useIvote:Boolean,useNormal:Boolean) {
    ElectionDataFastIO.savePickled(data, fastfile(useIvote:Boolean,useNormal:Boolean))
  }
  def loadPickled(useIvote:Boolean,useNormal:Boolean) : ElectionData = {
    ElectionDataFastIO.loadPickled(fastfile(useIvote:Boolean,useNormal:Boolean))
  }
    
  def loadRaw(useIvote:Boolean,useNormal:Boolean) : ElectionData = {
    val zipFile = new ZipFile(zipedFile)
    val entry = zipFile.getEntry("SGE"+year+" LC Pref Data_NA_State.txt")
    loadRaw(useIvote,useNormal,zipFile.getInputStream(entry))
  }
  def loadRaw(useIvote:Boolean,useNormal:Boolean,is:InputStream) : ElectionData = {
    val r = new BufferedReader(new InputStreamReader(is))
    var line : String = r.readLine()
    val candidates = new scala.collection.mutable.HashMap[String,Candidate]
    val helper = new VoteInterpreter(groups.toCharArray.map{c=>new GroupInformation(c.toString,c.toString,None,Array())},numCandidates)
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
            case "BTL" => helper.addBTL(ballotID, candidate(), preferenceNumber)
            case "SATL" => helper.addSATL(groupIndex(fields(8).charAt(0))) 
            case "RATL" => helper.addRATL(ballotID, fields(8), preferenceNumber)
          } else { if (fields(11)=="BTL") candidate() }
        }
        line=r.readLine()
    }} catch {
      case e : Exception =>
        println(line)
        e.printStackTrace()
    }
    
    def groupLT(s1:String,s2:String) : Boolean = {
      if (s1=="") false else if (s2=="") true else s1<s2
    }
    val orderedCandidates = candidates.values.toList.sortWith((c1,c2)=>groupLT(c1.group,c2.group) || (c1.group==c2.group&&c1.position<c2.position)).toArray
    helper.getData(orderedCandidates,new ElectionName(year,"NSWEC","NSW","NSW"),Array(downloadPage))
  }

  import collection.JavaConverters._



  def downloadDOP(candidateNames:Array[String]): Unit = {
    val dop = getAndRunJsoup("/lc/state/dop/dop_index",{doc=>{(0::(doc.select("td a").asScala.toList.map{_.text().toInt})).max}},Some("vtrdata-sg1901/lc/state/dop/dop_index_report.html"))
    val dopsum : Option[NSWWholeCountSummary] = if (dop.isDefined && dop.get>0) {
      //println("dop contains "+dop.get+" rounds")
      val dops = for (i<-1 to dop.get) yield {
        val countNo = "%03d".format(i)
        getAndRunJsoup("/lc/state/dop/dop_cnt_"+countNo,{doc=>null /* NSWCountSummary.ofJSoup(doc, candidateNames)*/},Some("vtrdata-sg1901/lc/state/dop/dop_cnt_"+countNo+"_report.html"))
      }
      if (dops.forall { _.isDefined }) Some(new NSWWholeCountSummary(dops.map{_.get}.toArray)) else None
      // download lots?
    } else None

  }

}