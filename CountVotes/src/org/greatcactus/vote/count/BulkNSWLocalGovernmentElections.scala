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
import java.net.URL
import java.io.FileOutputStream
import java.net.HttpURLConnection
import scala.collection.mutable.ListBuffer
import java.awt.Desktop
import java.net.URI
import java.util.zip.ZipFile
import scala.io.Codec
import java.io.PrintWriter
import java.io.FileWriter
import java.io.OutputStreamWriter

object AllNSWLocalGovernmentElections extends App {
  NSWLocalData.processAll()
}

/**
 * Downloading all the information by web browser and putting it into the right place takes time. 
 * This is a utility to search the web browser downloads directory for the ".zip"
 * files containing the votes to work out who they correspond to (as the all have the same file name).
 * They can then be automatically copied into the correct place.
 */
object SearchDownloadsDir {
  val downloads = new File("""C:\Users\Andrew\Downloads""")
  def findPreferenceZipFiles : Map[String,File] = {
    var map : Map[String,File] = Map.empty
    for (f<-downloads.listFiles()) {
      val filename = f.getName
      if (filename.startsWith("11 - Det") && filename.endsWith(".zip")) {
        val zipFile = new ZipFile(f)
        import collection.JavaConversions._
        for (longname<-zipFile.entries().toList.map{_.getName}.find { _.contains("CANDIDATES")})  {
            val name = longname.substring(0,longname.length-"CANDIDATES.txt".length).replace("_"," ").replace("-"," ").trim().toLowerCase()
            println("Found "+name);
            map+=name->f
        }
        zipFile.close()
      }
    }
    map
  }
}

object NSWLocalData {
  val cacheDir = new java.io.File("Elections/NSW/LGE2012/cache")
  cacheDir.mkdirs()
  val urlBase = "http://www.pastvtr.elections.nsw.gov.au"
  val listOfTownsURL = "http://www.pastvtr.elections.nsw.gov.au/LGE2012/lge-index.htm"
  val listOfTownsPatternMatcher = """<p><a title="([^"]*)" href="([^"]*)">([^<]*)</a></p>""".r
  val listOfContestsPatternMatcher = """<a title="Go to [^"]*" class="([^ ]*) urlfixed" href="([^"]*)">[^<]*""".r
  val zipFilePatternMatcher = """.*title="Details Preference for Count ZIP" href="(/LGE2012/Results/LGE2012/PRCC/([^/]*)/11 - Details Preference for Count.zip)".*""".r
  val detailsPreferenceFilePatternMatcher = """<li><strong><a class="FinalResultsFile" target="FinalResult" title="Details Preference for Count ZIP" href="([^"]*)">.*""".r
  val electedCandidatePatternMatcher = """<span class="candidate_name">([^<]*)</span>""".r
  import DownloadURL._
  def getSource(url:String) : Source = Source.fromFile(cacheFile(url,cacheDir),"utf-8")
  
  val numToRun = 10000
  val numThreads = 4
  val giveDetailedReportOnOneRun = false
  
  val generateStochasticReports = true
  val useNWSECredistributableRules = false
  val useStochastic = true
  val restrictCalculatedTo : Option[Set[String]] = Some(Set("Griffith"))
  val electionRules = new ElectionRules(true,useNWSECredistributableRules,useStochastic)
  
  def processAll() {
    val zipmap = SearchDownloadsDir.findPreferenceZipFiles
    val contestResults = new ContestResults
    val listOfTowns = getSource(listOfTownsURL) 
    for (line<-listOfTowns.getLines()) {
      // println(line)
      line.trim match {
        case listOfTownsPatternMatcher(title,relurl,human)=> 
           //println(title+"\t"+relurl+"\t"+human)
           val index = cacheFile(urlBase+relurl,cacheDir)
           class ContestType(val contesttype:String,val contestrelurl:String,val electedCandidates:List[String]) {
             def isMayoral = electedCandidates.length==1
           }
           val contests = new ListBuffer[ContestType]
           if (index.exists()) for (line<-Source.fromFile(index).getLines()) line.trim() match {
             case listOfContestsPatternMatcher(contesttype,contestrelurl) => // <a title="Go to Councillor" class="contest urlfixed" href="/LGE2012/tumbarumba-shire-council/councillor/summary/index.htm">
               
               //println(s" Contest $contestrelurl type $contesttype")
               val index2 = cacheFile(urlBase+contestrelurl,cacheDir)
               var haveFinalResults = false
               val electedCandidates = new collection.mutable.ListBuffer[String]
               if (index2.exists()) {
                 for (l<-Source.fromFile(index2).getLines()) l.trim match {
                   case """<li><a id="final" href="../final-results/index.htm" title="Final Results">Final Results</a></li>""" => haveFinalResults = true
                   case electedCandidatePatternMatcher(candidatename) => 
                     val rawnames = candidatename.trim().split(" ")
                     val lessaffinity = {
                       val last = rawnames.last
                       if (last.startsWith("(") && last.endsWith(")")) rawnames.dropRight(1) else rawnames
                     }
                     val (firstnames,lastnames) = lessaffinity.span{s=>s.last.isLower}
                     val modname = (lastnames++firstnames).mkString(" ")// lessaffinity.drop(1).mkString(" ")+" "+lessaffinity(0)
                     electedCandidates+=modname
                   case _ =>
                 }
               }
               if (haveFinalResults) contests+=new ContestType(contesttype,contestrelurl,electedCandidates.toList) // save for later so can extract mayor
             case _ =>
           }
           // should extract mayor who is not allowed to win.
           val mayor : Option[String] = contests.find { _.isMayoral}.map{_.electedCandidates(0)}
           for (contest<-contests) {
                 val index3 = cacheFile(urlBase+contest.contestrelurl.replace("summary","final-results"),cacheDir)
                 if (index3.exists) for (line<-Source.fromFile(index3)(Codec.ISO8859).getLines()) line.trim() match {
                   case zipFilePatternMatcher(ziprelurl,contestloc) => // <li><strong><a class="FinalResultsFile" target="FinalResult" title="Details Preference for Count ZIP" href="/LGE2012/Results/LGE2012/PRCC/Albury/11 - Details Preference for Count.zip">11 - Details Preference for Count.zip</a></strong> � This zip file provides preference details for all formal ballot papers used for count.</li></ul></div></div>
                     //println(s"  zip file at $ziprelurl $contestloc")
                     val generalname = contest.contestrelurl.substring(9,contest.contestrelurl.indexOf("/",10))
                     val extraname = if (contestloc.contains("%20")) " "+contestloc.substring(contestloc.indexOf("%20")+3) else ""
                     val prettyName = (human+extraname).replace("-"," ")
                     val lookingForName = (generalname+extraname).toLowerCase().replace("-"," ")
                     val lookingForName2 = (title+extraname).toLowerCase().replace("-"," ")
                     val lookingForName3 = (human+extraname).toLowerCase().replace("-"," ")
                     val foundDownload = zipmap.get(lookingForName).orElse(zipmap.get(lookingForName2)).orElse(zipmap.get(lookingForName3))
                     //println(s"   looking for $lookingForName found $foundDownload")
                     val indexzip = cacheFile(urlBase+ziprelurl.replace("%20"," "),cacheDir,foundDownload)
                     if ((restrictCalculatedTo.isEmpty||restrictCalculatedTo.get.contains(prettyName))&&indexzip.exists()) {
                       val data = NSWLocal2012IO.loadRaw(indexzip)// .reverseCandidateOrder
                       val ineligibleCandidates :Set[Int] = if (contest.isMayoral) Set.empty else { mayor.map{data.candidateIndex(_)}.toSet- (-1) }
                       if (giveDetailedReportOnOneRun) {
                          val worker = new NSWElectionHelper(data,contest.electedCandidates.length,new scala.util.Random,electionRules,ineligibleCandidates)
                          worker.run()
                          ElectionReport.saveReports(new File("LGE2012reports/"+prettyName+"/"),worker.report,data)
                       }
                       val output = new StatusOutput {
                         def status(isFinished:Boolean,heading:String,margin:String,candidates:IndexedSeq[CandidateStat]) {
                           if (isFinished) {
                             val cr = new ContestResult(prettyName,contest.electedCandidates,candidates)
                             if (cr.candidatesDifferFromOfficial) cr.printResults()
                             contestResults.add(cr)
                           }
                         }
                       }
                       val report = if (generateStochasticReports) Some(new StochasticReportOptions(new File("LGE2012reports/"+prettyName+"/Stochastic"),false)) else None
                       ProbabilisticWork.runProbabilisticly(numToRun, numThreads, data,true,contest.electedCandidates.length,electionRules,Some(output),ineligibleCandidates,report)
                     }
                   case _ =>
                 }
               }
        case _ =>
      }
      
    }
    listOfTowns.close()
    def of(what:String) =  "LocalGovernmentNSW2012_"+what+".html"
    contestResults.print(of("All"),"all",_ => true)
    contestResults.print(of("Stochastic"),"differ in any way",_.isStochastic)
    contestResults.print(of("StochasticElected"),"differ in candidates elected",_.isStochasticElectedCandidates)
    contestResults.print(of("NotOfficial"),"likely differ from official",_.likelyDifferFromOfficial)
  }
}

class ContestResult(val name:String,val officialCandidates:List[String],val stochasticResult:IndexedSeq[CandidateStat]) {
  val isStochasticElectedCandidates = stochasticResult.length>officialCandidates.length
  val isStochasticOrdering = stochasticResult.exists(c=> Math.floor(c.meanPosition)!=c.meanPosition)
  val isStochastic = isStochasticElectedCandidates || isStochasticOrdering
  val candidatesDifferFromOfficial = officialCandidates.toSet != stochasticResult.map{_.name}.toSet
  val likelyResults = stochasticResult.filter{_.proportionWon>0.5}.toList
  val likelyDifferFromOfficial = officialCandidates!=likelyResults.map{_.name}
  val formallyElected : Map[String,Int] = Map.empty++officialCandidates.zipWithIndex
  def printResults() {
    println(name)
    for (c<-stochasticResult) println(c.toTextTableRow(formallyElected.contains(c.name)))
    println
  }
  def printHTML(fos:PrintWriter) {
    fos.println("<p><b>"+name+"</b></p>")
    fos.println("<table class='elected'><tr><th>Candidate</th><th>Proportion Elected</th><th>Mean position</th><th>Official Count</th></tr>")
    for (c<-stochasticResult) fos.println(c.toHTMLTableRow(formallyElected.get(c.name)))
    fos.println("</table>")
  }
}

class ContestResults {
  private val list = new ListBuffer[ContestResult]
  def add(c:ContestResult) { list+=c }
  def print(filename:String,description:String,predicate:ContestResult=>Boolean) {
    val fos = new PrintWriter(new OutputStreamWriter(new FileOutputStream(filename),"UTF-8"))
    fos.println("<html>")
    fos.println("<body>")
    val pass = list.filter(predicate)
    fos.println("<p>Showing "+description+", "+pass.length+" of "+list.length+" contests</p>")
    for (c<-pass) c.printHTML(fos)
    fos.println("</body>")
    fos.println("</html>")
    fos.close()
    
  }
}

object DownloadURL {
  /** Prompt the user to download a URL and return a (local) file in cacheDir containing it. If file already exists, don't bother redownloading. */
  def cacheFile(urlToDownload:String,cacheDir:File,existingFile:Option[File]=None) : File = {
    val url = new URL(urlToDownload)
    val cached = new File(cacheDir,url.getPath)
    if (!cached.exists()) existingFile match {
      case Some(fileToCopy) =>
        IOUtil.copyFile(fileToCopy,cached)
      case None =>
        println("Don't have "+urlToDownload+" which should be saved at "+cached.toString)
        startBrowser(urlToDownload)
        Thread.sleep(5000) // Impolite to put too much load on servers... the above line will open up a tab in a web browser. Unfortunately it is then up to the user to put it in the right place :-(
    }
    cached
  }
  
  
  def startBrowser(url:String) {
    Desktop.getDesktop.browse(new URI(url.replace(" ","%20")))
  }
}
