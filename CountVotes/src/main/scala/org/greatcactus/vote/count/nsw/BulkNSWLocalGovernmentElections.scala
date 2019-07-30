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

import org.greatcactus.vote.count._
import java.io.File

import scala.io.Source
import java.net.URL
import java.io.FileOutputStream

import scala.collection.mutable.ListBuffer
import java.awt.Desktop
import java.net.URI
import java.util.zip.ZipFile

import scala.io.Codec
import java.io.PrintWriter
import java.io.OutputStreamWriter

import com.google.gson.Gson
import org.greatcactus.vote.count.ballots.ElectionData
import org.jsoup.nodes.Document
import org.jsoup.Jsoup

object AllNSWLocalGovernmentElections2016 extends App {
  ParseNSW2016.parseAll()
}
object AllNSWLocalGovernmentElections2017 extends App {
  ParseNSW2017.parseAll()
}
object AllNSWLocalGovernmentElections2012 extends App {
  ParseNSW2012.processAll2012()
}

/**
 * Downloading all the information by web browser and putting it into the right place takes time. 
 * This is a utility to search the web browser downloads directory for the ".zip"
 * files containing the votes to work out who they correspond to (as the all have the same file name).
 * They can then be automatically copied into the correct place.
 * Only works 2012 LGE
 */
object SearchDownloadsDir2012 {
  val downloads : File = new File("""/home/arc/Downloads""")
  def findPreferenceZipFiles : Map[String,File] = {
    var map : Map[String,File] = Map.empty
    for (f<-downloads.listFiles()) {
      val filename = f.getName
      if (filename.startsWith("11 - Det") && filename.endsWith(".zip")) {
        val zipFile = new ZipFile(f)
        import collection.JavaConverters._
        for (longname<-zipFile.entries().asScala.map{_.getName}.find { _.contains("CANDIDATES")})  {
            val name = longname.substring(0,longname.length-"CANDIDATES.txt".length).replace("_"," ").replace("-"," ").trim().toLowerCase()
            println("Found "+name)
            map+=name->f
        }
        zipFile.close()
      }
    }
    map
  }
}

/** Directory to cache stuff in. If a URL has no extension, cache in directory/index.htm to prevent collisions between file names and directory names */
class CacheDir(val dirname:String) {
  val dir = new java.io.File(dirname)
  def where(path:String,whereFileGetsRedirectedTo:Option[String]=None) : java.io.File = {
    for (alternate<-whereFileGetsRedirectedTo) {
      val ff = new java.io.File(dir,alternate)
      if (ff.exists()) return ff
    }
    val f = new java.io.File(dir,path)
    if (path.contains(".")) f else new java.io.File(f,"index.html")
  }
}


object ParseNSW2016 extends ParseNSW2016format(2016)
object ParseNSW2017 extends ParseNSW2016format(2017)

class ParseNSW2016format(year:Int) extends NSWLocalData(year) {
  import DownloadURL._
  

  val hour : Long = 1000L*60*60
  val redownloadTryInterval : Long = hour*4 // 4 hours

  /* Say that the cached file at url contains data that is not final, and it should be redownloaded. To prevent too much work, don't redownload if last done recently */
  def markToBeUpdated(url:String) {
    val f = cacheFile(urlBase+url,cacheDownloadsDir)
    if (f.exists()) {
      val age = System.currentTimeMillis()-f.lastModified()
      if (age>redownloadTryInterval) {
        println("Deleting cache file for "+url+" as "+(age/hour)+" hours old and not final.")
        f.delete()
        cacheFile(urlBase+url,cacheDownloadsDir)
      } else println("Not updating cache file for "+url+" as only "+(age/hour)+" hours old.")
    }
  }

  def parseAll() { // should work for 2016 and 2017.
    val runs = new RunLotsOfContests(year)
    getAndRunJSon("/councils.json",classOf[Array[NSW2016Council]],{areas : Array[NSW2016Council] => {
      for (area<-areas) if ((area.status=="NSWEC_Run")&&(restrictCalculatedTo.isEmpty || restrictCalculatedTo.get.contains(area.areaAuthorityName))) {
        println("Area "+area.areaId+"\t"+area.areaAuthorityName)
        val areaurlbase = "/data/"+area.areaId+"/"
        var mayor : Option[String] = None
        getAndRunJSon(areaurlbase+"lga.json",classOf[NSW2016LGA],{lga:NSW2016LGA => {
              import scala.collection.JavaConverters._
          def processWard(ward:NSW2016LGAWard,isWard:Boolean) {
//            println(" Ward "+ward.id+" available "+ward.isDataAvailable+" isWard="+isWard)
            def parseResult(result:Document) = {
              val winners = result.select("span.candidate-name").asScala
              val names = for (winner<-winners) yield reverseName(winner.text())
//              for (name<-names) println("     "+name)
              names.toArray
            }
            if (ward.isDataAvailable && (restrictWardTo.isEmpty || restrictWardTo.get.contains(ward.id))) {
              if (ward.isMayoral) {
                val winner = getAndRunJsoup(areaurlbase+"result_mayoral.html",parseResult _)
                if (winner.isDefined && winner.get.length==1) mayor=Some(winner.get.head)
                if (mayor.isEmpty) markToBeUpdated(areaurlbase+"result_mayoral.html")
              } else if (isWard || ward.id=="councillor") {
                // process council election
                val wardUrlBase = areaurlbase+(if (isWard) ward.id+"/" else "")
                val winners1 = getAndRunJsoup(wardUrlBase+"result_councillor.html",parseResult _)
                val winners2 = getAndRunJsoup(wardUrlBase+"candidates_in_sequence.html",{doc=>{doc.select("td[headers=1]").asScala.map{_.text()}.toArray}})
                val winners = if (winners2.isDefined && winners2.get.length>0) winners2 else if (winners1.isDefined && winners1.get.length>0) winners1 else None // winners2 more reliable than winners1
//                if (winners2.isDefined) { println("winners2 : "); winners2.get.foreach { println _ } }
                if (winners.isDefined && winners.get.length>0) {
                  val dop1 = getAndRunJsoup(wardUrlBase+"dop_cnt_001.html",{doc=>NSWLocal2016IO.parseDOP1ForCandidates(doc)})
                  if (dop1.isDefined) {
                    val (candidates,groups)=dop1.get
                    val candidateNames = candidates.map{_.name}
                    val dop = getAndRunJsoup(wardUrlBase+"dop_index.html",{doc=>{(0::(doc.select("td a").asScala.toList.map{_.text().toInt})).max}})
                    val dopsum : Option[NSWWholeCountSummary] = if (dop.isDefined && dop.get>0 &&area.areaId!="eurobodalla-shire-council") {
                      //println("dop contains "+dop.get+" rounds")
                      val dops = for (i<-1 to dop.get) yield getAndRunJsoup(wardUrlBase+"dop_cnt_"+("%03d".format(i))+".html",{doc=>NSWCountSummary.ofJSoup(doc, candidateNames)})
                      if (dops.forall { _.isDefined }) Some(new NSWWholeCountSummary(dops.map{_.get}.toArray)) else None
                    // download lots?
                    } else { markToBeUpdated(wardUrlBase+"dop_index.html"); None }

                    val ziplist = cacheDownloadFile(urlBase+wardUrlBase+"finalpreferencedatafile.zip",cacheDownloadsDir,"finalpreferencedatafile",".zip")
                    val prettyName = area.areaAuthorityName+(if (isWard) " "+ward.name else "")
                    val data = NSWLocal2016IO.loadRaw(ziplist,candidates,groups,prettyName)
                    for (summary<-dopsum) {
                      CheckTies.check(summary,candidateNames)
                      CheckRounding.check(summary,candidateNames)
                    }
                    runs.runElection(data, winners.get.toList, false, mayor,dopsum,year)
                  } else markToBeUpdated(wardUrlBase+"dop_cnt_001.html")
                } else { markToBeUpdated(wardUrlBase+"result_councillor.html"); markToBeUpdated(wardUrlBase+"candidates_in_sequence.html") }
              }
            } else if (!(ward.isReferendum||ward.isUncontested||ward.isPoll)) markToBeUpdated(areaurlbase+"lga.json")
            if (ward.isUncontested) runs.numUncontested+=1
          }
              /* need to run mayor first */
          for (ward<-lga.contests) if (ward.isMayoral) processWard(ward,false)
          for (ward<-lga.contests) if (!ward.isMayoral) processWard(ward,false)
          for (ward<-lga.wards) processWard(ward,true)
        }})
      } else runs.addAreaStatus(area.status)
    }})
    runs.saveOverallReports()
    ProbabilisticWork.writeExtremenessSummary()
  }
}

/** Used in gson parsing */
class NSW2016Council(val areaId:String,val areaAuthorityName:String,val status:String,val wardCount:String)
class NSW2016LGA(val id:String,val name:String,val wards:Array[NSW2016LGAWard],val contests:Array[NSW2016LGAWard])
class NSW2016LGAWard(val id:String,val realid:String,val name:String,val status:String,val isRecount:Boolean,val isFinal:Boolean,val isDeferred:Boolean) {
  def isDataAvailable : Boolean = isFinal && (status=="Publish_Results" || status=="Declared_Election")
  def isMayoral : Boolean = id=="mayoral"
  def isReferendum : Boolean = id=="referendum"
  def isPoll : Boolean = id=="poll"
  def isUncontested : Boolean = status=="Uncontested_Declared_Election"
}


object RunLotsOfContests {
  val giveDetailedReportOnOneRun = true
  val numToRun = 1000000
  val numThreads = 4

  val generateStochasticReports = false
  val useNWSECredistributableRules = false // the incorrect rules used accidentally by the NSWEC in the 2012 LGE.
  val useStochastic = true
  val electionRules = new ElectionRules(true,useNWSECredistributableRules,useStochastic)

}
class RunLotsOfContests(val year:Int) {
    val contestResults = new ContestResults
    var numUncontested = 0
    var areaStatuses : Map[String,Int] = Map.empty

    def addAreaStatus(status:String) { areaStatuses+= status->(1+(areaStatuses.getOrElse(status,0))) }

    def saveOverallReports() {
      def of(what:String) =  "LocalGovernmentNSW"+year+"_"+what+".html"
      contestResults.print(of("All"),"all",_ => true)
      contestResults.print(of("Stochastic"),"differ in any way",_.isStochastic)
      contestResults.print(of("StochasticElected"),"differ in candidates elected",_.isStochasticElectedCandidates)
      contestResults.print(of("NotOfficial"),"likely differ from official",_.likelyDifferFromOfficial)
      println("Uncontested contests "+numUncontested)
      for ((status,n)<-areaStatuses) println("Area status "+status+" : "+n)
    }

    def runElection(data:ElectionData,electedCandidates:List[String],isMayoral:Boolean,mayor:Option[String],officialDOP : Option[NSWWholeCountSummary],year:Int) {
                       val prettyName = data.meta.electionName.electorate
                       val ineligibleCandidates :Set[Int] = if (isMayoral) Set.empty else { mayor.map{data.candidateIndex(_)}.toSet- (-1) }
                       if (RunLotsOfContests.giveDetailedReportOnOneRun) {
                          val worker = new NSWElectionHelper(data,electedCandidates.length,new scala.util.Random,RunLotsOfContests.electionRules,ineligibleCandidates)
                          worker.run()
                          ElectionReport.saveReports(new File("LGE"+year+"reports/"+prettyName+"/"),worker.report,data)
                       }
                       val output = new StatusOutput {
                         def status(isFinished:Boolean,heading:String,margin:String,candidates:IndexedSeq[CandidateStat]) {
                           if (isFinished) {
                             val cr = new ContestResult(prettyName,electedCandidates,candidates)
                             if (cr.candidatesDifferFromOfficial) cr.printResults()
                             contestResults.add(cr)
                           }
                         }
                       }
                       val report = if (RunLotsOfContests.generateStochasticReports) Some(new StochasticReportOptions(new File("LGE"+year+"reports/"+prettyName+"/Stochastic"),false)) else None
                       ProbabilisticWork.runProbabilisticly(RunLotsOfContests.numToRun, RunLotsOfContests.numThreads, data,true,electedCandidates.length,RunLotsOfContests.electionRules,Some(output),ineligibleCandidates,report,officialDOP)

    }
}

class NSWLocalData(val year:Int,val electionType:String="LGE") {
  def isCurrent: Boolean = year==2019
  val cacheDownloadsDir = new CacheDir("Elections/NSW/"+electionType+year+"/cache")
  cacheDownloadsDir.dir.mkdirs()
  val urlBase = if (isCurrent) "http://vtr.elections.nsw.gov.au" else "http://www.pastvtr.elections.nsw.gov.au"

  import DownloadURL._
  def getSource(url:String) : Source = Source.fromFile(cacheFile(url,cacheDownloadsDir),"utf-8")

  val restrictCalculatedTo : Option[Set[String]] = None // Some(Set("Northern Beaches","Cumberland","Ku-rin-gai","North Sydney","City of Paramatta","City of Ryde")) // None // Some(Set("Hawkesbury City Council")) // Some(Set("Griffith"))
  val restrictWardTo : Option[Set[String]] = None // Some(Set("curl-curl-ward","greystanes-ward"))
  val gson = new Gson

    /** change Firstname LASTNAME to LASTNAME Firstname */
  def reverseName(candidatename:String) : String = {
                     val rawnames = candidatename.trim().split(" ")
                     val lessaffinity = {
                       val last = rawnames.last
                       if (last.startsWith("(") && last.endsWith(")")) rawnames.dropRight(1) else rawnames
                     }
                     val (firstnames,lastnames) = lessaffinity.span{s=>s.last.isLower}
                     (lastnames++firstnames).mkString(" ")// lessaffinity.drop(1).mkString(" ")+" "+lessaffinity(0)
  }

  def getAndRun[R](path:String,work: java.io.File=>R,/*If going to the URL actually loads the file from somewhere else. */ whereFileGetsRedirectedTo:Option[String]=None) : Option[R] = {
    val f = cacheFile(urlBase+path,cacheDownloadsDir,None,whereFileGetsRedirectedTo)
    if (f.exists()) {
      Some(work(f))
    } else None
  }

  def getAndRunJSon[R,T](path:String,clazz:Class[T],work:T=>R,/*If going to the URL actually loads the file from somewhere else. */ whereFileGetsRedirectedTo:Option[String]=None) : Option[R] = {
    getAndRun(path,jf =>{
      val parsedJson : T = gson.fromJson(IOUtil.loadFileAsString(jf), clazz)
      work(parsedJson)
    },whereFileGetsRedirectedTo)
  }

  def getAndRunJsoup[R](path:String,work:Document=>R,/*If going to the URL actually loads the file from somewhere else. */ whereFileGetsRedirectedTo:Option[String]=None) : Option[R] = {
    getAndRun(path,jf =>{
      val doc : Document = Jsoup.parse(jf,"UTF-8")
      try {
        work(doc)
      } catch { case e:Exception => println("Error with "+path); throw e;}
    },whereFileGetsRedirectedTo)
  }


}

object ParseNSW2012 extends NSWLocalData(2012) {
  private val listOfTownsURL = if (isCurrent) "http://vtr.elections.nsw.gov.au/lge-index.htm" else "http://www.pastvtr.elections.nsw.gov.au/LGE2012/lge-index.htm"
  private val listOfTownsPatternMatcher = """<p><a (?:class="disabled" )?title="([^"]*)" href="([^"]*)">([^<]*)</a></p>""".r
  private val listOfContestsPatternMatcher = if (year==2012) """<a title="Go to [^"]*" class="([^ ]*) urlfixed" href="([^"]*)">[^<]*""".r
                                     else """.*<a title="Go to [^"]*" class="([^"]*)" href="([^"]*)">""".r
  private val zipFilePatternMatcher = """.*title="Details Preference for Count ZIP" href="(/LGE201./Results/LGE201./PRCC/([^/]*)/11 - Details Preference for Count.zip)".*""".r
  private val detailsPreferenceFilePatternMatcher = """<li><strong><a class="FinalResultsFile" target="FinalResult" title="Details Preference for Count ZIP" href="([^"]*)">.*""".r
  private val electedCandidatePatternMatcher = """<span class="candidate_name">([^<]*)</span>""".r

  def processAll2012() {
    val runs = new RunLotsOfContests(2012)
    val zipmap = SearchDownloadsDir2012.findPreferenceZipFiles
    val listOfTowns = getSource(listOfTownsURL)
    for (line<-listOfTowns.getLines()) {
      // println(line)
      line.trim match {
        case listOfTownsPatternMatcher(title,relurl,human) =>
           println(title+"\t"+relurl+"\t"+human)
           val index = DownloadURL.cacheFile(urlBase+relurl,cacheDownloadsDir)
           class ContestType(val contesttype:String,val contestrelurl:String,val electedCandidates:List[String]) {
             def isMayoral = electedCandidates.length==1
           }
           val contests = new ListBuffer[ContestType]
           val realindex = if (year==2012) index else cacheDownloadsDir.where("/data"+relurl+"/summary.html")
           println(realindex.exists())
           if (realindex.exists()) for (line<-Source.fromFile(realindex).getLines()) line.trim() match {
             case listOfContestsPatternMatcher(contesttype,contestrelurl) => // <a title="Go to Councillor" class="contest urlfixed" href="/LGE2012/tumbarumba-shire-council/councillor/summary/index.htm">
               println(s" Contest $contestrelurl type $contesttype")
               val index2 = DownloadURL.cacheFile(urlBase+contestrelurl,cacheDownloadsDir)
               var haveFinalResults = false
               val electedCandidates = new collection.mutable.ListBuffer[String]
               if (index2.exists()) {
                 for (l<-Source.fromFile(index2).getLines()) l.trim match {
                   case """<li><a id="final" href="../final-results/index.htm" title="Final Results">Final Results</a></li>""" => haveFinalResults = true
                   case electedCandidatePatternMatcher(candidatename) =>
                     electedCandidates+=reverseName(candidatename)
                   case _ =>
                 }
               }
               if (haveFinalResults) contests+=new ContestType(contesttype,contestrelurl,electedCandidates.toList) // save for later so can extract mayor
             case s => // println(s) // http://vtr.elections.nsw.gov.au/data/sutherland-shire-council/a-ward/finalpreferencedatafile.zip
                                     // http://vtr.elections.nsw.gov.au/data/sutherland-shire-council/b-ward/dop_index.html
                                     // http://vtr.elections.nsw.gov.au/data/sutherland-shire-council/b-ward/dop_cnt_001.html
           }
           // should extract mayor who is not allowed to win.
           val mayor : Option[String] = contests.find { _.isMayoral}.map{_.electedCandidates(0)}
           for (contest<-contests) {
                 val index3 = DownloadURL.cacheFile(urlBase+contest.contestrelurl.replace("summary","final-results"),cacheDownloadsDir)
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
                     val indexzip = DownloadURL.cacheFile(urlBase+ziprelurl.replace("%20"," "),cacheDownloadsDir,foundDownload)
                     if ((restrictCalculatedTo.isEmpty||restrictCalculatedTo.get.contains(prettyName))&&indexzip.exists()) {
                       val data = NSWLocal2012IO.loadRaw(indexzip)// .reverseCandidateOrder
                       runs.runElection(data,contest.electedCandidates,contest.isMayoral,mayor,None,year)
                     }
                   case _ =>
                 }
               }
        case _ =>
      }
      
    }
    listOfTowns.close()
    runs.saveOverallReports()
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
    fos.println("<html><head><meta charset='UTF-8'/><title>"+description+"</title></head>")
    fos.println("<body>")
    val pass = list.filter(predicate)
    fos.println("<p>Showing "+description+", "+pass.length+" of "+list.length+" contests</p>")
    for (c<-pass) c.printHTML(fos)
    fos.println("<p>Disclaimer: This is the results of running our program. It may or may not be correct. No warranties provided.</p>")
    fos.println("<p>This is based on data that is © State of New South Wales through the Office of New South Wales Electoral Commission</p>")
    fos.println("</body>")
    fos.println("</html>")
    fos.close()
    
  }
}

object DownloadURL {
  val alreadyRequested = new collection.mutable.HashSet[String]
  /** Prompt the user to download a URL and return a (local) file in cacheDir containing it. If file already exists, don't bother redownloading. */
  def cacheFile(urlToDownload:String,cacheDir:CacheDir,existingFile:Option[File]=None,/*If going to the URL actually loads the file from somewhere else. */ whereFileGetsRedirectedTo:Option[String]=None) : File = {
    val url = new URL(urlToDownload)
    val cached = cacheDir.where(url.getPath,whereFileGetsRedirectedTo)
    if (!cached.exists()) existingFile match {
      case Some(fileToCopy) =>
        IOUtil.copyFile(fileToCopy,cached)
      case None =>
        if (!alreadyRequested.contains(urlToDownload)) {
           alreadyRequested+=urlToDownload
           println("Don't have "+urlToDownload+" which should be saved at "+cached.toString)
           startBrowser(urlToDownload)
           Thread.sleep(5000) // Impolite to put too much load on servers... the above line will open up a tab in a web browser. Unfortunately it is then up to the user to put it in the right place :-(
        }
    }
    cached
  }
  val downloadDir = new File(System.getProperty("user.home"),"Downloads")
  def cacheDownloadFile(urlToDownload:String,cacheDir:CacheDir,prefixExpected:String,suffixExpected:String) : File = {
    val url = new URL(urlToDownload)
    val cached = cacheDir.where(url.getPath)
    if (!cached.exists()) {
      val oldContents = downloadDir.list()
      startBrowser(urlToDownload) // open up a tab in a web browser. The resulting file will go to the downloads directory, which we can then find automatically with the following code.
      Thread.sleep(5000) // Impolite to put too much load on servers... 
      def newFiles() = (downloadDir.list().toSet--oldContents).toList
      
      var nf = newFiles()
      while (!(nf.length==1 && nf.head.endsWith(suffixExpected) && nf.head.startsWith(prefixExpected))) { Thread.sleep(1000); nf=newFiles() }
      println("Found "+nf.head)
      val downloadedFile = new java.io.File(downloadDir,nf.head)
      Thread.sleep(1000) // make sure it is all settled
      IOUtil.copyFile(downloadedFile,cached)
      downloadedFile.delete()
    }
    cached
  }
  
  def startBrowser(url:String) {
    Desktop.getDesktop.browse(new URI(url.replace(" ","%20")))
  }
}
