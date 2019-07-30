/*
    Copyright 2015-2018 Silicon Econometrics Pty. Ltd.

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

import scala.util.Random
import java.io.File

import org.greatcactus.vote.count.ballots.{Candidate, ElectionData}



class StochasticReportOptions(
    val dir:File,
    /** Merge election outcomes that only differ in the person excluded */
    val ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean
    )
    
trait StatusOutput {
  def status(isFinished:Boolean,heading:String,margin:String,candidates:IndexedSeq[CandidateStat])
}

object ProbabilisticWork {
  val pendingExtremenessReports = new collection.mutable.ArrayBuffer[(String,Int,Double)] 
  lazy val extremenessDir = {
    val dir = new File("NSWExtremenessReports")
    if (dir.exists()) dir.listFiles().foreach { _.delete()}
    dir.mkdirs()
    ElectionReport.createCSS(dir)
    dir
  }
  def writeExtremenessSummary() {
    val xml = 
     <html>
      <head>
        <meta charset="UTF-8"/> 
        <title>Official vs. distribution Summary</title>
        <link href="report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <table class="Display">
          <tr class="Head"><th>Contest</th><th>Statistic</th><th>P Value</th></tr>
          {
            for (r<-pendingExtremenessReports) yield <tr class="Striped"><td><a href={r._1+".html"}>{r._1}</a></td><td>{r._2}</td><td>{r._3}</td></tr>
          }
        </table>
      </body>
    </html>
    scala.xml.XML.save(new File(extremenessDir,"About.html").toString,xml)
  }
  
  def runProbabilisticly(numTotalRuns:Int,numThreads:Int,votedata:ElectionData,printProgress:Boolean,numCandidatesToElect:Int,electionRules:ElectionRules,output:Option[StatusOutput],ineligibleCandidates:Set[Int],reportLocations:Option[StochasticReportOptions],officialDOP : Option[NSWWholeCountSummary]=None) {
     if (output.isEmpty) votedata.printStatus()

     val dist = new NSWDistributionSummary(votedata.numCandidates)
     
     val cumulativeStats = new ElectionStats(votedata.candidates,printProgress,output)
  
     val stochasticReports = new collection.mutable.HashMap[String,ElectionResultReport] // only used if reportLocations.isDefined
     var numExtremenessRunsDone = 0
     
     def addStochiasticReports(report:ElectionResultReport) {
       if (officialDOP.isDefined) {
         val summary = NSWWholeCountSummary.ofReport(report)
         this.synchronized {
           if (numExtremenessRunsDone*2<numTotalRuns) dist.addJustGenerateFunction(summary)
           else dist.addJustEvaluateFunctionDistribution(summary)
           numExtremenessRunsDone+=1
         }
       }
       if (reportLocations.isDefined) this.synchronized {
         val stochrep = stochasticReports.getOrElseUpdate(report.stucture(reportLocations.get.ignoreWhoIsEliminatedForMergingIntoStochasticReport),{report.makeStochastic(reportLocations.get.ignoreWhoIsEliminatedForMergingIntoStochasticReport);report})
         stochrep.addStochastic(report)     
       }
     }
     
     
     def runOnce(random:Random) {
       val worker = new NSWElectionHelper(votedata,numCandidatesToElect,random,electionRules,ineligibleCandidates)
       worker.run()
       cumulativeStats.addElected(worker.report.electedCandidates,worker.report.lastMargin)
       addStochiasticReports(worker.report)
     }
  
     def runMultiple(numRunsPerThread:Int) {
       try {
          val random = new Random
          for (i<-0 until numRunsPerThread) runOnce(random)    
       } catch { case e:Throwable => e.printStackTrace(); System.exit(1) }
     }
  
     val threads = for (i<-0 until numThreads) yield {
        val runsPerThreadBase = numTotalRuns/numThreads
        val leftOverRuns = numTotalRuns-runsPerThreadBase*numThreads
        val t = new Thread {
          override def run() { runMultiple(runsPerThreadBase+(if (i<leftOverRuns) 1 else 0)) }
        }
        t.start()
        t
     }
  
     for (t<-threads) t.join() // wait for everyone to finish
  
     cumulativeStats.printStats(true)
     
     if (officialDOP.isDefined) {
       val (extremeness,pValue) = dist.extremeness(officialDOP.get)
       val name = votedata.meta.electionName.shortPrintName
       pendingExtremenessReports+= ((name,extremeness,pValue))
       println(name+" Official DOP extremeness "+extremeness+" pValue="+pValue)
       scala.xml.XML.save(new File(extremenessDir,name+".html").toString, dist.extremenessReport(officialDOP.get, votedata.candidates.map{_.name}, name))
       
       //if (pValue<0.1) {
      //   dist.printoutDetailedExtremeness(officialDOP.get,votedata.candidates.map{_.name})
         //if (pValue<0.01) System.exit(0)
       //}
     }
     reportLocations match {
       case Some(options) =>
         val dir = options.dir
         dir.mkdirs()
         def delete(f:File) {
           if (f.isDirectory()) f.listFiles.foreach { delete _ }
           f.delete()
         }
         delete(dir)
         dir.mkdirs()
         var count = 1;
         for (report<-stochasticReports.values.toList.sortBy { - _.numStochastic}) {
           val subdir = new File(dir,count.toString)
           count+=1
           ElectionReport.saveReports(subdir,report,votedata)
         }
       case None => // do nothing
     }
  }
}

class CandidateStat(val name:String,val proportionWon:Double,val meanPosition:Double) {
  override def toString = f"$name%s\t$proportionWon%.6f\t$meanPosition%.3f"
  def toTextTableRow(elected:Boolean) = {
    val isElected = if (elected) "\u2713" else "\u2717"
    f"$name%s\t$proportionWon%.6f\t$meanPosition%.3f\t$isElected%s"
  }
  def toTableRow = f"<tr><td>$name%s</td><td>$proportionWon%.6f</td><td>$meanPosition%.3f</td></tr>"
  def toHTMLTableRow(elected:Option[Int]) = {
    val isElected = elected match {
      case Some(rank) => (rank+1).toString()
      case None => ""
    }
    f"<tr><td>$name%s</td><td>$proportionWon%.6f</td><td>$meanPosition%.6f</td><td>$isElected%s</td></tr>"
  }
}
class ElectionStats(candidates:Array[Candidate],printProgress:Boolean,output:Option[StatusOutput]) {
  val numCandidates = candidates.length
  val numTimesCandidateWon = new Array[Int](numCandidates)
  val sumOfPositions = new Array[Int](numCandidates)
  var numRuns = 0
  val startTime = System.currentTimeMillis()
  var minMargin = Double.MaxValue
  var maxMargin = 0.0
  var sumMargin = 0.0
  var sumSqMargin = 0.0
  
  def addElected(elected:Seq[Int],margin:Double) {
    synchronized {
       for ((candidate,position)<-elected.zipWithIndex) {
         numTimesCandidateWon(candidate)+=1
         sumOfPositions(candidate)+=position+1
       }
       sumMargin+=margin
       sumSqMargin+=margin.toLong*margin
       if (minMargin>margin) minMargin=margin
       if (maxMargin<margin) maxMargin=margin
       numRuns+=1
       if (printProgress&&(numRuns<10 || (numRuns%100==0))) printStats(false)
    }
  }
  
  def printStats(isFinished:Boolean) {
    synchronized {
       val time = System.currentTimeMillis()-startTime
       val timeSeconds = time/1000
       val msPerRun = time.toDouble/numRuns
       val heading = f"Elections run : $numRuns%d time : $timeSeconds%d seconds  time per run : $msPerRun%.1f ms"
       val meanMargin = sumMargin.toDouble/numRuns
       val sdMargin = Math.sqrt((sumSqMargin.toDouble-meanMargin*sumMargin)/(numRuns-1))
       val marginLine = if (maxMargin>0) f"Final margin min: $minMargin%.0f max: $maxMargin%.0f  mean: $meanMargin%.2f sd $sdMargin%.4f" else ""
       val meaningful = for (i<-0 until numCandidates if numTimesCandidateWon(i)>0) yield new CandidateStat(candidates(i).name,numTimesCandidateWon(i).toDouble/numRuns,sumOfPositions(i).toDouble/numTimesCandidateWon(i))
       val sorted = meaningful.sortBy { _.meanPosition }.sortBy{- _.proportionWon}
       output match {
         case Some(out) =>
           out.status(isFinished,heading,marginLine,sorted)
         case None =>
           println(heading)
           println(marginLine)
           for (s<-sorted) println(s.toString)
       }
    }
  }
  
}