/*
    Copyright 2015-2020 Silicon Econometrics Pty. Ltd.

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

import java.io.{File, FileWriter, PrintWriter}
import java.util.concurrent.atomic.AtomicBoolean

import org.greatcactus.vote.count.MainDataTypes.CandidateIndex
import org.greatcactus.vote.count.ballots.{Candidate, ElectionData, ElectionMetadata}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class StochasticReportOptions(
                               val dir:File,
                               /** Merge election outcomes that only differ in the person excluded */
                               val ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean
                             )

trait StochasticStatusOutput {
  def status(isFinished:Boolean,heading:String,margin:String,candidates:IndexedSeq[CandidateStat])
}

/** information about a candidate being run probabilistically */
class CandidateStat(val name:String,val proportionWon:Double,val meanPosition:Double) {
  override def toString = f"$name%s\t$proportionWon%.6f\t$meanPosition%.3f"
  def toTextTableRow(elected:Boolean): String = {
    val isElected = if (elected) "\u2713" else "\u2717"
    f"$name%s\t$proportionWon%.6f\t$meanPosition%.3f\t$isElected%s"
  }
  def toTableRow = f"<tr><td>$name%s</td><td>$proportionWon%.6f</td><td>$meanPosition%.3f</td></tr>"
  def toHTMLTableRow(elected:Option[Int]): String = {
    val isElected = elected match {
      case Some(rank) => (rank+1).toString
      case None => ""
    }
    f"<tr><td>$name%s</td><td>$proportionWon%.6f</td><td>$meanPosition%.6f</td><td>$isElected%s</td></tr>"
  }
}
/** Keep track of multiple non-deterministic runs of the election */
class ElectionStats(val candidates:Array[Candidate],printProgress:Boolean,output:Option[StochasticStatusOutput]) {
  val numCandidates: Int = candidates.length
  val numTimesCandidateWon: Array[Int] = new Array[Int](numCandidates)
  val sumOfPositions: Array[Int] = new Array[Int](numCandidates)
  var numRuns : Int = 0
  val startTime: Long = System.currentTimeMillis()
  var minMargin: Double = Double.MaxValue
  var maxMargin: Double = 0.0
  var sumMargin: Double = 0.0
  var sumSqMargin: Double = 0.0

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
      val meanMargin = sumMargin/numRuns.toDouble
      val sdMargin = Math.sqrt((sumSqMargin-meanMargin*sumMargin)/(numRuns-1))
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

/** If the randomization is parameterizable, build a table as a function of the parameter. */
class MultipleElectionStats(parameterName:String) {
  private val table = new ArrayBuffer[(String,ElectionStats)]
  def add(parameter:String,results:ElectionStats): Unit = table+=((parameter,results))
  def printTable(pw:PrintWriter): Unit = if (table.nonEmpty) {
    val candidates = table.head._2.candidates
    val candidatesWinningAtLeastOnce : Seq[CandidateIndex] = candidates.indices.filter(i=>table.exists{case (_,stats) => stats.numTimesCandidateWon(i)>0})
    // print headings
    pw.print(parameterName);
    for (i<-candidatesWinningAtLeastOnce) { pw.print(',');pw.print('"'); pw.print(candidates(i).name);pw.print('"')}
    pw.println()
    // print data lines
    for ((parameter,stats)<-table) {
      pw.print(parameter);
      for (i<-candidatesWinningAtLeastOnce) { pw.print(','); pw.print(stats.numTimesCandidateWon(i))}
      pw.println()
    }
  }
  def printTable(f:File): Unit = {
    val pw = new PrintWriter(new FileWriter(f))
    printTable(pw)
    pw.close()
  }
}


abstract class ProbabilisticRunner(reportLocations:Option[StochasticReportOptions]) {

  /** Do the actual probabilistic run, possibly concurrently with other runs */
  def doOneRun(random:Random) : ElectionResultReport

  /** Add said report to some tracker */
  def addStochiasticReports(report:ElectionResultReport): Unit = {
    if (reportLocations.isDefined) this.synchronized {
      val stochrep = stochasticReports.getOrElseUpdate(report.stucture(reportLocations.get.ignoreWhoIsEliminatedForMergingIntoStochasticReport),{report.makeStochastic(reportLocations.get.ignoreWhoIsEliminatedForMergingIntoStochasticReport);report})
      stochrep.addStochastic(report)
    }
  }


  val stochasticReports = new collection.mutable.HashMap[String,ElectionResultReport] // only used if reportLocations.isDefined

  def runProbabilisticly(metadata:ElectionMetadata,numTotalRuns:Int,numThreads:Int,printProgress:Boolean,output:Option[StochasticStatusOutput],storeExample:Option[File]=None)  : ElectionStats = {
    // if (output.isEmpty) votedata.printStatus()
    stochasticReports.clear()

    var needToWriteReport : AtomicBoolean = new AtomicBoolean(storeExample.isDefined)

    val cumulativeStats = new ElectionStats(metadata.candidates,printProgress,output)

    def runOnce(random:Random) {
      val report = doOneRun(random)
      cumulativeStats.addElected(report.electedCandidates,report.lastMargin)
      addStochiasticReports(report)
      if (needToWriteReport.getAndSet(false)) ElectionReport.saveReports(storeExample.get,report,metadata)
    }

    def runMultiple(numRunsPerThread:Int) {
      try {
        val random = new Random
        for (_ <-0 until numRunsPerThread) runOnce(random)
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

    reportLocations match {
      case Some(options) =>
        val dir = options.dir
        dir.mkdirs()
        def delete(f:File) {
          if (f.isDirectory) f.listFiles.foreach { delete }
          f.delete()
        }
        delete(dir)
        dir.mkdirs()
        var count = 1
        for (report<-stochasticReports.values.toList.sortBy { - _.numStochastic}) {
          val subdir = new File(dir,count.toString)
          count+=1
          ElectionReport.saveReports(subdir,report,metadata)
        }
      case None => // do nothing
    }

    cumulativeStats
  }
}
