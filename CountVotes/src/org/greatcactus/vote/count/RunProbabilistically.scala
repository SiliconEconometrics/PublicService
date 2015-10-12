/*
    Copyright 2015 Silicon Econometrics Pty. Ltd. 

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

import scala.util.Random

/**
 * @author Andrew
 */
object RunProbabilistically extends App {
  
  val numRunsPerThread = 2500
  val numThreads = 4
  
  val votedata = LoadFromFile.load(false)
  votedata.printStatus()

  val cumulativeStats = new ElectionStats(votedata.candidates)
  
  def runOnce(random:Random) {
    val worker = new NSWElectionHelper(votedata,21,random)
    worker.run()
    cumulativeStats.addElected(worker.report.electedCandidates,worker.report.lastMargin)
  }
  
  def runMultiple() {
    try {
      val random = new Random
      for (i<-0 until numRunsPerThread) runOnce(random)    
    } catch { case e:Throwable => e.printStackTrace(); System.exit(1) }
  }
  
  val threads = for (i<-0 until numThreads) yield {
    val t = new Thread {
      override def run() { runMultiple() }
    }
    t.start()
    t
  }
  
  for (t<-threads) t.join() // wait for everyone to finish
  
  cumulativeStats.printStats()

}

class CandidateStat(val name:String,val proportionWon:Double,val meanPosition:Double) {
  override def toString = f"$name%s\t$proportionWon%.6f\t$meanPosition%.3f"
}
class ElectionStats(candidates:Array[Candidate]) {
  val numCandidates = candidates.length
  val numTimesCandidateWon = new Array[Int](numCandidates)
  val sumOfPositions = new Array[Int](numCandidates)
  var numRuns = 0
  val startTime = System.currentTimeMillis()
  var minMargin = Int.MaxValue
  var maxMargin = 0
  var sumMargin = 0L
  var sumSqMargin = 0L
  
  def addElected(elected:Seq[Int],margin:Int) {
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
       if (numRuns<10 || (numRuns%100==0)) printStats()
    }
  }
  
  def printStats() {
    synchronized {
       val time = System.currentTimeMillis()-startTime
       val timeSeconds = time/1000
       val msPerRun = time/numRuns
       val heading = s"Elections run : $numRuns time : $timeSeconds seconds  time per run : $msPerRun ms"
       println(heading)
       val meanMargin = sumMargin.toDouble/numRuns
       val sdMargin = Math.sqrt((sumSqMargin.toDouble-meanMargin*sumMargin)/(numRuns-1))
       val marginLine = f"Final margin min: $minMargin%d max: $maxMargin%d  mean: $meanMargin%.2f sd $sdMargin%.4f"
       println(marginLine)
       val meaningful = for (i<-0 until numCandidates if numTimesCandidateWon(i)>0) yield new CandidateStat(candidates(i).name,numTimesCandidateWon(i).toDouble/numRuns,sumOfPositions(i).toDouble/numTimesCandidateWon(i))
       val sorted = meaningful.sortBy { _.meanPosition }
       for (s<-sorted) println(s.toString)
    }
  }
  
}