/*
    Copyright 2020 Silicon Econometrics Pty. Ltd.

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

import scala.collection.mutable.ArrayBuffer

class StatsFile(f:File) {

  private val writer = new PrintWriter(new FileWriter(f))

  def write(cols:Array[String]) { writer.println(cols.mkString(",")) }

  def close(): Unit = { writer.close() }
}

class MeanTracker(val name:String) {
  private var n : Int  = 0
  private var sum : Double = 0.0

  def add(v:Double) : Unit = { n+=1; sum+=v; }

  def count : Int = n
  def total : Double = sum
  def mean : Double = sum/n;
}

/** Track the means of multiple things of the same kind, where you get v_ij first by all the i values,
  * but want means for each i over all the j values.
  * Call add(v_ij) after each is available; call startSet() when a new j is available.
  * */
class MeanTrackerWidthFirst(val name:String) {
  private val trackers = new ArrayBuffer[MeanTracker]
  private var upto:Int = 0
  def startSet() : Unit = { upto=0; }
  def add(v:Double) : Unit = {
    if (upto==trackers.length) trackers+=new MeanTracker(name)
    trackers(upto).add(v)
    upto+=1
  }
  def length : Int = trackers.length
  def tracker(n:Int): MeanTracker = trackers(n)
}

/** Data on what happened on one ballot at OCR error simulation time */
class OCRVoteStatistic(val atl:Boolean,val validPrefs:Int,val hasError:Boolean)

class OCRPerElectionData() {
  private var numWithAtLeastOneError = 0
  private var numValidATL = 0
  private var numValidBTL = 0

  def add(stat:OCRVoteStatistic) : Unit = {
    if (stat.hasError) numWithAtLeastOneError+=1
    if (stat.validPrefs>0) {
      if (stat.atl) numValidATL+=1 else numValidBTL+=1
    }
  }
  def clear() :Unit ={ numWithAtLeastOneError=0; numValidATL=0; numValidBTL=0 }
  def data : Array[String] = Array(numWithAtLeastOneError.toString,numValidATL.toString,numValidBTL.toString)
  def headings : Array[String] = Array("Ballots with at least 1 error","Valid ATL ballots","Valid BTL ballots")
}

class OCRPerBallotData() {
  private val isATL = new MeanTrackerWidthFirst("1 is ATL")
  private val meanNumPrefs = new MeanTrackerWidthFirst("Mean num valid prefs")
  private val numTimesHaveError = new MeanTrackerWidthFirst("Num times have error")

  def add(stat:OCRVoteStatistic) : Unit = {
    isATL.add(if (stat.atl) 1 else 0)
    meanNumPrefs.add(stat.validPrefs)
    numTimesHaveError.add(if (stat.hasError) 1 else 0)
  }
  def endElection(): Unit = { isATL.startSet(); meanNumPrefs.startSet(); numTimesHaveError.startSet() }
  def length = meanNumPrefs.length
  def data(n:Int) : Array[String] = Array(isATL.tracker(n).mean.toInt.toString,meanNumPrefs.tracker(n).mean.toString,numTimesHaveError.tracker(n).total.toInt.toString)
  def headings : Array[String] = Array(isATL.name,meanNumPrefs.name,numTimesHaveError.name)
}

class OCRStats(dir:File,numElections:Int,errRate:String) {
  dir.mkdirs()
  private val perBallotFile = new StatsFile(new File(dir,"PerBallot.csv"))
  private val perElectionFile = new StatsFile(new File(dir,"PerElection.csv"))
  private val perBallotData = new OCRPerBallotData
  private val perElectionData = new OCRPerElectionData
  private val firstLine = Array("number of election simulations = "+numElections+"  error rate = "+errRate)
  perBallotFile.write(firstLine)
  perElectionFile.write(firstLine)
  perBallotFile.write(perBallotData.headings)
  perElectionFile.write(perElectionData.headings)
  def add(stat:OCRVoteStatistic) : Unit = { perBallotData.add(stat); perElectionData.add(stat) }
  def endElection(): Unit = {
    perElectionFile.write(perElectionData.data)
    perElectionData.clear()
    perBallotData.endElection()
  }
  def close(): Unit = {
    perElectionFile.close()
    for (i<-0 until perBallotData.length) {
      perBallotFile.write(perBallotData.data(i))
    }
    perBallotFile.close()
  }
}