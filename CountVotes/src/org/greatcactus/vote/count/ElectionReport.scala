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

import scala.xml._
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import java.io.File

/**
 * @author Andrew
 */
object ElectionReport {
  
  def generateReportForASingleCount(step:ElectionCountReport,number:Int,candidates:Array[Candidate]) = {
    val t = step.countType
    <html>
      <head>
        <title>Count {number}</title>
        <link href="../report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <p>{t.heading(candidates)}</p>
        {for (e<-step.electedCandidates) yield <p>{candidates(e).name} elected</p>}
        <table class="Display">
           <tr class="Head"><td>Group</td><td>Candidate</td>{if (t.useStartCounts) <td>Start Votes</td>}{if (t.useDistributed) <td>Distributed</td>}{if (t.useTransferred) <td>Transferred</td>}{if (t.useSetAside) <td>Set Aside</td>}<td>Progressive Total</td></tr>
           {
             var lastgroup = "***"
             for (i<-0 until candidates.length) yield {
               val c = candidates(i)
               val r2 = <tr class="Striped"><td></td><td>{c.name}</td>{if (t.useStartCounts) <td>{step.totalAtStart(i)}</td>}{if (t.useDistributed) <td>{step.ballotPapersDistributed(i)}</td>}{if (t.useTransferred) <td>{step.ballotPapersTransferred(i)}</td>}{if (t.useSetAside) <td>{step.ballotPapersSetAside(i)}</td>}<td>{step.totalAtEnd(i)}</td></tr>
               if (c.group!=lastgroup) {
                 lastgroup=c.group
                 val r1 = <tr class="Group"><td>{c.group}</td><td></td>{if (t.useStartCounts) <td></td>}{if (t.useDistributed) <td></td>}{if (t.useTransferred) <td></td>}{if (t.useSetAside) <td></td>}<td></td></tr>
                 r1++r2
               } else r2 
             }
           }
        </table>
      </body>
    </html>
  }
  
  def generateOverallReport(result:ElectionResultReport,candidates:Array[Candidate],quota:Int) = {
    <html>
      <head>
        <title>Election Process</title>
        <link href="../report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <p>Quota : {quota}</p>
        <table class="Display">
           <tr class="Head"><td>Count number</td><td>Candidates elected</td><td>Canndidates Distributed</td><td>Candidates Excluded</td></tr>
           { for ((step,count)<-result.history.zipWithIndex) yield 
               <tr class="Striped">
                 <td><a href={"count"+(count+1)+".html"}>{count+1}</a></td>
                 <td>
                   {for (c<-step.electedCandidates) yield <p>{candidates(c).name}</p>}
                 </td>
                 <td>{val c=step.countType.candidateDistributed;if (c!= -1) candidates(c).name}</td>
                 <td>{val c=step.countType.candidateEliminated;if (c!= -1) candidates(c).name}</td>
               </tr>
           }
        </table>
      </body>
    </html>
  }
 
  val dir = "/Users/Andrew/Desktop/Election Report/"
  def saveReports(result:ElectionResultReport,candidates:Array[Candidate],quota:Int) {
    (new File(dir)).mkdirs()
    val overall = generateOverallReport(result,candidates,quota)
    scala.xml.XML.save(dir+"About.html", overall)
    for ((step,count)<-result.history.zipWithIndex) {
      val humanCount = count+1
      val xml = generateReportForASingleCount(step,humanCount,candidates)
      scala.xml.XML.save(dir+"count"+humanCount+".html", xml)
    }
  }
}

 /** historical information about a particular count step */
class ElectionCountReport(numCandidates:Int,val countType:CountReportType) {
  val totalAtStart = new Array[Int](numCandidates)
  val totalAtEnd = new Array[Int](numCandidates)
  def ballotPapersTransferred(candidate:Int) = totalAtEnd(candidate)-totalAtStart(candidate) 
  val ballotPapersDistributed = new Array[Int](numCandidates)
  def ballotPapersSetAside(candidate:Int) = ballotPapersDistributed(candidate)-ballotPapersTransferred(candidate)
  def setCountsStart(tallys:Array[CandidateTally]) { for (c<-tallys) totalAtStart(c.candidateID)=c.numVotes }
  def setCountsEnd(tallys:Array[CandidateTally]) { for (c<-tallys) totalAtEnd(c.candidateID)=c.numVotes }
  var exhaustedVotesStart = 0
  var exhaustedVotesEnd = 0
  val electedCandidates = new ArrayBuffer[Int]
  def setTransferred(distribution:List[VotesToBeTransferred]) { for (v<-distribution) ballotPapersDistributed(v.candidateID)=v.originalNumVotes}
}

abstract class CountReportType(val name:String,val useStartCounts:Boolean,val useDistributed:Boolean,val useTransferred:Boolean,val useSetAside:Boolean) {
  def heading(candidates:Array[Candidate]):String
  def candidateDistributed:Int= -1
  def candidateEliminated:Int = -1
}
class CountReportTypeElimination(override val candidateEliminated:Int,votes:Int,val margin:Int) extends CountReportType("Eliminated",true,false,true,false) {
   def heading(candidates:Array[Candidate]) = "Eliminated candidate "+candidates(candidateEliminated).name+" with "+votes+" votes losing by a margin of "+margin
}
class CountReportTypeExcessDistribution(override val candidateDistributed:Int,votes:Int,totalTransferred:Int,transferValue:Double) extends CountReportType("Distributed Excess",true,true,true,true) {
   def heading(candidates:Array[Candidate]) = "Distributed excess votes for candidate "+candidates(candidateDistributed).name+" with "+votes+" surplus votes of which "+totalTransferred+" were transferred, transfer value="+transferValue
}
class CountReportTypeFirstCount() extends CountReportType("First Count",false,false,false,false) {
   def heading(candidates:Array[Candidate]) = ""
}




class ElectionResultReport(candidates:Array[Candidate],tallys:Array[CandidateTally]) {
   def numCandidates = candidates.length
   val history = new ArrayBuffer[ElectionCountReport]
   def currentCount = history.last
   val electedCandidates = new ArrayBuffer[Int]
   var countCount =1
   var progressiveTotalOfExhaustedVotes=0
   def declareElected(candidateID:Int,reason:String) {
    electedCandidates+=candidateID
    currentCount.electedCandidates+=candidateID
    // println("Elected candidate "+candidates(candidateID).name+"   as "+reason)
  }
  def note(s:String)  { 
    // println(s)
  }
  def addExhaustedVotes(count:Int) {
    progressiveTotalOfExhaustedVotes+=count
    // println(s"$count exhausted votes")
  }
  def declareCandidateExcluded(candidateID:Int,margin:Int) {
    // println("Excluded candidate "+candidates(candidateID).name)
    history+=new ElectionCountReport(numCandidates,new CountReportTypeElimination(candidateID,tallys(candidateID).numVotes,margin)) 
    startCount()
  }
  def declareCandidateDistributed(candidateID:Int,surplusVotes:Int,totalTransferred:Int,transferValue:Double,distribution:List[VotesToBeTransferred]) {
    // println("Distributed candidate "+candidates(candidateID).name)
    history+=new ElectionCountReport(numCandidates,new CountReportTypeExcessDistribution(candidateID,surplusVotes,totalTransferred,transferValue)) 
    startCount()
    currentCount.setTransferred(distribution)
  }
  def finishCount() {
    currentCount.setCountsEnd(tallys)
  }
  def startCount() {
    currentCount.setCountsStart(tallys)
  }
  def initialCountDone() {
    history+=new ElectionCountReport(numCandidates,new CountReportTypeFirstCount)
    finishCount()
  }
  def lastMargin = currentCount.countType.asInstanceOf[CountReportTypeElimination].margin
}
