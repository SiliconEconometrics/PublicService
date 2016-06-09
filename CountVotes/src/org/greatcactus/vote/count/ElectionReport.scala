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

import scala.xml._
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import java.io.File
import java.io.FileOutputStream


object ElectionReport {
  
  def createCSS(dir:File) {
    IOUtil.copyFile(new File("report.css"),new File(dir,"report.css"))
  }
  
  def generateReportForASingleCount(step:ElectionCountReport,number:Int,candidates:Array[Candidate]) = {
    val t = step.countType
    <html>
      <head>
        <meta charset="UTF-8"/> 
        <title>Count {number}</title>
        <link href="report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <p>{step.heading(candidates)}</p>
        {for (e<-step.electedCandidates) yield <p>{candidates(e).name} elected</p>}
        <table class="Display">
           <tr class="Head"><td>Group</td><td>Candidate</td>{if (t.useStartCounts) <td>Start Votes</td>}{if (t.useDistributed) <td>Distributed</td>}{if (t.useTransferred) <td>Transferred</td>}{if (t.useSetAside) <td>Set Aside</td>}<td>Progressive Total</td></tr>
           {
             var lastgroup = "***"
             for (i<-0 until candidates.length) yield {
               val c = candidates(i)
               def isElected = if (step.end.electedCandidates.contains(i)) Some("Elected") else None
               def isExcluded = if (step.start.excludedCandidates.contains(i)) Some("Excluded") else None
               val isExcludedEnd = if (step.end.excludedCandidates.contains(i)) Some("Excluded") else None
               val isIneligible = if (step.end.ineligibleCandidates.contains(i)) Some("Ineligible") else None
               def startOverride : Option[String] = isExcluded orElse isIneligible
               def middleOverride : Option[String] = isElected orElse isExcluded orElse isIneligible
               val endOverride : Option[String] = isExcludedEnd orElse isIneligible
               val r2 = <tr class="Striped"><td></td><td>{c.name}</td>{if (t.useStartCounts) <td>{startOverride getOrElse step.string_totalAtStart(i)}</td>}{if (t.useDistributed) <td>{middleOverride getOrElse step.string_ballotPapersDistributed(i)}</td>}{if (t.useTransferred) <td>{middleOverride getOrElse step.string_ballotPapersTransferred(i)}</td>}{if (t.useSetAside) <td>{middleOverride getOrElse step.string_ballotPapersSetAside(i)}</td>}<td>{endOverride getOrElse step.string_totalAtEnd(i)}</td></tr>
               if (c.group!=lastgroup) {
                 lastgroup=c.group
                 val r1 = <tr class="Group"><td>{c.group}</td><td></td>{if (t.useStartCounts) <td></td>}{if (t.useDistributed) <td></td>}{if (t.useTransferred) <td></td>}{if (t.useSetAside) <td></td>}<td></td></tr>
                 r1++r2
               } else r2 
             }
           }
           {
             /*
             def blankIfZero(num:Int) = if (num==0) "" else num.toString
             val (dist,trans,quota) = step.countType match {
               case excess:CountReportTypeExcessDistribution => (blankIfZero(excess.numExhaustedSetAside+excess.numExhaustedThatWouldBeCarriedOn),blankIfZero(excess.numExhaustedThatWouldBeCarriedOn),blankIfZero(excess.numExhaustedSetAside))
               case _ => ("","","")
             }*/
             <tr class="Exhausted"><td></td><td>Exhausted</td>{if (t.useStartCounts) <td>{step.string_exhaustedAtStart}</td>}{if (t.useDistributed) <td>{step.string_exhaustedDistributed}</td>}{if (t.useTransferred) <td>{step.string_exhaustedTransferred}</td>}{if (t.useSetAside) <td>{step.string_exhaustedSetAside}</td>}<td>{step.string_exhaustedAtEnd}</td></tr>
           }
           {/*
             val setAsidePrior : Int = step.countType match {
               case excess:CountReportTypeExcessDistribution => step.totalAtStart(excess.candidateDistributed)-excess.numVotesToBeRedistributed
               case _ => 0
             }*/
             if (step.countType.setAsidePrior>0) <tr class="SetAside"><td></td><td>Set Aside (previous counts)</td>{if (t.useStartCounts) <td></td>}{if (t.useDistributed) <td></td>}{if (t.useTransferred) <td></td>}{if (t.useSetAside) <td>{step.string_setAsidePrior}</td>}<td></td></tr>
           }
        </table>
      </body>
    </html>
  }
  
  def generateOverallReport(result:ElectionResultReport,candidates:Array[Candidate],quota:Int) = {
    <html>
      <head>
        <title>Election Process</title>
        <link href="report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <p>Quota : {quota}</p>
        {if (result.numStochastic>0) <p> {result.numStochastic.toString+" Stochastic runs."} {if (result.isStochasticIgnoreWhoIsEliminatedForMergingIntoStochasticReport) " Note that different elimination orders are collated here; the list below is typical." } </p>}
        <table class="Display">
           <tr class="Head"><td>Count number</td><td>Candidates elected</td><td>Candidates Distributed</td><td>Candidates Excluded</td></tr>
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
 
  def saveReports(dir:File,result:ElectionResultReport) {
    dir.mkdirs()
    createCSS(dir)
    val overall = generateOverallReport(result,result.candidates,result.quota)
    scala.xml.XML.save(new File(dir,"About.html").toString, overall)
    for ((step,count)<-result.possiblyStochasticHistory.zipWithIndex) {
      val humanCount = count+1
      val xml = generateReportForASingleCount(step,humanCount,result.candidates)
      scala.xml.XML.save(new File(dir,"count"+humanCount+".html").toString, xml,"UTF-8")
    }
  }
  def blankIfZero(num:Double) = if (num==0) "" else stringOfVoteCount(num)
  def stringOfVoteCount(num:Double) = if (num.toInt==num) num.toInt.toString else num.toString

}

class ElectionProgressiveTotals(val excludedCandidates:Set[Int],val electedCandidates:Set[Int],val ineligibleCandidates:Set[Int],val exhausedVotes:Double,val votesSetAside:Double,tallys:Array[CandidateTally]) {
  val votesPerCandidate : Array[Double] = tallys.map{_.numVotes}
}

 /** historical information about a particular count step */
class ElectionCountReport(val numCandidates:Int,val countType:CountReportType) {
  var start : ElectionProgressiveTotals = _
  var end : ElectionProgressiveTotals = _
  def totalAtStart(c:Int) = if (start==null) 0 else start.votesPerCandidate(c)
  def totalAtEnd(c:Int) = if (end==null) 0 else end.votesPerCandidate(c)
  def ballotPapersTransferred(candidate:Int) = totalAtEnd(candidate)-totalAtStart(candidate) 
  val ballotPapersDistributed = new Array[Double](numCandidates)
  val electedCandidates = new ArrayBuffer[Int] // elected this round
  def ballotPapersSetAside(candidate:Int) = ballotPapersDistributed(candidate)-ballotPapersTransferred(candidate)
  def setCountsStart(getElectionProgressiveTotals:ElectionProgressiveTotals) { start=getElectionProgressiveTotals }
  def setCountsEnd(getElectionProgressiveTotals:ElectionProgressiveTotals) { end=getElectionProgressiveTotals }
  def setTransferred(distribution:List[VotesToBeTransferred]) { for (v<-distribution) ballotPapersDistributed(v.candidateID)=v.originalNumVotes}
  def structureDesc(candidates:Array[Candidate],ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) = countType.structureDesc(candidates,ignoreWhoIsEliminatedForMergingIntoStochasticReport)+electedCandidates.toArray.map{" elected "+candidates(_).name}.mkString(" ")
  // methods that are overridden for stochastic
  def heading(candidates:Array[Candidate]) : String = countType.heading(candidates)
  import ElectionReport._
  def string_totalAtStart(c:Int) : String = stringOfVoteCount(totalAtStart(c))
  def string_totalAtEnd(c:Int) : String = stringOfVoteCount(totalAtEnd(c))
  def string_ballotPapersDistributed(c:Int) : String = stringOfVoteCount(ballotPapersDistributed(c))
  def string_ballotPapersTransferred(c:Int) : String = stringOfVoteCount(ballotPapersTransferred(c))
  def string_ballotPapersSetAside(c:Int) : String = stringOfVoteCount(ballotPapersSetAside(c))
  def string_exhaustedAtStart : String = stringOfVoteCount(start.exhausedVotes)
  def string_exhaustedAtEnd : String = stringOfVoteCount(end.exhausedVotes)
  def string_exhaustedDistributed = blankIfZero(countType.numExhaustedDistributed)
  def string_exhaustedTransferred = blankIfZero(countType.numExhaustedThatWouldBeCarriedOn)
  def string_exhaustedSetAside = blankIfZero(countType.numExhaustedSetAside)
  def string_setAsidePrior = stringOfVoteCount(countType.setAsidePrior)
}

class NumberDistributionPerCandidate(val numCandidates:Int) {
  var n = 0;
  val sum = new Array[Double](numCandidates+2)
  val sumsq = new Array[Double](numCandidates+2)
  val mins = new Array[Double](numCandidates+2)
  val maxs = new Array[Double](numCandidates+2)
  def add(data:Int=>Double,exhausted:Double,setAside:Double) {
    for (i<-0 until numCandidates+2) {
      val v = if (i<numCandidates) data(i) else if (i==numCandidates) exhausted else setAside
      sum(i)+=v
      sumsq(i)+=v*v
      mins(i) = if (n==0) v else mins(i) min v
      maxs(i) = if (n==0) v else maxs(i) max v
    }
    n+=1
  }
  def desc(candidateID:Int) : String = {
    val min = mins(candidateID)
    val max = maxs(candidateID)
    if (min==max) ElectionReport.stringOfVoteCount(min) else {
      val mean = sum(candidateID).toDouble/n
      val variance = (sumsq(candidateID)-n*mean*mean)/(n-1)
      val sd = Math.sqrt(variance)
      val s1 = f"$mean%.1f\u00b1$sd%.1f"
      val s2 = ElectionReport.stringOfVoteCount(min)+"\u2026"+ElectionReport.stringOfVoteCount(max)
      s1+" ("+s2+")"
    }
  }
  def descExhausted = desc(numCandidates)
  def descSetAside = desc(numCandidates+1)
}
class ElectionCountReportStochasticSummary(base:ElectionCountReport,candidates:Array[Candidate],val ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) extends ElectionCountReport(base.numCandidates,base.countType) {
  val structure : String = countType.structureDesc(candidates,ignoreWhoIsEliminatedForMergingIntoStochasticReport)
  var n = 0
  val d_ballotPapersDistributed = new NumberDistributionPerCandidate(numCandidates)
  val d_totalAtStart = new NumberDistributionPerCandidate(numCandidates)
  val d_totalAtEnd = new NumberDistributionPerCandidate(numCandidates)
  val d_ballotPapersSetAside = new NumberDistributionPerCandidate(numCandidates)
  val d_ballotPapersTransferred = new NumberDistributionPerCandidate(numCandidates)
  def addStochastic(rep:ElectionCountReport) {
    n+=1
    d_ballotPapersDistributed.add(rep.ballotPapersDistributed,rep.countType.numExhaustedDistributed,0)
    if (rep.start!=null) d_totalAtStart.add(rep.start.votesPerCandidate,rep.start.exhausedVotes,0)
    d_totalAtEnd.add(rep.end.votesPerCandidate,rep.end.exhausedVotes,0)
    d_ballotPapersSetAside.add(rep.ballotPapersSetAside _,rep.countType.numExhaustedSetAside,rep.countType.setAsidePrior)
    d_ballotPapersTransferred.add(rep.ballotPapersTransferred _,rep.countType.numExhaustedThatWouldBeCarriedOn,0)
  }
  override def heading(candidates:Array[Candidate]) : String = structure + " summary of "+n+" runs."
  override def string_totalAtStart(c:Int) : String = d_totalAtStart.desc(c)
  override def string_totalAtEnd(c:Int) : String = d_totalAtEnd.desc(c)
  override def string_ballotPapersDistributed(c:Int) : String = d_ballotPapersDistributed.desc(c)
  override def string_ballotPapersTransferred(c:Int) : String = d_ballotPapersTransferred.desc(c)
  override def string_ballotPapersSetAside(c:Int) : String = d_ballotPapersSetAside.desc(c)
  override def string_exhaustedAtStart : String = d_totalAtStart.descExhausted
  override def string_exhaustedAtEnd : String = d_totalAtEnd.descExhausted
  def blankIfZeroS(num:String) = if (num=="0") "" else num
  override def string_exhaustedDistributed = blankIfZeroS(d_ballotPapersDistributed.descExhausted)
  override def string_exhaustedTransferred = blankIfZeroS(d_ballotPapersTransferred.descExhausted)
  override def string_exhaustedSetAside = blankIfZeroS(d_ballotPapersSetAside.descExhausted)
  override def string_setAsidePrior = d_ballotPapersSetAside.descSetAside

  {
    this.start = base.start
    this.end = base.end
    this.electedCandidates++= base.electedCandidates
  }
}

abstract class CountReportType(val name:String,val useStartCounts:Boolean,val useDistributed:Boolean,val useTransferred:Boolean,val useSetAside:Boolean) {
  def heading(candidates:Array[Candidate]):String
  def candidateDistributed:Int= -1
  def candidateEliminated:Int = -1
  def numExhaustedThatWouldBeCarriedOn = 0.0
  def numExhaustedSetAside = 0.0
  def setAsidePrior = 0.0
  def numExhaustedDistributed = numExhaustedThatWouldBeCarriedOn+numExhaustedSetAside
  /** A string that defines the structure... if two values have the same value of this, then the same people do the same things, but the counts may be different */
  def structureDesc(candidates:Array[Candidate],ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) : String
}
class CountReportTypeElimination(override val candidateEliminated:Int,votes:Double,val margin:Double) extends CountReportType("Eliminated",true,false,true,false) {
   def heading(candidates:Array[Candidate]) = "Eliminated candidate "+candidates(candidateEliminated).name+" with "+ElectionReport.stringOfVoteCount(votes)+" votes losing by a margin of "+ElectionReport.stringOfVoteCount(margin)
   override def structureDesc(candidates:Array[Candidate],ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) = "Eliminated candidate "+(if (ignoreWhoIsEliminatedForMergingIntoStochasticReport) "" else candidates(candidateEliminated).name)
}
class CountReportTypeExcessDistribution(override val candidateDistributed:Int,val votes:Double,totalTransferred:Double,transferValue:Double,val numVotesToBeRedistributed:Double,override val numExhaustedSetAside:Double,override val numExhaustedThatWouldBeCarriedOn:Double,override val setAsidePrior:Double) extends CountReportType("Distributed Excess",true,true,true,true) {
   def heading(candidates:Array[Candidate]) = "Distributed "+ElectionReport.stringOfVoteCount(votes)+" excess votes for candidate "+candidates(candidateDistributed).name+", transfer value "+votes+"/("+numVotesToBeRedistributed+"-"+numExhaustedSetAside+")="+transferValue
   override def structureDesc(candidates:Array[Candidate],ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) = "Distributed excess votes for candidate "+candidates(candidateDistributed).name
}
class CountReportTypeFirstCount() extends CountReportType("First Count",false,false,false,false) {
   def heading(candidates:Array[Candidate]) = "First Count"
   override def structureDesc(candidates:Array[Candidate],ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) = "First Count"
}



class ElectionResultReport(val candidates:Array[Candidate],tallys:Array[CandidateTally],val ineligibleCandidates:Set[Int]) {
   def numCandidates = candidates.length
   val history = new ArrayBuffer[ElectionCountReport]
   def currentCount = history.last
   val electedCandidates = new ArrayBuffer[Int]
   var excludedCandidates :Set[Int] = Set.empty
   var electedCandidatesSet :Set[Int] = Set.empty
   var countCount =1
   var progressiveTotalOfExhaustedVotes=0.0
   var progressiveTotalOfSetasideVotes=0.0
   var quota = 0
   def setQuota(actualQuota:Int) { quota=actualQuota }
   def declareElected(candidateID:Int,reason:String) {
    electedCandidates+=candidateID
    currentCount.electedCandidates+=candidateID
    //println("Elected candidate "+candidates(candidateID).name+"   as "+reason)
   }
  
   var numStochastic = 0
   val historyStochastic = new ArrayBuffer[ElectionCountReportStochasticSummary]
   def isStochasticIgnoreWhoIsEliminatedForMergingIntoStochasticReport = (!historyStochastic.isEmpty) && historyStochastic.head.ignoreWhoIsEliminatedForMergingIntoStochasticReport
   def stucture(ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) : String = history.map{_.structureDesc(candidates,ignoreWhoIsEliminatedForMergingIntoStochasticReport)}.mkString(";")
   def makeStochastic(ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) {
     historyStochastic++=history.map{new ElectionCountReportStochasticSummary(_,candidates,ignoreWhoIsEliminatedForMergingIntoStochasticReport)} 
   }
   def addStochastic(rep:ElectionResultReport) {
     numStochastic+=1
     assert (history.length==rep.history.length)
     for ((sh,h)<-historyStochastic zip rep.history) sh.addStochastic(h)
   }
   def possiblyStochasticHistory:Array[ElectionCountReport] = if (historyStochastic.isEmpty) history.toArray else historyStochastic.toArray

       
  def note(s:String)  { 
    // println(s)
  }
  def addExhaustedVotes(count:Double) {
    progressiveTotalOfExhaustedVotes+=count
    // println(s"$count exhausted votes")
  }
  def declareCandidateExcluded(candidateID:Int,margin:Double) {
    //println("Excluded candidate "+candidates(candidateID).name)
    history+=new ElectionCountReport(numCandidates,new CountReportTypeElimination(candidateID,tallys(candidateID).numVotes,margin)) 
    startCount()
    excludedCandidates+=candidateID
  }
  def declareCandidateDistributed(candidateID:Int,surplusVotes:Double,totalTransferred:Double,transferValue:Double,distribution:List[VotesToBeTransferred],numVotesToBeRedistributed:Double,numExhaustedSetAside:Double,numExhaustedThatWouldBeCarriedOn:Double) {
    // println("Distributed candidate "+candidates(candidateID).name)
    val setAsidePrior = tallys(candidateID).numVotes-numVotesToBeRedistributed
    history+=new ElectionCountReport(numCandidates,new CountReportTypeExcessDistribution(candidateID,surplusVotes,totalTransferred,transferValue,numVotesToBeRedistributed,numExhaustedSetAside,numExhaustedThatWouldBeCarriedOn,setAsidePrior)) 
    startCount()
    electedCandidatesSet+=candidateID
    currentCount.setTransferred(distribution)
  }
  def getElectionProgressiveTotals = new ElectionProgressiveTotals(excludedCandidates,electedCandidatesSet,ineligibleCandidates,progressiveTotalOfExhaustedVotes,progressiveTotalOfSetasideVotes,tallys) 
  def finishCount() {
    currentCount.setCountsEnd(getElectionProgressiveTotals)
  }
  def startCount() {
    currentCount.setCountsStart(getElectionProgressiveTotals)
  }
  def initialCountDone() {
    history+=new ElectionCountReport(numCandidates,new CountReportTypeFirstCount)
    finishCount()
  }
  
  /** Look at a set of tied candidates to see who most recently had a highest count. Used to break ties. If more than one candidate, find a place where there is a score difference between the desired# candidate's tally and the next highest. Return the chosen candidates if available. */
  def searchRecentHistoryForCandidatesWithHigherCounts(candidatesToTake:Int,tiedCandidates:List[Int]) : Option[Set[Int]] = {
    class CandidateAndScore(val candidate:Int,val tally:Double) {
      override def toString = candidates(candidate).name+":"+tally
    }
    for (round<-(history.size-1) to 0 by -1) {
      val h : ElectionCountReport = history(round)
      val tallys = (for (c<-tiedCandidates) yield new CandidateAndScore(c,h.totalAtEnd(c))).sortBy{_.tally}.reverse // sorted by vote count descending
      //println("Round "+round+" scores "+tallys)
      if (tallys(candidatesToTake-1).tally != tallys(candidatesToTake).tally) {
        //println("searchRecentHistoryForCandidatesWithHigherCounts resolved tie in favor of "+tallys.take(candidatesToTake).map{t=>candidates(t.candidate).name})
        return Some(tallys.take(candidatesToTake).map{_.candidate}.toSet)
      }
    }
    //println("searchRecentHistoryForCandidatesWithHigherCounts did not resolve tie")
    None
  }
  /** Look at a set of tied candidates to see who most recently had a highest count. Used to break ties. If more than one candidate, find a place where there is a score difference between the desired# candidate's tally and the next highest. Return the chosen candidates if available. */
  def searchRecentHistoryForCandidatesWithHigherCountsAndOrder(tiedCandidates:List[Int]) : Option[List[Int]] = {
    class CandidateAndScore(val candidate:Int,val tally:Double)
    for (round<-(history.size-1) to 0 by -1) {
      val h : ElectionCountReport = history(round)
      val tallys = (for (c<-tiedCandidates) yield new CandidateAndScore(c,h.totalAtEnd(c))).sortBy{_.tally}.reverse // sorted by vote count descending
      if (tallys.map{_.tally}.distinct.length == tiedCandidates.length) return Some(tallys.map{_.candidate})
    }
    None
  }

  def lastMargin : Double = currentCount.countType match {
    case elim:CountReportTypeElimination => elim.margin
    case _ => Double.NaN
  }
}


