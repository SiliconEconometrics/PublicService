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
import scala.collection.mutable.ListBuffer


object ElectionReport {
  
  def createCSS(dir:File) {
    IOUtil.copyFile(new File("report.css"),new File(dir,"report.css"))
  }
  
  class DetailedReportHelper(data:ElectionData,report:ElectionResultReport,val useStartCounts:Boolean,val useDistributed:Boolean,val useTransferred:Boolean,val useSetAside:Boolean,val usePapers:Boolean,val usesNames:Boolean,val usesCount:Boolean) {
    val candidates = data.candidates
    // val usePapers = 
    
    def heading =  
      <tr class="Head">{if (usesNames) <td>Group</td><td>Candidate</td>}{if (usesCount) <td>Count</td>}{if (useStartCounts) <td>Start Votes</td>} {if (useStartCounts&&usePapers) <td>Start Papers</td>}{if (useDistributed) <td>Distributed</td>}{if (useTransferred) <td>Transferred</td>} {if (useTransferred&&usePapers) <td>Transferred Papers</td>}{if (useSetAside) <td>Set Aside</td>}{if (usePapers) <td>Papers</td>}<td>Progressive Total</td></tr>

    var lastgroup = "***"
    
    def bodyRow(step:ElectionCountReport,countNumber:Int,/** Candidate number */ i:Int) = {
               val c = candidates(i)
               def isElected = if (step.end.electedCandidates.contains(i)) Some("Elected") else None
               def isExcluded = if (step.start!=null && step.start.excludedCandidates.contains(i) && step.start.votesPerCandidate(i)==0.0) Some("Excluded") else None
               val isExcludedEnd = if (step.end.excludedCandidates.contains(i)) Some("Excluded") else None
               val isIneligible = if (step.end.ineligibleCandidates.contains(i)) Some("Ineligible") else None
               def startOverride : Option[String] = isExcluded orElse isIneligible
               def middleOverride : Option[String] = isElected orElse isExcluded orElse isIneligible
               val endOverride : Option[String] = isExcludedEnd orElse isIneligible
               val r2 = <tr class="Striped">{if (usesNames) <td></td><td>{c.name}</td>}{if (usesCount) <td>{countNumber}</td>}{if (useStartCounts) <td>{startOverride getOrElse step.string_totalAtStart(i)}</td>} {if (useStartCounts&&usePapers) <td class="papers">{step.string_papersAtStart(i)}</td>}{if (useDistributed) <td>{middleOverride getOrElse step.string_ballotPapersDistributed(i)}</td>}{if (useTransferred) <td>{middleOverride getOrElse step.string_ballotPapersTransferred(i)}</td>}{if (useTransferred&&usePapers) <td class="papers">{step.string_papersTransferred(i)}</td>}{if (useSetAside) <td>{middleOverride getOrElse step.string_ballotPapersSetAside(i)}</td>}{if (usePapers) <td class="papers" title={step.string_papersAtEndATL(i)+" ATL, "+step.string_papersAtEndBTL(i)+" BTL"}>{step.string_papersAtEnd(i)}</td>}<td>{endOverride getOrElse step.string_totalAtEnd(i)}</td></tr>
               if (c.group!=lastgroup && usesNames) {
                 lastgroup=c.group
                 val r1 = <tr class="Group"><td>{c.group}</td><td class="groupname">{data.groupNameFromID(c.group)}</td>{if (useStartCounts) <td></td>}{if (useStartCounts&&usePapers) <td></td>}{if (useDistributed) <td></td>}{if (useTransferred) <td></td>}{if (useTransferred&&usePapers) <td></td>}{if (useSetAside) <td></td>}{if (usePapers) <td></td>}<td></td></tr>
                 r1++r2
               } else r2 
     }
  }
  
  def generateReportForASingleCount(step:ElectionCountReport,number:Int,data:ElectionData,report:ElectionResultReport) = {
    val candidates = data.candidates
    val t = step.countType
    val helper = new DetailedReportHelper(data,report,t.useStartCounts,t.useDistributed,t.useTransferred,t.useSetAside,step.usePapers,true,false)
    val usePapers = helper.usePapers
    <html>
      <head>
        <meta charset="UTF-8"/> 
        <title>Count {number}</title>
        <link href="report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <p>{step.heading(candidates)}</p>
        {for (e<-step.electedCandidates) yield <p>{candidates(e).name} elected</p>}
        {for (s<-step.equalCandidatesDisabiguatedByEC) yield <p>Election Commission needed to choose among equal candidates {for (c<-s.toList.sorted) yield candidates(c).name+" ("+c+") "} </p>}        
        <table class="Display">
           { helper.heading }
           {
             for (i<-0 until candidates.length) yield helper.bodyRow(step,number, i)
           }
           {
             <tr class="Exhausted"><td></td><td>Exhausted</td>{if (t.useStartCounts) <td>{step.string_exhaustedAtStart}</td>}{if (t.useStartCounts&&usePapers) <td class="papers">{step.string_exhaustedPapersAtStart}</td>}{if (t.useDistributed) <td>{step.string_exhaustedDistributed}</td>}{if (t.useTransferred) <td>{step.string_exhaustedTransferred}</td>}{if (t.useTransferred&&usePapers) <td class="papers">{step.string_exhaustedPapersTransferred}</td>}{if (t.useSetAside) <td>{step.string_exhaustedSetAside}</td>}{if (usePapers) <td class="papers">{step.string_exhaustedPapersAtEnd}</td>}<td>{step.string_exhaustedAtEnd}</td></tr>
           }
           { if (step.useRounding) {
             <tr class="Rounding"><td></td><td>Rounding</td>{if (t.useStartCounts) <td>{step.string_roundingAtStart}</td>}{if (t.useStartCounts&&usePapers) <td></td>}{if (t.useDistributed) <td></td>}{if (t.useTransferred) <td>{step.string_roundingTransferred}</td>}{if (t.useTransferred&&usePapers) <td></td>}{if (t.useSetAside) <td></td>}{if (usePapers) <td></td>}<td>{step.string_roundingAtEnd}</td></tr>          
           }}
           {
             if (step.countType.setAsidePrior>0) <tr class="SetAside"><td></td><td>Set Aside (previous counts)</td>{if (t.useStartCounts) <td></td>}{if (t.useStartCounts&&usePapers) <td></td>}{if (t.useDistributed) <td></td>}{if (t.useTransferred) <td></td>}{if (t.useTransferred&&usePapers) <td></td>}{if (t.useSetAside) <td>{step.string_setAsidePrior}</td>}{if (usePapers) <td></td>}<td></td></tr>
           }
        </table>
      </body>
    </html>
  }
  
  def generateReportForASingleIndividual(candidateID:Int,data:ElectionData,report:ElectionResultReport) = {
    def existsT(f:CountReportType=>Boolean) : Boolean = report.history.exists { step => f(step.countType) }
    
    val candidate = data.candidates(candidateID)
    val helper = new DetailedReportHelper(data,report,existsT(_.useStartCounts),existsT(_.useDistributed),existsT(_.useTransferred),existsT(_.useSetAside),report.history.exists { _.usePapers },false,true)
    val usePapers = helper.usePapers
    <html>
      <head>
        <meta charset="UTF-8"/> 
        <title>{candidate.name}</title>
        <link href="report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <p>Summary report for {candidate.name}</p>
        <table class="Display">
           { helper.heading }
           {
             for (stepID<-0 until report.history.length) yield helper.bodyRow(report.history(stepID),stepID+1, candidateID)
           }
        </table>
      </body>
    </html>
  }
  

  
  def generateMarginReport(data:ElectionData,report:ElectionResultReport,requireTamperable:Boolean) = {
    val candidates = data.candidates
    val usesResults = requireTamperable && report.marginsTamperableResult.exists { _.isDefined }
    def table(justShowElected:Boolean)= {
         <table class="Display">
           <tr class="Head"><td>Group</td><td>Candidate</td><td>Margin votes</td><td>Margin papers</td><td>Counting Step</td><td>Vote Recipients</td>{if (usesResults) <td>Effects</td>}</tr>
           {
             var lastgroup = "***"
             val margins = report.margins(requireTamperable) 
             for (i<-0 until candidates.length) yield {
               val margin = margins(i)
               val c = candidates(i)
               val elected = report.electedCandidates.contains(i)
               if (elected || !justShowElected) {
                 val r2 = <tr class="Striped"><td></td><td>{c.name}</td><td>{margin.map{_.votes}.getOrElse("")}</td><td>{margin.map{_.papers}.getOrElse("")}</td><td>{margin.map{_.step}.getOrElse("")}</td><td>{margin.map{_.how(data.candidates)}.getOrElse("")}</td>{if (usesResults) <td>{report.marginsTamperableResult(i).map{_.desc(candidates)}.getOrElse("")}</td>}</tr>
                 if (c.group!=lastgroup) {
                   lastgroup=c.group
                   val r1 = <tr class="Group"><td>{c.group}</td><td class="groupname">{data.groupNameFromID(c.group)}</td><td/><td/><td/><td/>{if (usesResults) <td/>}</tr>
                   r1++r2
                 } else r2      
               }
             }
           }
         </table>
    }
    <html>
      <head>
        <meta charset="UTF-8"/> 
        <title>Margins</title>
        <link href="report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <p>Simple Margins - the number of fewer votes for candidate X that would make X be excluded earlier than actual, all else being unchanged. Candidates elected before any exclusions not included. Of course there may be more complex ways to achieve the same result with fewer changes by altering the order of eliminations.</p>
        <p>First preferences may {if (requireTamperable) "NOT" else ""} be changed. Papers is number of ballots that are changed; votes is after taking transfer values into account, and may be slightly inaccurate due to rounding.</p>
        <p><em>Just elected</em></p> {table(true)} <p><em>All</em></p> {table(false)}
      </body>
    </html>
  }
  
  def generateOverallReport(result:ElectionResultReport,candidates:Array[Candidate]) = {
    <html>
      <head>
        <title>Election Process</title>
        <link href="report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <p>Quota : {result.quota}</p>
        {if (result.numStochastic>0) <p> {result.numStochastic.toString+" Stochastic runs."} {if (result.isStochasticIgnoreWhoIsEliminatedForMergingIntoStochasticReport) " Note that different elimination orders are collated here; the list below is typical." } </p>}
        <table class="Display">
           <tr class="Head"><td>Count number</td><td>Candidates elected</td><td>Candidates Distributed</td><td>Candidates Excluded</td></tr>
           { 
             val distinctLines = new ArrayBuffer[(ElectionCountReport,List[Int])]
             val lastCounts = new ListBuffer[Int]
             var lastLineDef : List[Int] = null
             var lastReport : ElectionCountReport = null
             def emitRecord() {
               distinctLines+= ((lastReport,lastCounts.toList))
               lastReport=null; lastLineDef=null; lastCounts.clear()
             }
             for ((step,count)<-result.history.zipWithIndex) {
               val linedef : List[Int] = step.electedCandidates.toList++List(-1,step.countType.candidateDistributed)++step.countType.candidatesEliminated
               if (linedef!=lastLineDef && lastLineDef!=null) emitRecord()
               lastLineDef = linedef
               lastReport=step
               lastCounts+= count+1 // +1 makes it 1 based rather than 0 based - humans like counting from 1.
             }
             emitRecord()
             for ((step,counts)<-distinctLines) yield 
               <tr class="Striped">
                 <td>
                   { for (count<-counts) yield <span><a href={"count"+(count)+".html"}>{count}</a> </span> }
                 </td>
                 <td>
                   {for (c<-step.electedCandidates) yield <p>{candidates(c).name}</p>}
                 </td>
                 <td>{val c=step.countType.candidateDistributed;if (c!= -1) candidates(c).name}</td>
                 <td> {for (c<-step.countType.candidatesEliminated) yield <p>{candidates(c).name}</p>}</td>
               </tr>
           }
        </table>
        {if (result.hasMarginInfo) <p>Margins <a href="marginsAllow1PrefsChanges.html">allowing</a> and <a href="marginsNo1Prefs.html">not allowing</a> first preference changes</p>}
      </body>
    </html>
  }
 
  def saveReports(dir:File,result:ElectionResultReport,data:ElectionData) {
    dir.mkdirs()
    createCSS(dir)
    val overall = generateOverallReport(result,result.candidates)
    scala.xml.XML.save(new File(dir,"About.html").toString, overall)
    for ((step,count)<-result.possiblyStochasticHistory.zipWithIndex) {
      val humanCount = count+1
      val xml = generateReportForASingleCount(step,humanCount,data,result)
      scala.xml.XML.save(new File(dir,"count"+humanCount+".html").toString, xml,"UTF-8")
    }
    for (candidateID<-0 until data.numCandidates) {
      val xml = generateReportForASingleIndividual(candidateID,data,result)
      scala.xml.XML.save(new File(dir,"candidate "+data.candidates(candidateID).name+".html").toString, xml,"UTF-8")
    }
    if (result.hasMarginInfo) {
      scala.xml.XML.save(new File(dir,"marginsAllow1PrefsChanges.html").toString, generateMarginReport(data,result,false),"UTF-8")      
      scala.xml.XML.save(new File(dir,"marginsNo1Prefs.html").toString, generateMarginReport(data,result,true),"UTF-8")      
    }
  }
  def blankIfZero(num:Double) = if (num==0) "" else stringOfVoteCount(num)
  def stringOfVoteCount(num:Double) = {
    val eps = 1e-8
    val intv = (num+eps*num.signum).toInt
    if ((intv-num).abs<eps) intv.toString else num.toString
  }

}

class ElectionProgressiveTotals(val excludedCandidates:Set[Int],val electedCandidates:Set[Int],val ineligibleCandidates:Set[Int],val exhausedVotes:Double,val exhaustedPapers:Double,val votesSetAside:Double,val lostDueToRounding:Double,tallys:Int=>Double,papers:Option[Int=>Int],papersATL:Option[Int=>Int],numCandidates:Int) {
  val votesPerCandidate : Array[Double] = Array.tabulate(numCandidates)(tallys)
  val papersPerCandidate : Option[Array[Int]] = papers.map{p=>Array.tabulate(numCandidates)(p)}
  val papersPerCandidateATL : Option[Array[Int]] = papersATL.map{p=>Array.tabulate(numCandidates)(p)}
  def continuingCandidates : Set[Int] = ((0 until numCandidates).toSet--excludedCandidates)--electedCandidates
}

 /** historical information about a particular count step */
class ElectionCountReport(val numCandidates:Int,val countType:CountReportType) {
  var start : ElectionProgressiveTotals = _
  var end : ElectionProgressiveTotals = _
  def usePapers : Boolean = if (end!=null) end.papersPerCandidate.isDefined else if (start!=null) start.papersPerCandidate.isDefined else false
  def roundingAtStart = if (start!=null) start.lostDueToRounding else 0.0
  def roundingAtEnd = if (end!=null) end.lostDueToRounding else 0.0
  def roundingTransferred = roundingAtEnd-roundingAtStart
  def useRounding : Boolean = roundingAtEnd>0
  def exhaustedPapersAtStart = if (start!=null) start.exhaustedPapers else 0.0
  def exhaustedPapersAtEnd = if (end!=null) end.exhaustedPapers else 0.0
  def exhaustedPapersTransferred = exhaustedPapersAtEnd-exhaustedPapersAtStart
  def totalAtStart(c:Int) = if (start==null) 0 else start.votesPerCandidate(c)
  def totalAtEnd(c:Int) = if (end==null) 0 else end.votesPerCandidate(c)
  def papersAtStart(c:Int) = if (start==null || start.papersPerCandidate.isEmpty) 0 else start.papersPerCandidate.get(c)
  def papersAtEnd(c:Int) = if (end==null || end.papersPerCandidate.isEmpty) 0 else end.papersPerCandidate.get(c)
  def papersAtEndATL(c:Int) = if (end==null || end.papersPerCandidateATL.isEmpty) 0 else end.papersPerCandidateATL.get(c)
  def papersTransferred(c:Int) = papersAtEnd(c)-papersAtStart(c)
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
  def string_exhaustedPapersAtStart : String = stringOfVoteCount(exhaustedPapersAtStart)
  def string_exhaustedPapersAtEnd : String = stringOfVoteCount(exhaustedPapersAtEnd)
  def string_exhaustedPapersTransferred = blankIfZero(exhaustedPapersTransferred)
  def string_exhaustedDistributed = blankIfZero(countType.numExhaustedDistributed)
  def string_exhaustedTransferred = blankIfZero(countType.numExhaustedThatWouldBeCarriedOn)
  def string_exhaustedSetAside = blankIfZero(countType.numExhaustedSetAside)
  def string_roundingAtStart : String = stringOfVoteCount(roundingAtStart)
  def string_roundingAtEnd : String = stringOfVoteCount(roundingAtEnd)
  def string_roundingTransferred = blankIfZero(roundingTransferred)
  def string_setAsidePrior = stringOfVoteCount(countType.setAsidePrior)
  def string_papersAtEnd(c:Int) : String = stringOfVoteCount(papersAtEnd(c))
  def string_papersAtEndATL(c:Int) : String = papersAtEndATL(c).toString
  def string_papersAtEndBTL(c:Int) : String = (papersAtEnd(c)-papersAtEndATL(c)).toString
  def string_papersAtStart(c:Int) : String = stringOfVoteCount(papersAtStart(c))
  def string_papersTransferred(c:Int) : String = stringOfVoteCount(papersTransferred(c))
  
  var equalCandidatesDisabiguatedByEC : Set[Set[Int]] = Set.empty
  def addECDecision(equalCandidates:Set[Int]) {
    equalCandidatesDisabiguatedByEC+=equalCandidates
  }

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
  def candidatesEliminated : List[Int] = List.empty
  def numExhaustedThatWouldBeCarriedOn = 0.0
  def numExhaustedSetAside = 0.0
  def setAsidePrior = 0.0
  def numExhaustedDistributed = numExhaustedThatWouldBeCarriedOn+numExhaustedSetAside
  /** A string that defines the structure... if two values have the same value of this, then the same people do the same things, but the counts may be different */
  def structureDesc(candidates:Array[Candidate],ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) : String
}
class CountReportTypeElimination(candidateEliminated:Int,votes:Double,val margin:Double) extends CountReportType("Eliminated",true,false,true,false) {
   def heading(candidates:Array[Candidate]) = "Eliminated candidate "+candidates(candidateEliminated).name+" with "+ElectionReport.stringOfVoteCount(votes)+" votes losing by a margin of "+ElectionReport.stringOfVoteCount(margin)
   override def candidatesEliminated = List(candidateEliminated)
   override def structureDesc(candidates:Array[Candidate],ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) = "Eliminated candidate "+(if (ignoreWhoIsEliminatedForMergingIntoStochasticReport) "" else candidates(candidateEliminated).name)
}
class CountReportTypeEliminationSet(override val candidatesEliminated:List[Int],fromCounts:List[Int],transferValue:Double) extends CountReportType("Eliminated",true,false,true,false) {
   def heading(candidates:Array[Candidate]) = "Eliminated "+candidatesEliminated.map{candidates(_).name}.mkString(",")+" transfer value "+transferValue+" with votes from counts "+fromCounts.mkString(",")
   override def structureDesc(candidates:Array[Candidate],ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) = "Eliminated candidate "+(if (ignoreWhoIsEliminatedForMergingIntoStochasticReport) "" else candidatesEliminated.map{candidates(_).name}.mkString(","))
}

class CountReportTypeExcessDistribution(override val candidateDistributed:Int,val votes:Double,totalTransferred:Double,transferValue:Double,val numVotesToBeRedistributed:Double,override val numExhaustedSetAside:Double,override val numExhaustedThatWouldBeCarriedOn:Double,override val setAsidePrior:Double,distributedMakesSense:Boolean) extends CountReportType("Distributed Excess",true,distributedMakesSense,true,distributedMakesSense) {
   def heading(candidates:Array[Candidate]) = "Distributed "+ElectionReport.stringOfVoteCount(votes)+" excess votes for candidate "+candidates(candidateDistributed).name+", transfer value "+votes+"/("+numVotesToBeRedistributed+"-"+numExhaustedSetAside+")="+transferValue
   override def structureDesc(candidates:Array[Candidate],ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) = "Distributed excess votes for candidate "+candidates(candidateDistributed).name
}
class CountReportTypeFirstCount() extends CountReportType("First Count",false,false,false,false) {
   def heading(candidates:Array[Candidate]) = "First Count"
   override def structureDesc(candidates:Array[Candidate],ignoreWhoIsEliminatedForMergingIntoStochasticReport:Boolean) = "First Count"
}

class TamperedVote(val whoFrom:Int,val whoTo:Int,val numPapers:Int,val numVotes:Int,val src:Option[ActualListOfTamperableVotes]) {
  def desc(candidates:Array[Candidate]) : String = candidates(whoFrom).name+"\u2192"+candidates(whoTo).name+":"+numPapers+(if (numPapers!=numVotes) "("+numVotes+" votes)" else "") // \u2192 is right arrow
}
class Margin(val step:Int,val tamperings:Array[TamperedVote]) {
  val votes = tamperings.map { _.numVotes}.sum
  val papers = tamperings.map {_.numPapers}.sum

  def how(candidates:Array[Candidate]) : String = tamperings.map{_.desc(candidates)}.mkString(" , ")
}
class ElectionChanged(val originalElected:List[Int],val newElected:List[Int]) {
  val newWinners : Set[Int] = newElected.toSet--originalElected
  val newLosers : Set[Int] = originalElected.toSet--newElected
  def desc(candidates:Array[Candidate]) : String = newLosers.toList.map{"-"+candidates(_).name}.mkString(" ")+newWinners.toList.map{"+"+candidates(_).name}.mkString(" ")
}

class ElectionResultReport(val candidates:Array[Candidate],val ineligibleCandidates:Set[Int],val printDebugMessages:Boolean) { 
   def numCandidates = candidates.length
   val history = new ArrayBuffer[ElectionCountReport]
   def currentCount = history.last
   val electedCandidates = new ArrayBuffer[Int]
   val marginsNoRestrictions : Array[Option[Margin]] = Array.fill(candidates.length)(None)
   val marginsTamperable : Array[Option[Margin]] = Array.fill(candidates.length)(None) //margins where you can't change first pref votes below or above line
   val marginsTamperableResult : Array[Option[ElectionChanged]] = Array.fill(candidates.length)(None)
   var excludedCandidates :Set[Int] = Set.empty
   var electedCandidatesSet :Set[Int] = Set.empty
   var progressiveTotalOfExhaustedVotes=0.0
   var progressiveTotalOfExhaustedPapers=0.0
   var progressiveTotalOfLostDueToRounding=0.0
   var progressiveTotalOfSetasideVotes=0.0
   var quota = 0
   def setQuota(actualQuota:Int) { quota=actualQuota }
   def declareElected(candidateID:Int,reason:String) {
    electedCandidates+=candidateID
    currentCount.electedCandidates+=candidateID
    //println("Elected candidate "+candidates(candidateID).name+"   as "+reason)
   }
   def addECDecision(equalCandidates:Set[Int]) {
     if (printDebugMessages) println("Count "+history.length+" EC had to decide among "+(for (c<-equalCandidates.toList.sorted) yield candidates(c).name+" ("+c+") ").mkString(","))
     currentCount.addECDecision(equalCandidates)
   }
   var tallys:Int=>Double=_
   var papers:Option[Int=>Int]=None
   var papersATL:Option[Int=>Int]=None
   /** Must be called before anything else */
   def setTallyFunction(f:Int=>Double) { tallys=f }
   /** May be called to distinguish papers (physical votes) from tallys (which may be weighted) */
   def setPapersFunction(total:Int=>Int,atl:Int=>Int) { papers=Some(total); papersATL=Some(atl) }
   /** May (and should be) called at the end of the counting to release references to functions provided above to save memory */
   def freeReferencesWhenAllDone() { tallys=null; papers=None; papersATL=None }
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

   def margins(requireTamperable:Boolean) = if (requireTamperable) marginsTamperable else marginsNoRestrictions
   
   def addMarginInfo(candidate:Int,recipients:Array[TamperedVote],requireTamperable:Boolean) {
     val margin = new Margin(history.length+1,recipients)
     val shouldOverwrite = margins(requireTamperable)(candidate).map{_.papers>margin.papers}.getOrElse(true)
     if (shouldOverwrite) margins(requireTamperable)(candidate)=Some(margin)
   }
   def hasMarginInfo :Boolean = marginsNoRestrictions.exists { _.isDefined }    
   def addMarginTamperableEffectInfo(candidate:Int,delta:ElectionChanged) { marginsTamperableResult(candidate)=Some(delta) }
   
  def note(s:String)  { 
    // println(s)
  }
  def addExhaustedVotes(count:Double) {
    progressiveTotalOfExhaustedVotes+=count
    // println(s"$count exhausted votes")
  }
  def addExhaustedPapers(count:Double) {
    progressiveTotalOfExhaustedPapers+=count
    // println(s"$count exhausted votes")
  }
  def addLostDueToRounding(count:Double) {
    progressiveTotalOfLostDueToRounding+=count
    // println(s"$count exhausted votes")
  }
  def declareCandidateExcluded(candidateID:Int,margin:Double) {
    //println("Excluded candidate "+candidates(candidateID).name)
    history+=new ElectionCountReport(numCandidates,new CountReportTypeElimination(candidateID,tallys(candidateID),margin)) 
    startCount()
    excludedCandidates+=candidateID
  }
  def declareCandidatesExcluded(candidateIDs:List[Int],fromCounts:List[Int],transferValue:Double) {
    //println("Excluded candidate "+candidates(candidateID).name)
    history+=new ElectionCountReport(numCandidates,new CountReportTypeEliminationSet(candidateIDs,fromCounts,transferValue)) 
    startCount()
    excludedCandidates++=candidateIDs
  }
  def declareCandidateDistributed(candidateID:Int,surplusVotes:Double,totalTransferred:Double,transferValue:Double,distribution:List[VotesToBeTransferred],numVotesToBeRedistributed:Double,numExhaustedSetAside:Double,numExhaustedThatWouldBeCarriedOn:Double,distributedMakesSense:Boolean) {
    // println("Distributed candidate "+candidates(candidateID).name)
    val setAsidePrior = tallys(candidateID)-numVotesToBeRedistributed
    history+=new ElectionCountReport(numCandidates,new CountReportTypeExcessDistribution(candidateID,surplusVotes,totalTransferred,transferValue,numVotesToBeRedistributed,numExhaustedSetAside,numExhaustedThatWouldBeCarriedOn,setAsidePrior,distributedMakesSense)) 
    startCount()
    electedCandidatesSet+=candidateID
    currentCount.setTransferred(distribution)
  }
  def getElectionProgressiveTotals = new ElectionProgressiveTotals(excludedCandidates,electedCandidatesSet,ineligibleCandidates,progressiveTotalOfExhaustedVotes,progressiveTotalOfExhaustedPapers,progressiveTotalOfSetasideVotes,progressiveTotalOfLostDueToRounding,tallys,papers,papersATL,numCandidates) 
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


