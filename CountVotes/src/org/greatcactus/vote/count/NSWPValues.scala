/*
    Copyright 2016 Silicon Econometrics Pty. Ltd.

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

    Analyze probability of NSW outcomes
 */

package org.greatcactus.vote.count

import org.jsoup.nodes.Document


class NSWCountSummary(/* Eliminated=-1; Elected = 10^9, otherwise votes */ val tally:Array[Int],val exhausted:Int)
class NSWWholeCountSummary(val counts:Array[NSWCountSummary])

class NumDistribution {
  var seen : Map[Int,Int] = Map.empty // how many times a particular integer was seen
  /** Add an occurrence of i */
  def add(i:Int) { seen+=i->(1+(seen.getOrElse(i,0))); }
  /** See how many values are at least this extreme */
  def extremeness(i:Int) : Int = {
    var above = 0
    var below = 0
    for ((key,value)<-seen) {
      if (key<=i) below+=value
      if (key>=i) above+=value
    }
    above min below
  }
  def extremenessDesc(i:Int) : String = {
    var above = 0
    var below = 0
    var equal = 0
    for ((key,value)<-seen) {
      if (key<i) below+=value
      if (key>i) above+=value
      if (key==i) equal+=value
    }
    i.toString+" has "+below+" below, "+equal+" equal and "+above+" above."
  }
  /** Proportion <=i */
  def lessOrEqual(i:Int) : Double = {
    var above = 0
    var below = 0
    for ((key,value)<-seen) {
      if (key<=i) below+=value
      if (key>i) above+=value // note different to extremeness() function
    }
    below/(above+below).toDouble
  }
}

class CountNumDistribution(numCandidates:Int) {
  var total : Int = 0
  val candidates : Array[NumDistribution] = Array.fill(numCandidates)(new NumDistribution)
  val exhausted = new NumDistribution
  
  def add(sum:NSWCountSummary) {
     for (i<-0 until numCandidates) candidates(i).add(sum.tally(i))
     exhausted.add(sum.exhausted)
     total+=1
  }
  def extremeness(sum:NSWCountSummary) : Int = {
    var res = exhausted.extremeness(sum.exhausted)
    for (i<-0 until numCandidates) res=res min candidates(i).extremeness(sum.tally(i))
    res
  } 
  def printoutDetailedExtremeness(sum:NSWCountSummary,candidateNames:Array[String]) {
    println("   Exhausted : "+sum.exhausted+" => "+exhausted.extremeness(sum.exhausted))
    for (i<-0 until numCandidates) println("  "+candidateNames(i)+" : "+sum.tally(i)+" => "+candidates(i).extremeness(sum.tally(i)))
  }

}
class NSWAllCountsSummary(val numCandidates:Int) {
  val maxRounds = numCandidates*2+1 // extrememely conservative
  var maxrounds = -1
  var total = 0
  val counts = Array.fill(maxRounds){new CountNumDistribution(numCandidates)}
  def add(sum:NSWWholeCountSummary) {
    for (i<-0 until sum.counts.length) counts(i).add(sum.counts(i))
    total+=1
    maxrounds = maxrounds max sum.counts.length
  }
  def extremeness(sum:NSWWholeCountSummary) : Int = {
    var res = total
    for (i<-0 to maxrounds) res=res min {
      if (i<sum.counts.length) counts(i).extremeness(sum.counts(i))
      else total-counts(i).total
    }
    res
  }
  def printoutDetailedExtremeness(sum:NSWWholeCountSummary,candidateNames:Array[String]) {
    for (i<-0 until sum.counts.length) {
      println(" Count "+(i+1)+"  => "+counts(i).extremeness(sum.counts(i)))
      counts(i).printoutDetailedExtremeness(sum.counts(i),candidateNames:Array[String])
    }
    
  }

  def extremenessReport(sum:NSWWholeCountSummary,candidateNames:Array[String],name:String,score:Int,pValue:Double) = {
    <html>
      <head>
        <meta charset="UTF-8"/> 
        <title>Official vs. distribution for {name}</title>
        <link href="report.css" type="text/css" rel="stylesheet"></link>
      </head>
      <body>
        <p>Overall score {score} of {total} P-Value {pValue}</p>
        <table class="Display">
          <tr class="Head"><th>Count</th>{for (i<-0 until sum.counts.length) yield <th>{i+1}</th>}</tr>
          {
            for (c<-0 until candidateNames.length) yield <tr class="Striped"><td>{candidateNames(c)}</td>{for (i<-0 until sum.counts.length) yield <td title={counts(i).candidates(c).extremenessDesc(sum.counts(i).tally(c))}>{counts(i).candidates(c).extremeness(sum.counts(i).tally(c))}</td>}</tr>
          }
          <tr class="Exhausted"><td>Exhausted</td>{for (i<-0 until sum.counts.length) yield <td title={counts(i).exhausted.extremenessDesc(sum.counts(i).exhausted)}>{counts(i).exhausted.extremeness(sum.counts(i).exhausted)}</td>}</tr>
          <tr class="Exhausted"><td>Min</td>{for (i<-0 until sum.counts.length) yield <td>{counts(i).extremeness(sum.counts(i))}</td>}</tr>
        </table>
        <p>Finishing that early : {(total-counts(sum.counts.length).total)}</p>
      </body>
    </html>
  }
  
}

class NSWDistributionSummary(val numCandidates:Int) {
  val extremenessSummary = new NSWAllCountsSummary(numCandidates)
  val extremenessDistribution = new NumDistribution
  val components = new collection.mutable.ArrayBuffer[NSWWholeCountSummary]
  
  /** Biased but efficient method - use same set for generating and evaluating function. Probably a bad idea to use */
  def addBiased(sum:NSWWholeCountSummary) {
    components+=sum
    extremenessSummary.add(sum)
  }
  /** Use a run to generate a function describing extremeness */
  def addJustGenerateFunction(sum:NSWWholeCountSummary) { extremenessSummary.add(sum) }
  /** Use a run to evaluate the distribution of said function */
  def addJustEvaluateFunctionDistribution(sum:NSWWholeCountSummary) { extremenessDistribution.add(extremenessSummary.extremeness(sum)) }
  private def makeDistribution() {
    for (c<-components) extremenessDistribution.add(extremenessSummary.extremeness(c))
    components.clear()
  }
  /** Calculate the extremeness, and a P value for it */
  def extremeness(sum:NSWWholeCountSummary) : (Int,Double) = {
    /** See if extremenessDistribution needs making */
    if (!components.isEmpty) makeDistribution()
    val extremeness = extremenessSummary.extremeness(sum)
    (extremeness,extremenessDistribution.lessOrEqual(extremeness))
  }
  
  def printoutDetailedExtremeness(sum:NSWWholeCountSummary,candidateNames:Array[String]) {
    /** See if extremenessDistribution needs making */
    if (!components.isEmpty) makeDistribution()
    extremenessSummary.printoutDetailedExtremeness(sum,candidateNames)
  }
  def extremenessReport(sum:NSWWholeCountSummary,candidateNames:Array[String],name:String) ={
    val (score,p) = extremeness(sum)
    extremenessSummary.extremenessReport(sum,candidateNames,name,score,p)
  }
}



object NSWWholeCountSummary {
  def ofReport(report:ElectionResultReport) : NSWWholeCountSummary = {
    val res = new NSWWholeCountSummary(report.history.toArray.map{NSWCountSummary.ofReport(_)})
    val no1exhaust = new NSWWholeCountSummary(res.counts.map{c=>new NSWCountSummary(c.tally,c.exhausted-res.counts(0).exhausted)}) // compensate for 2016 NSW LGE not including exhausted in first count votes in subsequent counts.
    no1exhaust
  }
}
object NSWCountSummary {
  val excludedSpecialCode = -1
  val ineligibleSpecialCode = -2
  val electedSpecialCode = 1000000000
  
  def ofReport(step:ElectionCountReport) : NSWCountSummary = {
    val counts = if (step.end!=null) step.end else step.start
    val tallys = counts.votesPerCandidate.map{_.toInt}
//    for (i<-counts.electedCandidates) tallys(i)=electedSpecialCode
    for (i<-counts.excludedCandidates) tallys(i)=excludedSpecialCode
    for (i<-counts.ineligibleCandidates) tallys(i)=ineligibleSpecialCode
    new NSWCountSummary(tallys,counts.exhausedVotes.toInt)
  }
  
  
  import scala.collection.JavaConversions._
  def ofJSoup(doc:Document,candidateNames:Array[String]) : NSWCountSummary = {
    val tally = new collection.mutable.ArrayBuffer[Int]
    var exhausted:Int=0
    val desc = doc.select("h4 a").first().attr("href").substring(4,7)+" : "
    for (table<-doc.select("table")) {
      val rows = table.select("tr").toList
      val headings : Map[String,Int] = Map(rows.head.select("th").toList.map{_.text.trim}.zipWithIndex :_*)
      headings.get("Candidates in Ballot Order") match {
        case None =>
        case Some(candidateCol) =>
         val groupCol = headings.get("Group")
         var groupCount = 0
         val totalCol = headings.get("Progressive Total").orElse(headings.get("Votes")).orElse(headings.get("First Preferences")).getOrElse(throw new Exception("Document has "+headings+" but no total."))
         val transCol = headings.get("Ballot Papers Transferred")
         def parseNum(num:String) : Int = {
           if (num=="EXCLUDED") excludedSpecialCode
           else if (num=="ELECTED") electedSpecialCode
           else if (num=="INELIGIBLE") ineligibleSpecialCode
           else if (num=="") 0
           else num.replace(",","").toInt
         }
         for (row<-rows.tail) {
           val entries : IndexedSeq[String] = row.select("td").toIndexedSeq.map{_.text.replace('\u00a0',' ').trim}
           val num = parseNum(entries(totalCol))+groupCount
           groupCount=0
           val name = entries(candidateCol)
           val groupStart = groupCol.map{entries(_)}.getOrElse("")
           if (groupStart.length>0) { groupCount = num } // group should be carried on to next.
           else if (name=="UNGROUPED CANDIDATES") {  }
           else if (name=="Formal Votes") {}
           else if (name=="Informal Ballot Papers") {}
           else if (name=="Total Votes / Ballot Papers") {}
           else if (name=="Group Total") {}
           else if (name=="TOTAL") {}
           else if (name=="Brought Forward") {exhausted+=num}
           else if (name=="Set Aside this Count") {exhausted+=num}
           else if (name=="Set Aside (previous counts)") {}
           else if (name=="Exhausted") { /* exhausted+=num */} // this value is not actually maintained in subsequent counts.
           else {
             if (candidateNames.length<=tally.length || candidateNames(tally.length)!=name) throw new Exception(desc+"Was not expecting "+name+" starting code "+name.charAt(0).toInt+" was expecting "+(if (candidateNames.length<=tally.length) "an end" else candidateNames(tally.length)+" in row containing "+entries))
             val isElected = transCol.isDefined && entries(transCol.get)=="ELECTED"
             tally+= num // (if (isElected) electedSpecialCode else num)
           }
         }
      }
    }
    if (tally.length!=candidateNames.length) throw new Exception(desc+"Tally.length "+tally.length+" != candidateNames.length "+candidateNames.length+" "+candidateNames.mkString(";"))
    new NSWCountSummary(tally.toArray,exhausted)
  }
}