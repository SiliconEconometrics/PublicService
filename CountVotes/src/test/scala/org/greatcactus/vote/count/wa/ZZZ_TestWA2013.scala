/*
    Copyright 2015-2017 Silicon Econometrics Pty. Ltd.

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
package org.greatcactus.vote.count.wa

import org.greatcactus.vote.count._
import org.greatcactus.vote.count.wa._
import org.greatcactus.vote.count.MainDataTypes._

import org.junit.Assert._

import org.junit.Test;

/**
 * Test my algorithm against official distribution of preferences
 */
class ZZZ_TestWA2013 {

  def test(region:String,waecDeemedOrder:Seq[CandidateIndex]) {
    val (data,officialResults) = WA2013ElectionData.loadRegionRaw(region)
    data.printStatus()
    val worker = new WAElectionHelper(data,officialResults.vacancies,Map.empty,waecDeemedOrder,true)
    worker.run()
    val myreport = worker.report

    assertEquals("Quota",officialResults.quota,myreport.quota)
    for (countNo<-0 until officialResults.counts.length) {
      val official = officialResults.counts(countNo)
      if (myreport.history.length<=countNo) fail("My report is too short, only "+myreport.history.length+" expecting "+officialResults.counts.length)
      val my = myreport.history(countNo)
      val desc = "Count "+my.prettyCountName // (countNo+1)
      // test candidates
      for (i<-0 until data.numCandidates) {
        val name = desc+" " + data.candidates(i).name
        assertEquals(name,official.papersDelta(i),my.papersTransferred(i))
        assertEquals(name,official.papersCum(i),my.papersAtEnd(i))
        assertEquals(name,official.votesDelta(i),my.ballotPapersTransferred(i).toInt)
        assertEquals(name,official.votesCum(i),my.totalAtEnd(i).toInt)
      }
      // test exhausted
      assertEquals(desc,0,my.exhaustedPapersTransferred.toInt)
      assertEquals(desc,0,(my.end.exhausedVotes-(if (my.start==null) 0 else my.start.exhausedVotes)).toInt)
      assertEquals(desc,0,my.end.exhausedVotes.toInt)
      // test rounding
      assertEquals(desc,official.lostDelta,my.roundingTransferred,1e-7)
      assertEquals(desc,official.lostCum,my.roundingAtEnd,1e-7)
    }
    assertEquals("My report is too long",officialResults.counts.length,myreport.history.length)
  }
  
  @Test def testAgri() { test("Agri",Nil) }
  @Test def testEmet() { test("Emet",Nil) }
  @Test def testNmet() { test("Nmet",Nil) }
  @Test def testSmet() { test("Smet",Nil) }
  @Test def testSthWest() { test("SthWest",Nil) }
  @Test def testMinPas() { test("MinPas",Nil) }
/*
	@Test def testNT() { test("NT",2,DeducedAEC2013TicketSplits.nt,DeducedAEC2013Orders.nt) }
	@Test def testVIC() { test("VIC",6,DeducedAEC2013TicketSplits.vic,DeducedAEC2013Orders.vic) }
  @Test def testNSW() { test("NSW",6,DeducedAEC2013TicketSplits.nsw,DeducedAEC2013Orders.nsw) }
  @Test def testACT() { test("ACT",2,DeducedAEC2013TicketSplits.act,DeducedAEC2013Orders.act) }
  @Test def testTAS() { test("TAS",6,DeducedAEC2013TicketSplits.tas,DeducedAEC2013Orders.tas) }
  @Test def testSA() { test("SA",6,DeducedAEC2013TicketSplits.sa,DeducedAEC2013Orders.sa) } // IMPORTANT NOTE! THIS PRODUCES SLIGHTLY DIFFERENT RESULTS TO OFFICIAL.
  @Test def testWA() { test("WA",6,DeducedAEC2013TicketSplits.wa,DeducedAEC2013Orders.wa) }
  @Test def testQLD() { test("QLD",6,DeducedAEC2013TicketSplits.qld,DeducedAEC2013Orders.qld) }
  
  
  @Test def testSAexhaustion() {
    val data = FederalElectionData.load2013("SA")
    val votes : Array[Vote] = data.makeVotes(DeducedAEC2013TicketSplits.sa)
    val dvotes : Array[DVote] = for (v<-votes) yield new DVote(0,v.numVoters,v.preferences,v.src)
    val continuing = Set(2,14,68)
    val redvotes = dvotes.map{_.skipNotContinuingCandidates(continuing)}
    val exhausted = redvotes.filter { _.isExhausted }
    val numexhausted = exhausted.map{_.numVoters}.sum
    println("Numexhausted = "+numexhausted)
  }*/
}
