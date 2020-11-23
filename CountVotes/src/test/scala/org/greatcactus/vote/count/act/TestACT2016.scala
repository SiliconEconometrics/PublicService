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
package org.greatcactus.vote.count.act

import org.greatcactus.vote.count.FindBaseDir
import org.greatcactus.vote.count.act.parsing.ACTElectionDataLoader
import org.junit.Assert._
import org.junit.Test

/**
  * Test my algorithm against official distribution of preferences.
  */
class TestACT2016 {

  FindBaseDir.findBaseDir()

  def test(year:String,roundTo6DecimalDigits:Boolean,emulateACT2020behaviour:Boolean=false): Unit = {
    val loader = new ACTElectionDataLoader(year)
    for (region<-loader.availableRegions) {
      val data = loader.load(region)
      data.printStatus()
      val officialResults = loader.readOfficialResults(data.meta)
      println("Official results have "+officialResults.counts.length+" counts.")
      val ecDeemedOrder:Seq[Int] = List.empty // WA2017OfficialResults.usedOrders.getOrElse(region.data.meta.electionName.electorate,List.empty)
      val worker = new ACTElectionHelper (data,loader.getSpec(region).numToBeElected,ecDeemedOrder,true,Set.empty,if (roundTo6DecimalDigits) 1000000 else 1,emulateACT2020behaviour)
      worker.run()
      val myreport = worker.report
      for (countNo<-officialResults.counts.indices) {
        val official = officialResults.counts(countNo)
        if (myreport.history.length<=countNo) fail("My report is too short, only "+myreport.history.length+" expecting "+officialResults.counts.length)
        val my = myreport.history(countNo)
        val desc = "Count "+my.prettyCountName // (countNo+1)
        // test candidates
        for (i<-0 until data.numCandidates) {
          val name = desc+" " + data.candidates(i).name
          assertEquals(name,official.delta.tallys(i),my.ballotPapersTransferred(i),1e-9)
          assertEquals(name,official.end.tallys(i),my.totalAtEnd(i),1e-9)
        }
        // test exhausted
        assertEquals(desc,official.delta.exhausted,my.end.exhausedVotes-(if (my.start==null) 0 else my.start.exhausedVotes),1e-9)
        assertEquals(desc,official.end.exhausted,my.end.exhausedVotes,1e-9)
        // test rounding
        assertEquals(desc,official.delta.lossByFraction,my.roundingTransferred,1e-7)
        assertEquals(desc,official.end.lossByFraction,my.roundingAtEnd,1e-7)
        // test who is elected
        val myElected = my.electedCandidates.toArray.map{_._1}
        assertEquals(desc,official.elected.mkString(","),myElected.mkString(","))
      }
      assertEquals(officialResults.counts.length,myreport.history.length);
    }
  }

  // Note that this test still fails, due to some bizarre things Elections ACT did that I haven't worked around yet.
  @Test def test2020(): Unit = test("2020",roundTo6DecimalDigits = true,emulateACT2020behaviour = true) // in 2020, ACT did a variety of bizarre things
  @Test def test2016(): Unit = test("2016",roundTo6DecimalDigits = false)
  @Test def test2012(): Unit = test("2012",roundTo6DecimalDigits = false)
  @Test def test2008(): Unit = test("2008",roundTo6DecimalDigits = false)

}


