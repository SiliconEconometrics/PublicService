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

package org.greatcactus.vote.count.ballots

import org.greatcactus.vote.count.MainDataTypes.{NumberOfCandidates, PaperCountUnscaled}


class MeanPosition(val numATL:PaperCountUnscaled,val numBTL:PaperCountUnscaled,val meanPosition: Array[Double])
class MultiMeanPosition(val btls:MeanPosition,val btlsByFirstPreference:Array[MeanPosition],val all:MeanPosition,val allByFirstPreference:Array[MeanPosition])

class MeanPositionBuilder(numCandidates:NumberOfCandidates) {
  var numBTL : PaperCountUnscaled = 0
  var numATL : PaperCountUnscaled = 0
  val sumVotes : Array[Double] = new Array[Double](numCandidates) // sum over each vote of the ordinal number of the vote for that candidate. Blanks count as the mean of the remaining votes.
  def add(v:Vote): Unit = {
    if (v.src.isATL) numATL+=v.numVoters else numBTL+=v.numVoters
    val numBlank = numCandidates-v.preferences.indices.length
    val blankSurrogatePreference = numCandidates-(numBlank-1)/2.0
    val noVoteGivenScore = v.numVoters*blankSurrogatePreference
    for (i<-sumVotes.indices) sumVotes(i)+=noVoteGivenScore // give blank score to everyone, then later remove it from those with a vote.
    for (i<-v.preferences.indices) {
      sumVotes(v.preferences(i))+=v.numVoters*(i+1)-noVoteGivenScore
    }
  }
  def get : MeanPosition = {
    val mul = 1.0 / (numATL+numBTL)
    new MeanPosition(numATL,numBTL,sumVotes.map{_ * mul})
  }
}
object MultiMeanPosition {
  def compute(data:ElectionData): MultiMeanPosition = {
    // println("Computing statistics for "+data.meta.electionName)
    val btls = new MeanPositionBuilder(data.numCandidates)
    val btlBy1Pref = Array.fill(data.numCandidates){new MeanPositionBuilder(data.numCandidates)}
    for (b<-data.btls) {
      val v = b.toVote
      btls.add(v)
      btlBy1Pref(v.preferences(0)).add(v)
    }
    val all = new MeanPositionBuilder(data.numCandidates)
    val allBy1Pref = Array.fill(data.numCandidates){new MeanPositionBuilder(data.numCandidates)}
    for (v<-data.makeVotes()) {
      all.add(v)
      allBy1Pref(v.preferences(0)).add(v)
    }
    new MultiMeanPosition(btls.get,btlBy1Pref.map{_.get},all.get,allBy1Pref.map{_.get})
  }
}