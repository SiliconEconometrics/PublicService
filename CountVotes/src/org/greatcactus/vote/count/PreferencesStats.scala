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


object PreferencesStats extends App {

  
  val prefFrom = "HUGHES Hollie"
  val prefsTo = List("JONES Peter","HOUSSOS Courtney","BORSAK Robert","NILE Fred","PEARSON Mark")
  
  def proc(data:ElectionData) {
    val from = data.candidateIndex(prefFrom)
    val to = prefsTo.map{data.candidateIndex}
    val careAbout = (from::to).toSet
    var exhausted = 0
    val prefTo = new Array[Int](data.candidates.length)
    println("Loaded data")
    val votes = data.makeVotes
    println("Got votes")
    for (v<-votes) {
      val sub = v.preferences.filter { careAbout }
      if (sub.length>=1 && sub.head==from) {
        if (sub.length>=2) prefTo(sub(1))+=v.numVoters
        else exhausted+=v.numVoters
      }
    }
    println("Preferences for "+data.candidates(from).name)
    println("Exhausted\t"+exhausted)
    for (i<-to) println(data.candidates(i).name+"\t"+prefTo(i))
  }
  
  proc(LoadFromFile.load(false))
  println("\n\nIVOTE\n")
  proc(LoadFromFile.load(true))

}