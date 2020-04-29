/*
    Copyright 2016-2020 Silicon Econometrics Pty. Ltd.

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

import org.greatcactus.vote.count.ballots.GroupInformation.GroupID
import org.greatcactus.vote.count.ballots.parsing.IterateOverRawBTLData

trait ElectionCountRules {
  val name:String
  val usedIn:List[String]
  val minATLmarksToBeValid : Int
  val minBTLmarksToBeValid : Int
}

trait ElectionSpecification {
  def jurisdiction:String
  def year : String
  def region : String

  def metadataAvailable : Boolean
  def dataAvailable : Boolean
  def iterateOverRawBTLDataAvailable : Boolean

  /** The number of candidates to be elected */
  def numToBeElected : Int

  def metadata : ElectionMetadata
  def data : ElectionData

  def getIterateOverRawBTLData : IterateOverRawBTLData

  def ticketRoundingChoicesMade:Map[GroupID,Int] = Map.empty

  /** Name suitable for a file based on this specification */
  def filename : String = jurisdiction+"_"+year+"_"+region
}

trait SetOfConcurrentElectionsSpecification {
  def jurisdiction:String
  def year : String
  def availableRegions:Seq[String]
  def getSpec(region:String) : ElectionSpecification
  def get(region:String) : Option[ElectionSpecification] = if (availableRegions.contains(region)) Some(getSpec(region)) else None
}

trait ElectionJurisdictionSpecification {
  def jurisdiction:String
  def availableYears:Seq[String]
  def getSpec(year:String) : SetOfConcurrentElectionsSpecification
  def get(year:String) : Option[SetOfConcurrentElectionsSpecification] = if (availableYears.contains(year)) Some(getSpec(year)) else None
}

