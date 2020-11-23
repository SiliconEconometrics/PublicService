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

package org.greatcactus.vote.count

import org.greatcactus.vote.count.MainDataTypes.Tally

case class RationalTransferValue(numerator:BigInt,denominator:BigInt) extends Ordered[RationalTransferValue] {
  val toDouble :Double = numerator.toDouble/denominator.toDouble

  /** Multiply a tally by n, producing an integer part and a fractional part between 0 inclusive and 1 exclusive */
  def mul(n:Tally) : (Tally,Double) = {
    val bn = numerator*n;
    val int_part = (bn/denominator).toLong
    val fractional_part = (bn%denominator).toDouble/denominator.toDouble
    (int_part,fractional_part)
  }

  def * (other:RationalTransferValue) : RationalTransferValue = RationalTransferValue(numerator*other.numerator,denominator*other.denominator)

  /** -1 when `this < that`, 0  when `this == that`, 1  when  `this > that`
   */
  override def compare(that: RationalTransferValue): Int = (this.numerator*that.denominator-that.numerator*this.denominator).signum
}

object RationalTransferValue {
  def apply(numerator:BigInt,denominator:BigInt): RationalTransferValue = {
    if (numerator.signum==0) zero else {
      val gcd = numerator.gcd(denominator)
      new RationalTransferValue(numerator/gcd,denominator/gcd)
    }
  }
  private val bigIntZero = BigInt(0)
  private val bigIntOne = BigInt(1)
  val zero = new RationalTransferValue(bigIntZero,bigIntOne)
  val one = new RationalTransferValue(bigIntOne,bigIntOne)
}


object MainDataTypes {
  type CandidateIndex = Int // an integer that represents a candidate in particular, being the index into the array of candidates.
  type GroupIndex = Int
  type NumberOfCandidates = Int  // an integer that represents a number of candidates, such as the number elected at a given count.
  type CountNumber = Int
  type TransferValue = RationalTransferValue
  val TransferValueOne : TransferValue = RationalTransferValue.one
  def transferValue(surplus:Tally,ballots:PaperCount) : TransferValue = RationalTransferValue(surplus,ballots)
  type Tally = Long
  type TallyUnscaled = Long
  type TallyScaled = Long
  val TallyMaxValue : Tally = Long.MaxValue
  //val Tally0 : Tally = 0
  type PaperCount = Long
  type PaperCountUnscaled = Long
  type PreferenceNumber = Int
  implicit class ToTallyHelper(d:Double) {
    def toTally = d.toLong
    def toPaperCount = d.toLong
  }
}

