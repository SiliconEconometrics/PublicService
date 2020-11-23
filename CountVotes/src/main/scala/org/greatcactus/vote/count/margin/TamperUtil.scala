/*
    Copyright 2018-2020 Silicon Econometrics Pty. Ltd.

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


package org.greatcactus.vote.count.margin

class TamperUtil {

}

object ListUtil {
  /** Reorder list, moving elements that satisfy f to the front */
  def moveToFront[T](l:List[T])(f:T=>Boolean) : List[T] = {
    val (isf,notf) = l.partition(f)
    isf++notf
  }

}



object BinarySearch {
  /** Find the lowest integer in the inclusive range low...high that satisfies p. E.g if p is _>5, this would return 6. Assumes p is monotonic. If p always returns false, then return high. */
  def findLowest(p:Int=>Boolean,low:Int,high:Int) : Int = {
    if (low>high) throw new IllegalArgumentException(low.toString+" > "+high)
    if (low==high) low else {
      val mid=(low+high)/2
      if (p(mid)) findLowest(p,low,mid) else findLowest(p,mid+1,high)
    }
  }
  /** Find the lowest integer in the inclusive range low...high that satisfies p. E.g if p is _>5, this would return 6. Assumes p is monotonic. If p always returns false, then return high. */
  def findLowest(p:Long=>Boolean,low:Long,high:Long) : Long = {
    if (low>high) throw new IllegalArgumentException(low.toString+" > "+high)
    if (low==high) low else {
      val mid=(low+high)/2
      if (p(mid)) findLowest(p,low,mid) else findLowest(p,mid+1,high)
    }
  }
}

