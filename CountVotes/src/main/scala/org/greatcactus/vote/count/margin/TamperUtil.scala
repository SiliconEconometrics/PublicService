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
}

