package edu.knoldus
import scala.annotation.tailrec
class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    @tailrec
    def binarySearch(arr: Array[Int], target: Int, start: Int, end: Int): Int = {
      if(start > end) return -1
      val mid = start + (end - start) / 2
      arr(mid) match {
        case i if (i == target) => mid
        case i if (i > target) => binarySearch(arr, target, start, mid - 1)
        case _ => binarySearch(arr, target, mid + 1, end)
      }
    }
    val result=binarySearch(array, elem, 0, array.length-1)
    if (result==(-1)) false
    else true
  }

  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    if (array.isEmpty) false
    else if (array.head == elem) true
    else linearSearch(array.tail, elem)
  }

}
