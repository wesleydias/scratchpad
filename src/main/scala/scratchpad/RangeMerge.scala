package scratchpad

import scala.collection.SortedSet

object order extends Ordering[(Int,Int)] {
  override def compare(a: (Int,Int), b: (Int,Int)): Int = {
    if (a._1 != b._1) a._1 - b._1
    else a._2 - b._2
  }
}

object RangeMerge {
  def main(args:Array[String]):Unit = {
    val sorted_input = SortedSet((6, 9), (1, 4), (3, 5), (8, 12))(order)

    def deduce(list: List[(Int,Int)], pair: (Int,Int)): List[(Int,Int)] = {
      val finalList = (pair, list) match {
        case (pair, head :: tail) if (pair._1 <= head._2) => (head._1, if (pair._2 > head._2) pair._2 else head._2) :: tail
        case (pair, emptyList) => pair :: emptyList
      }
      finalList
    }

    val result = sorted_input.foldLeft(List[(Int,Int)]())(deduce)

    println(result)
  }
}