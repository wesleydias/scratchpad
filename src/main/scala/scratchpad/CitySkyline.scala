package scratchpad

object Order4 extends Ordering[(Int,Int,Int)] {
  override def compare(x: (Int, Int, Int), y: (Int, Int, Int)): Int = {
    x._1 compare y._1 match {
      case 0 => x._2 compare y._2 match {
        case 0 => x._3 compare y._3 match {
          case v => v
        }
        case s => s
      }
      case c => c
    }
  }
}

object CitySkyline {

  val deduce = (list:List[(Int, Int, Int)], pair:(Int, Int, Int)) =>
  { val finalList = (pair,list) match {
      case (pair, head :: tail) if pair._1 < head._2 =>
        if (pair._3 != head._3)
          pair :: (head._1,pair._1,head._3) :: tail
          else
        (head._1,if(pair._2 > head._2) pair._2 else head._2,pair._3) :: tail
      case(pair, emptyList) => pair :: emptyList
    }
    finalList
    }

  def main(args: Array[String]): Unit = {
    implicit val ord4 = Order4
    val input = List((5,8,3),(2,3,1),(3,4,2),(4,6,2))
    val sortedInput = input.sorted

    val nz_input = sortedInput.foldLeft(List[(Int,Int,Int)]())(deduce)

    val finalResult = nz_input.map(s => ((s._2 - s._1)*s._3))

    println(sortedInput)
    println(nz_input)
    println(finalResult.sum)

  }

}
