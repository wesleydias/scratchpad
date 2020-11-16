package scratchpad

object order2 extends Ordering[(String,Int,Int)] {
  override def compare(a: (String,Int,Int), b: (String,Int,Int)): Int = {
    b._2 compare a._2 match {
      case 0 => b._3 compare a._3 match {
        case m => m
      }
      case a => a
    }
  }
}

object GameFun {

  def normalizeInput(input: List[(String, Int, Int)]):List[(String,Int,Int)] = {
    implicit val ord2 = order2
    input.map(s => (s._1.toLowerCase,s._2,s._3)).sorted
  }

  def deduce1(list: List[(List[String],List[Int],List[Int])],pair:(String,Int,Int))
    :List[(List[String],List[Int],List[Int])] = {

    val finalList = (pair,list) match {
      case (pair, e@(head :: tail)) if ((pair._2 + head._2.head) <= 120)
        => e.filter(x => (pair._2 + x._2.sum) <= 120)
        .map(s => (pair._1 :: s._1,pair._2 ::s._2,pair._3 :: s._3) )::: e
      case (pair,emptyList)  => if (pair._2 <= 120)
        (pair._1 :: Nil,pair._2 :: Nil,pair._3 :: Nil) :: emptyList else emptyList
    }

      finalList
  }

  def main(args: Array[String]): Unit = {

    implicit val ord3 = order3
    val input = List(("Pac-man",90,400),
      ("Mortal Kombat",10,30),
      ("Super Tetris",25,100),
      ("Pump it Up",10,40),
      ("Street Fighter II",90,450),
      ("Speed Racer",10,40))
    
    val nz_input = normalizeInput(input)

    val gamePermutations = nz_input.foldLeft(List[(List[String],List[Int],List[Int])]())(deduce1)

    val gameWeight = gamePermutations.map(s => (s._1.mkString(":"),s._3.sum)).sorted

    val resultGame = gameWeight.head._1.split(":").toList.sorted


    println(nz_input)
    println(gamePermutations)
    println(gameWeight)
    println(resultGame)
  }

}
