package scratchpad

object order3 extends Ordering[(String,Int)] {
  override def compare(a: (String,Int), b: (String,Int)): Int = {
    b._2 compare a._2 match {
      case 0 => a._1 compare b._1 match {
        case m => m
      }
      case a => a
    }
  }
}

object MovieFun {

  def normalizeInputStrings(ip:List[(String,String)]):List[(String,String,Int)] ={
    val finalList = ip.map(s => (s._1.toLowerCase,s._2.toLowerCase,1))
    finalList
  }

  def deduce(list:List[(String,Int)],pair:(String,String,Int)):
    List[(String, Int)] = {

    val finalResult = (pair,list) match {
      case (pair,head :: tail) if(pair._1 equals head._1) => (head._1,(head._2 + 1) ) :: tail
        case(pair,emptyList) => (pair._1,pair._3) :: emptyList
    }

    finalResult
  }

  def getTopThree(list:List[Int],pair:(String,Int)):List[Int] = {
    val finalResult = (pair,list) match {
      case (pair,head :: tail) if (pair._2 == head ) => head :: tail
      case (pair,emptyList) => if (emptyList.length < 3) pair._2 :: emptyList else emptyList
    }
    finalResult
  }

  def main(args: Array[String]): Unit = {

    implicit val ord2: order2.type = order2
    implicit val ord3: order3.type = order3

    val input = List(("Leonardo DiCaprio","The Revenant"),
      ("Christian Bale","Vice"),
      ("Christian Bale","Ford vs Farrari"),
      ("Morgan Freeman","Shawshank Redemption"),
      ("Leonardo DiCaprio","Django Unchained"),
      ("Christian Bale","American Psycho"),
      ("Morgan Freeman","The Dark Knight"),
      ("Leonardo DiCaprio","Titanic"),
      ("Christian Bale","The Dark Knight"),
      ("Samuel L. Jackson","Pulp Fiction"),
      ("John Travolta","Pulp Fiction"))

    val nz_input = normalizeInputStrings(input).sorted

    val result = nz_input.foldLeft(List[(String,Int)]())(deduce).sorted

    val topThree = result.foldLeft(List[Int]())(getTopThree).reverse

    val finalList = result.filter(topThree contains _._2)

    println(finalList)
  }

}
