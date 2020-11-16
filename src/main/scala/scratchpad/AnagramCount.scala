package scratchpad

object order1 extends Ordering[(String,String)] {
  override def compare(a: (String,String), b: (String,String)): Int = {
    (a._1 compare b._1) match {
      case 0 => (a._2 compare b._2) match {
        case  d => d
      }
      case c => c
    }
  }
}

object AnagramCount {

  def max(xs: List[(String,List[String],Int)]): Option[Int] =
    xs match {
      case Nil => None
      case List((a:String,b:List[String], x: Int)) => Some(x)
      case x :: y :: rest => max( (if (x._3 >= y._3) x else y) :: rest )
  }

  def normalizedStringOps(s: String): List[(String,List[String],Int)] = {
    implicit val ord = order1

    val s1 = s.toLowerCase.replaceAll("[(.,)]","")

    val s2 = s1.split(" ").filter(_.length > 3).toList

    s2.map(c => (c.sorted,c)).sorted.map(c => (c._1,List(c._2),1))
  }

  def fetchLongestAnagram(list:List[(String,List[String],Int)],pair:(String,List[String],Int)):
    List[(String,List[String],Int)] = {

    val finalList = (pair,list) match {
      case (pair, head :: tail) if (pair._1 equals head._1) =>
        (head._1, pair._2 ::: head._2, (head._3 + 1)) :: tail
      case (pair, emptyList) => pair :: emptyList
    }
    finalList
  }

  def main(args:Array[String]):Unit = {

    val sentence = "Bristly birds steal the least stale tales of Tesla," +
      " that are written on a slate stela (stone slab). " +
      "Bristly birds sbeal the leasb sbale bales of Besla," +
      " that are written on a slabe sbela (stone slab)."

    val nz_sentence = normalizedStringOps(sentence)

    val fetchAnagram = nz_sentence.foldLeft(List[(String,List[String],Int)]())(fetchLongestAnagram)

    val maxFetchAnagram:Int = max(fetchAnagram) match {
      case Some(x) => x
      case None => 0
    }

    println(fetchAnagram)

    println(fetchAnagram.filter(_._3 == maxFetchAnagram).map(_._2))

  }

}
