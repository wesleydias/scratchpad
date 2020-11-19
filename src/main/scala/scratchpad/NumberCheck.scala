package scratchpad

object NumberCheck {

  def deduce(array:Array[Int],pair:Int):Array[Int] = {
    val finalArray = { (pair,array) match {
      case (pair,array)  => if (pair < array.length) {array(pair) = pair; array} else array
    }

    }
    finalArray
  }

  def getMinNumber(res:Array[Int]):Int = {
    var minNumber = -1
    for(i <- 0 to res.length-1) {
        if (res(i) == -1) {
          minNumber = i
          return minNumber
        }
    }
    return (res.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val input = Array(2,1,0,5,3,8,7,9)

    val result = input.foldLeft(Array.fill[Int](input.length)(-1))(deduce)

    println(getMinNumber(result))
  }

}
