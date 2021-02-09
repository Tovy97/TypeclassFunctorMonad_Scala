package seminar_codes

object ForMain extends App {
  //For expression - Option
  val temp1 = for {
    i <- Option(1)
    j <- Option(2)
    k <- Option(3)
  } yield i + j + k
  println(temp1) //Some(6)

  //desugar
  val temp2: Option[Int] = Some(1).flatMap { //bind(Int=>Option[Int]) : Option[Int]
    (i: Int) => Some(2).flatMap {            //bind(Int=>Option[Int]) : Option[Int]
      (j: Int) => Some(3).map {              //map(Int=>Int)          : Option[Int]
        (k: Int) => i + j + k
      }
    }
  }
  println(temp2) //Some(6)

  //For expression - List
  val temp3 = for {
    i <- List(1, 2)
    j <- List(3, 4)
  } yield i + j
  println(temp3) //List(4, 5, 5, 6)

  //desugar
  val temp4: List[Int] = List(1, 2).flatMap { //bind(Int=>List[Int]) : List[Int]
    (i: Int) => List(3, 4).map {              //map(Int=>Int)        : List[Int]
      (j: Int) => i + j
    }
  }
  println(temp4) //List(4, 5, 5, 6)

}
