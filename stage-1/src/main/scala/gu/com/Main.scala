package gu.com

import gu.com

import scala.collection.+:

object AllRows {
  val rows = List(
    RowDefinition(List(7,3,1,1,7)),
    RowDefinition(List(1,1,2,2,1,1)),
    RowDefinition(List(3,1,3,1,1,3,1)),
    RowDefinition(List(3,1,1,6,1,3,1)),
    RowDefinition(List(3,1,5,2,1,3,1)),
    RowDefinition(List(1,1,2,1,1)),
    RowDefinition(List(7,1,1,1,1,1,1,1,7)),
    RowDefinition(List(3,3)),
    RowDefinition(List(3,1,1,3,1,1,2)),
    RowDefinition(List(1,1,3,2,1,1)),
    RowDefinition(List(4,1,4,2,1,2)),
    RowDefinition(List(1,1,1,1,4,1,3)),
    RowDefinition(List(2,1,1,1,2,5)),
    RowDefinition(List(3,2,2,6,3,1)),
    RowDefinition(List(1,9,1,1,2,1)),
    RowDefinition(List(2,1,2,2,3,1)),
    RowDefinition(List(3,1,1,1,1,5,1)),
    RowDefinition(List(1,2,2,5)),
    RowDefinition(List(7,1,2,1,1,1,3)),
    RowDefinition(List(1,1,2,1,2,2,1)),
    RowDefinition(List(1,3,1,4,5,1)),
    RowDefinition(List(1,3,1,3,10,2)),
    RowDefinition(List(1,3,1,1,6,6)),
    RowDefinition(List(1,1,2,1,1,2)),
    RowDefinition(List(7,2,1,2,5))
  )
}

case class RowDefinition(tiles: List[Int]) {

  val boardWidth = 25

  val tileString = {
    val firstN = tiles.take(tiles.length - 1)
    val t = firstN.map{ tile =>
      List.fill(tile){1} :+ 0
    }
    val ret = t :+ List.fill(tiles.last){1}

    ret :+ List.empty
  }

  val sum = tiles.sum
  val groups = tiles.length
  val slots = groups + 1
  val spaces = groups - 1
  val extraSpaces = boardWidth - (sum + spaces)

  val spacesList = (1 to extraSpaces).toList

  val combinations = Sum.sumCombinations(extraSpaces, spacesList)

  def template(item: List[Int]): List[Int] = List.fill(slots - item.length){0} ++ item

  val permutations = {
    combinations.map { item =>
      val t = template(item)
      t.permutations.toList
    }
  }

  val permutationsString: List[List[String]] = permutations.map{ items =>
    items.map { perm =>
      val strings = for ( (p, s) <- perm zip tileString) yield {
        val sString = s mkString ""
        p match {
          case -1 => ""
          case 0 => "" + sString
          case x => "0" * x + sString
        }
      }
      strings mkString ""
    }
  }
}

object Sum {
  def sumCombinations(total: Int, numbers: List[Int]): List[List[Int]] = {

    def add(x: (Int, List[List[Int]]), y: (Int, List[List[Int]])): (Int, List[List[Int]]) = {
      (x._1 + y._1, x._2 ::: y._2)
    }

    def sumCombinations(resultAcc: List[List[Int]], sumAcc: List[Int], total: Int, numbers: List[Int]): (Int, List[List[Int]]) = {
      if (numbers.isEmpty || total < 0) {
        (0, resultAcc)
      } else if (total == 0) {
        (1, sumAcc :: resultAcc)
      } else {
        add(sumCombinations(resultAcc, sumAcc, total, numbers.tail), sumCombinations(resultAcc, numbers.head :: sumAcc, total - numbers.head, numbers))
      }
    }

    sumCombinations(Nil, Nil, total, numbers.sortWith(_ > _))._2
  }
}
