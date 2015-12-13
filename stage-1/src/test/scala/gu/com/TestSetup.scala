package gu.com

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TestSetup extends FlatSpec with ShouldMatchers {
  "RowDefinition" should "produce the correct number of permutations" in {
    val r = RowDefinition(List(7,3,1,1,7))

    r.sum should be (19)
    r.groups should be (5)
    r.slots should be (6)
    r.spaces should be (4)
    r.extraSpaces should be (2)
    r.combinations should be (List(List(1, 1), List(2)))
    println(r.permutationsString)


  }
  "rows" should "be long" in {
    println(AllRows.rows.map(_.permutationsString))
  }
}

