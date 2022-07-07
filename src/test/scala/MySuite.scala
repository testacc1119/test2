package dyhas.bo.lab1
import scala.collection.mutable
import munit.Clue.generate

class MySuite extends munit.FunSuite {

test("List constructor"){
  val expected = List(1)
  var z = 1
  val actual = List(z)
  assertEquals(actual, expected)
}

test("List.reverse empty"){
  val expected = List()
  val actual = List().reverse
  assertEquals(actual, expected)
}

test("List.reverse"){
  val expected = List(3, 2, 1)
  val temp_list = List(1, 2, 3)
  val actual = temp_list.reverse
  assertEquals(actual, expected)
}

test("List.add_left"){
  val expected = List(0, 1, 2, 3)
  val actual = List.of(1, 2, 3).add_left(0)
  assertEquals(actual, expected)
}

test("List.add_left empty"){
  val expected = List(1)
  val actual = List.of().add_left(1)
  assertEquals(actual, expected)
}

test("ListofLists.add_left"){
  val expected = List(List(1), List(2))
  val actual = List.of(List(2)).add_left(List(1))
  assertEquals(actual, expected)
}

test("ListofLists.add_left empty"){
  val expected = List((List(1)))
  val actual = List.of().add_left(List(1))
  assertEquals(actual, expected)
}

test("List.add_right"){
  val expected = List(0, 1, 2, 3)
  val actual = List.of(0, 1, 2).add_right(3)
  assertEquals(actual, expected)
}

test("List.add_right empty"){
  val expected = List(1)
  val actual = List.of().add_right(1)
  assertEquals(actual, expected)
}

test("ListofLists.add_right"){
  val expected = List(List(1), List(2))
  val actual = List.of(List(1)).add_right(List(2))
  assertEquals(actual, expected)
}

test("ListofLists.add_right empty"){
  val expected = List((List(1)))
  val actual = List.of().add_right(List(1))
  assertEquals(actual, expected)
}

test("List.foldLeft (Int)"){
  val expected = 600
  val actual = List(1,2,3,4,5).foldLeft(5)(_*_)
  assertEquals(expected,actual)
}

test("List.foldLeft (String)"){
  val expected = "123abcde"
  val actual = List("a","b", "c", "d", "e").foldLeft("123")(_+_)
  assertEquals(actual, expected)
}

test("List.grouped"){
  val expected = List(List(0, 1), List(2, 3))
  var temp = List(0, 1, 2, 3)
  val actual = temp.grouped(2)
  assertEquals(actual, expected)
}

test("List.sliding"){
  val expected = List(List(1, 2, 3), List(2, 3, 4))
  var temp = List(1, 2, 3, 4)
  val actual = temp.sliding(3)
  assertEquals(actual, expected)
}

test("List.windowed"){
  val expected = List(List(1, 2, 3), List(3, 4, 5), List(5, 6, 7), List(7, 8, 9))
  var temp = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val actual = temp.windowed(3, 2)
  assertEquals(actual, expected)
}

}