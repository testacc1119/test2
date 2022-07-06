class MySuite extends munit.FunSuite {
 test("List.foldLeft (Int)"){
    val expected = 600
    val actual = List(1,2,3,4,5).foldLeft(5)(_*_)
    assertEquals(expected,actual)
  }

  test("List.foldLeft (String)"){
    val expected = "123abcde"
    val actual = List("a","b", "c", "d", "e").foldLeft("123")(_+_)
    assertEquals(expected,actual)
  }
}
