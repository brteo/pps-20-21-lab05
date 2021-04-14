package u05lab.code

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class ListTest {
  val lInt = List(10, 20, 30, 40)
  val lString = List("a","b","a","c")

  @Test
  def testZipRight() {
    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a",0),("b",1),("a",2),("c",3)), lString.zipRight)
  }

  @Test
  def testPartition() {
    assertEquals((List(20, 30, 40), List(10)), lInt.partition(_ > 15))
    assertEquals((List("a","a"), List("b","c")), lString.partition(_ == "a"))
  }

  @Test
  def testSpan() {
    assertEquals((List.nil, List(10, 20, 30, 40)), lInt.span(_ > 15))
    assertEquals((List(10), List(20, 30, 40)), lInt.span(_ < 15))
    assertEquals((List("a"), List("b","a","c")), lString.span(_ == "a"))
  }

  @Test
  def testReduce() {
    assertEquals(100, lInt.reduce(_ + _))
    assertThrows(classOf[UnsupportedOperationException], () => List.nil[Int].reduce(_+_))
  }
}