package u05lab.code

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class ListTest {

  @Test
  def testZipRight() {
    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a",0),("b",1),("c",2)), List("a","b","c").zipRight)
  }
}