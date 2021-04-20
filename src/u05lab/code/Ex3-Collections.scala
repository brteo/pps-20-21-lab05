package u05lab.code

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime()-startTime, TimeUnit.NANOSECONDS)
    if(!msg.isEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)
}

import PerformanceUtils._
import scala.collection.immutable.{HashSet, TreeSet}
import scala.collection.mutable.{Buffer, ListBuffer, ArrayBuffer, Set => MutableSet, HashSet => MutableHashSet, TreeSet => MutableTreeSet}

trait IterableTest[A] {
  def i: Iterable[A]

  def size: Int = i.size
  def last: A = i.last
  def append(value: A): Iterable[A]
  def remove(value: A): Iterable[A]
}

/* Immutable */
case class SetTest[A](i: Set[A]) extends IterableTest[A] {
  override def append(value: A): Set[A] = i + value
  override def remove(value: A): Set[A] = i - value
}
case class SeqTest[A](i: Seq[A]) extends IterableTest[A] {
  override def append(value: A): Seq[A] = i :+ value
  override def remove(value: A): Seq[A] = i.filter(_ != value)
}

/* Mutable */
case class BufferTest[A](i: Buffer[A]) extends IterableTest[A] {
  override def append(value: A): Buffer[A] = i += value
  override def remove(value: A): Buffer[A] = i -= value
}
case class MutableSetTest[A](i: MutableSet[A]) extends IterableTest[A] {
  override def append(value: A): MutableSet[A] = i += value
  override def remove(value: A): MutableSet[A] = i -= value
}


object TestTool {
  val NUMBERS: Int = 1000000
  val REPS: Int = 5

  val iterables:Map[String, IterableTest[Int]] = Map(
    /* Immutable */
    "Set" -> SetTest((1 to NUMBERS).toSet),
    "HashSet" -> SetTest((1 to NUMBERS).to[HashSet]),
    "TreeSet" -> SetTest((1 to NUMBERS).to[TreeSet]),
    "List" -> SeqTest((1 to NUMBERS).toList),
    "Vector" -> SeqTest((1 to NUMBERS).toVector),
    "Stream" -> SeqTest((1 to NUMBERS).toStream),
    /* Mutable */
    "ListBuffer" -> BufferTest((1 to NUMBERS).to[ListBuffer]),
    "ArrayBuffer" -> BufferTest((1 to NUMBERS).to[ArrayBuffer]),
    "MutableHashSet" -> MutableSetTest((1 to NUMBERS).to[MutableHashSet]),
    "MutableTreeSet" -> MutableSetTest((1 to NUMBERS).to[MutableTreeSet]),
  )

  val tests = Set("size", "last", "append", "remove")

  def go:Map[String, Map[String, FiniteDuration]] =
    tests
      .foldLeft( Map[String, Map[String, FiniteDuration]]() ) {
        (results, test) => { results + (test -> execTest(test)) }
      }

  def execTest(test: String):Map[String, FiniteDuration] =
    iterables
      .foldLeft( Map[String, FiniteDuration]() ) {
        case (results, (k, v)) => results + (k -> measureTest(test)(v))
      }

  def measureTest(test: String)(i: IterableTest[Int]):FiniteDuration = test match {
    case "size" => mean(measure("size", i.size))
    case "last" => mean(measure("last", i.last))
    case "append" => mean(measure("last", i.append(NUMBERS+1)))
    case "remove" => mean(measure("last", i.append(NUMBERS/2)))
  }

  def mean(expr: => MeasurementResults[_]):FiniteDuration = {
    (1 to REPS).map( _ => expr.duration).reduce(_ + _) / REPS
  }

  def print(results: Map[String, Map[String, FiniteDuration]]):Unit =
    results
      .foreach {
        case (key, values) => {
          println("Test: " + key)
          values
            .toSeq
            .sortWith(_._2 < _._2)
            .foldLeft(FiniteDuration(0, TimeUnit.NANOSECONDS)) {
              case (last, (t, r)) => {
                println("\t" + t + " -> " + r.toNanos.toString() + " ns" + (if (last.toNanos != 0) " (+" + (r - last).toNanos.toString() + " ns)" else ""))
                r
              }
            }
          }
        }

  def exec: Unit = print(go)
}

object CollectionsTest extends App {

  TestTool.exec

  /*
  * Output
  *

  Test: size
    HashSet -> 809 ns
    ListBuffer -> 1784 ns (+975 ns)
    MutableHashSet -> 2545 ns (+761 ns)
    Vector -> 2986 ns (+441 ns)
    TreeSet -> 3444 ns (+458 ns)
    MutableTreeSet -> 3499 ns (+55 ns)
    ArrayBuffer -> 4493 ns (+994 ns)
    Set -> 47197 ns (+42704 ns)
    List -> 5576353 ns (+5529156 ns)
    Stream -> 31627256 ns (+26050903 ns)
  Test: last
    ListBuffer -> 3682 ns
    TreeSet -> 5288 ns (+1606 ns)
    Vector -> 6826 ns (+1538 ns)
    ArrayBuffer -> 7632 ns (+806 ns)
    MutableTreeSet -> 8523 ns (+891 ns)
    Stream -> 6277890 ns (+6269367 ns)
    MutableHashSet -> 12983784 ns (+6705894 ns)
    List -> 20556216 ns (+7572432 ns)
    Set -> 23012226 ns (+2456010 ns)
    HashSet -> 30858477 ns (+7846251 ns)
  Test: append
    ListBuffer -> 2753 ns
    HashSet -> 6125 ns (+3372 ns)
    Set -> 15475 ns (+9350 ns)
    MutableTreeSet -> 19731 ns (+4256 ns)
    TreeSet -> 24490 ns (+4759 ns)
    Vector -> 25152 ns (+662 ns)
    MutableHashSet -> 28097 ns (+2945 ns)
    ArrayBuffer -> 1148156 ns (+1120059 ns)
    Stream -> 1519933 ns (+371777 ns)
    List -> 54674375 ns (+53154442 ns)
  Test: remove
    ListBuffer -> 1233 ns
    HashSet -> 2547 ns (+1314 ns)
    MutableHashSet -> 2653 ns (+106 ns)
    ArrayBuffer -> 4996 ns (+2343 ns)
    MutableTreeSet -> 6288 ns (+1292 ns)
    Set -> 8082 ns (+1794 ns)
    Vector -> 14466 ns (+6384 ns)
    TreeSet -> 17004 ns (+2538 ns)
    Stream -> 31547 ns (+14543 ns)
    List -> 11760816 ns (+11729269 ns)
  */

  /* Comparison */
  // val lst = (1 to 1000000).toList
  // val vec = (1 to 1000000).toVector
  // assert( measure("lst last"){ lst.last } > measure("vec last"){ vec.last } )
}