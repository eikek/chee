package chee.query

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import org.scalatest._
import chee.properties._

class ProgressTest extends FlatSpec with Matchers {

  "andThen" should "call both" in {
    val checked = new AtomicBoolean(false)
    val p1 = Progress.before[Option[String], Int]{ (n: Int) => checked.set(true) }
    val p2 = Progress.after[Option[String], Int] { (n: Int) => n+1 }

    val (n, _) = (p1 andThen p2).foreach(0)(List(LazyMap(Ident.path -> "bla")), MapGet.value('path))
    checked.get should be (true)
    n should be (1)
  }

  "seq" should "call all given progresses" in {
    val count = new AtomicInteger(0)
    val done = new AtomicInteger(0)
    val p1 = Progress.before[Option[String], Int] { (n: Int) => count.addAndGet(n) }
    val p2 = Progress.after[Option[String], Int] { (n: Int) => n + 1 }
    val p3 = Progress.done[Option[String], Int] { (n: Int, _) => done.addAndGet(n) }

    val (n, _) = Progress.seq(p1,p2,p3).foreach(0)(List(
      LazyMap(Ident.path -> "bla1"),
      LazyMap(Ident.path -> "bla2")), MapGet.value('path))
    count.get should be (1)
    done.get should be (2)
    n should be (2)
  }
}
