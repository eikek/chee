package chee

import org.scalatest._
import better.files._
import chee.LocationConf.{Entry => LE}
import scala.util.{Try, Success, Failure}

class LocationConfTest extends FlatSpec with Matchers with chee.FileLoan {

  "remove(dir)" should "replace file with filtered list" in withNewFile { f =>
    val es = List(LE(file"/mnt/fotos/1", "", true, false), LE(file"/mnt/fotos/2", "", false, true))
    val conf = new LocationConf(ConfigFile(f))
    conf.addAll(es).get
    conf.list.get should be (es)
    conf.remove(es.head.dir).get should be (es.tail)
    conf.list.get should be (es.tail)
  }

  "add" should "not add entries twice" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f))

    val entry = LE(file"/mnt/fotos/1", "", true, false)
    conf.add(entry).get
    conf.add(entry).get
    conf.list.get should be (List(entry))
  }

  it should "replace with new entries" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f))
    val entry1 = LE(file"/mnt/fotos/1", "", true, false)
    val entry2 = LE(file"/mnt/fotos/2", "", true, false)
    val entry3 = LE(file"/mnt/fotos/3", "", true, false)

    conf.addAll(Seq(entry1, entry2, entry3))
    conf.list.get should be (List(entry1, entry2, entry3))
    conf.add(entry2.copy(query = "model?"))
    conf.list.get should be (List(entry1, entry2.copy(query = "model?"), entry3))
  }

  it should "append new entries" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f))
    val entry1 = LE(file"/mnt/fotos/1", "", true, false)
    val entry2 = LE(file"/mnt/fotos/2", "", true, false)

    conf.add(entry1)
    conf.list.get should be (List(entry1))
    conf.add(entry2)
    conf.list.get should be (List(entry1, entry2))
  }

  // "contains" should "check all given paths" in withNewFile { f =>
  //   val conf = new LocationConf(ConfigFile(f))
  //   val entry1 = LE(file"/mnt/fotos/1", "", true, false)
  //   val entry2 = LE(file"/mnt/fotos/2", "", true, false)
  //   conf.addAll(Seq(entry1, entry2))

  //   conf.contains(file"/mnt/fotos").get should be (Left("`/mnt/fotos' is not a known location"))
  //   conf.contains(file"/mnt/fotos/1").get should be (Right(true))
  //   conf.contains(file"/mnt/fotos/1", file"/mnt/fotos").get should be (Left("`/mnt/fotos' is not a known location"))
  //   conf.contains(file"/mnt/xyz", file"/mnt/fotos/1", file"/mnt/fotos").get should be (Left(
  //     "`/mnt/xyz' is not a known location\n`/mnt/fotos' is not a known location"))
  // }
}
