package chee

import org.scalatest._
import better.files._
import chee.LocationConf.{Entry => LE}
import scala.util.{Try, Failure}

class LocationConfTest extends FlatSpec with Matchers with chee.FileLoan {

  "remove(dir)" should "replace file with filtered list" in withNewFile { f =>
    val es = List(LE("/mnt/fotos/1", "", true, false), LE("/mnt/fotos/2", "", false, true))
    val conf = new LocationConf(ConfigFile(f), None)
    conf.addAll(es).get
    conf.list.get should be (es)
    conf.remove(es.head.dir).get should be (es.tail)
    conf.list.get should be (es.tail)
  }

  "add" should "not add entries twice" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f), None)

    val entry = LE("/mnt/fotos/1", "", true, false)
    conf.add(entry).get
    conf.add(entry).get
    conf.list.get should be (List(entry))
  }

  it should "replace with new entries" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f), None)
    val entry1 = LE("/mnt/fotos/1", "", true, false)
    val entry2 = LE("/mnt/fotos/2", "", true, false)
    val entry3 = LE("/mnt/fotos/3", "", true, false)

    conf.addAll(Seq(entry1, entry2, entry3))
    conf.list.get should be (List(entry1, entry2, entry3))
    conf.add(entry2.copy(query = "model?"))
    conf.list.get should be (List(entry1, entry2.copy(query = "model?"), entry3))
  }

  it should "append new entries" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f), None)
    val entry1 = LE("/mnt/fotos/1", "", true, false)
    val entry2 = LE("/mnt/fotos/2", "", true, false)

    conf.add(entry1)
    conf.list.get should be (List(entry1))
    conf.add(entry2)
    conf.list.get should be (List(entry1, entry2))
  }

  "checkRegisteredLocations" should "throw for non-location dirs" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f), None)
    val entry1 = LE("/mnt/fotos/1", "", true, false)
    val entry2 = LE("/mnt/fotos/2", "", true, false)
    conf.addAll(Seq(entry1, entry2))
    val check: Seq[File] => Unit =
      s => cli.Location.checkRegisteredLocations(conf, s)

    intercept[UserError] {
      check(Seq(file"/a/b"))
    }
    intercept[UserError] {
      check(Seq(file"/mnt/fotos/1", file"/a/b"))
    }
  }

  it should "include every failed dir" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f), None)
    val entry1 = LE("/mnt/fotos/1", "", true, false)
    val entry2 = LE("/mnt/fotos/2", "", true, false)
    conf.addAll(Seq(entry1, entry2))
    val check: Seq[File] => Unit =
      s => cli.Location.checkRegisteredLocations(conf, s)

    val Failure(error) = Try(check(Seq(file"/a/b", file"/mnt/fotos/3")))
    error.getMessage should include ("/a/b")
    error.getMessage should include ("/mnt/fotos/3")
  }

  it should "be successful for location and subdirs" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f), None)
    val entry1 = LE("/mnt/fotos/1", "", true, false)
    val entry2 = LE("/mnt/fotos/2", "", true, false)
    conf.addAll(Seq(entry1, entry2))

    cli.Location.checkRegisteredLocations(conf, Seq(
      file"/mnt/fotos/1",
      file"/mnt/fotos/1/a",
      file"/mnt/fotos/2/",
      file"/mnt/fotos/2/a/z/n"))
  }

    "checkNotRegisteredLocations" should "throw for location dirs" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f), None)
    val entry1 = LE("/mnt/fotos/1", "", true, false)
    val entry2 = LE("/mnt/fotos/2", "", true, false)
    conf.addAll(Seq(entry1, entry2))
    val check: Seq[File] => Unit =
      s => cli.Location.checkNotRegisteredLocations(conf, s)

    intercept[UserError] {
      check(Seq(file"/mnt/fotos/1"))
    }
    intercept[UserError] {
      check(Seq(file"/mnt/fotos/1/a", file"/a/b"))
    }
  }

  it should "include every failed dir" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f), None)
    val entry1 = LE("/mnt/fotos/1", "", true, false)
    val entry2 = LE("/mnt/fotos/2", "", true, false)
    conf.addAll(Seq(entry1, entry2))
    val check: Seq[File] => Unit =
      s => cli.Location.checkNotRegisteredLocations(conf, s)

    val Failure(error) = Try(check(Seq(file"/mnt/fotos/1", file"/mnt/fotos/2/a")))
    error.getMessage should include ("/mnt/fotos/1")
    error.getMessage should include ("/mnt/fotos/2/a")
  }

  it should "be successful for non-locations" in withNewFile { f =>
    val conf = new LocationConf(ConfigFile(f), None)
    val entry1 = LE("/mnt/fotos/1", "", true, false)
    val entry2 = LE("/mnt/fotos/2", "", true, false)
    conf.addAll(Seq(entry1, entry2))

    cli.Location.checkNotRegisteredLocations(conf, Seq(
      file"/a/b",
      file"/mnt/fotos/3/a",
      file"/mnt/fotos/",
      file"/mnt/fotos/z"))
  }
}
