package chee.properties

import better.files._
import java.time.Duration

case class MapGet[+A](run: LazyMap => (LazyMap, A)) { self =>

  def map[B](f: A => B): MapGet[B] = MapGet { map =>
    val (next, a) = run(map)
    (next, f(a))
  }

  def flatMap[B](f: A => MapGet[B]): MapGet[B] = MapGet { map =>
    val (next, va) = run(map)
    f(va).run(next)
  }

  def when(f: MapGet[Boolean]): MapGet[Option[A]] = f.flatMap {
    case true => self.map(Some(_))
    case _ => MapGet.unit(None)
  }

  def whenNot(f: MapGet[Boolean]): MapGet[Option[A]] =
    when(Predicates.not(f))

  def combine[B, C](h: MapGet[B])(f: (A, B) => C): MapGet[C] = MapGet { map =>
    val (nexta, a) = run(map)
    val (nextb, b) = h.run(nexta)
    (nextb, f(a, b))
  }

  def result(lmap: LazyMap): A = run(lmap)._2

  def modify(f: LazyMap => LazyMap): MapGet[A] =
    flatMap(a => MapGet.modify(f).map(_ => a))

  def timed: MapGet[(A, Duration)] = MapGet { m =>
    val ((nextm, a), dur) = chee.Timing.timedResult(run(m))
    (nextm, (a, dur))
  }

  def around[B](before: MapGet[Unit], after: (A, Duration) => MapGet[B]): MapGet[B]
    = MapGet.aroundEffect(before, after)(this)
}

object MapGet {

  val virtualKeys: MapGet[Set[Ident]] = get.map(_.virtualKeys)
  val propertyKeys: MapGet[Set[Ident]] = get.map(_.propertyKeys)
  val allKeys: MapGet[Set[Ident]] = propertyKeys.combine(virtualKeys){ _ ++ _ }

  def idents(includeVirtual: Boolean): MapGet[Seq[Ident]] = {
    if (includeVirtual) allKeys.map(Ident.sort)
    else propertyKeys.map(Ident.sort)
  }

  def find(id: Ident): MapGet[Option[Property]] =
    MapGet(map => map(id))

  def value(id: Ident): MapGet[Option[String]] =
    find(id).map(_.map(_.value))

  def valueForce(id: Ident): MapGet[String] =
    value(id).map {
      case Some(s) => s
      case None => sys.error(s"Property not available: ${id.name}")
    }

  def path: MapGet[File] =
    valueForce(Ident.path).map(File(_))

  def existingPath: MapGet[Option[File]] =
    path.map(p => Option(p).filter(_.exists))

  /** Modifies the map by changing {{path}} and {{location}} properties
    * by the given function. */
  def changePath(f: String => String): MapGet[Unit] =
    seq(Seq(find(Ident.path), find(Ident.location))).flatMap { list =>
      val props = list.collect({
        case Some(Property(id, value)) =>
          Property(id, f(value))
      })
      modify(props.foldLeft(_){ (m, p) => m + p })
    }

  def convert[T](id: Ident, conv: Converter[T]) =
    value(id).map(v => v.map(conv.parse(_)))

  def intValue(id: Ident) = convert(id, IntConverter)

  def unit[A](a: A): MapGet[A] = MapGet(map => (map, a))

  def pair[A, B](a: MapGet[A], b: MapGet[B]): MapGet[(A, B)] =
    a.combine(b)((av, bv) => (av, bv))

  def seq[A](gs: Seq[MapGet[A]]): MapGet[List[A]] =
    gs.foldRight(unit(List[A]())){ (e, acc) =>
      e.combine(acc)(_ :: _)
    }

  def concat(ds: Seq[MapGet[String]]): MapGet[String] =
    ds.foldLeft(unit("")) { (acc, e) =>
      acc.combine(e){ (last, s) => last + s }
    }

  def joinEitherBiased[A, B](a: Seq[MapGet[Either[A, B]]]): MapGet[Either[A, List[B]]] = {
    val zero: Either[A, List[B]] = Right(Nil)
    a.toStream.foldRight(unit(zero)) { (e, acc) =>
      e.combine(acc) { (ev, accv) =>
        if (accv.isLeft) accv
        else if (ev.isLeft) Left(ev.left.get)
        else Right(ev.right.get :: accv.right.get)
      }
    }
  }

  def set(lm: LazyMap): MapGet[Unit] = MapGet(_ => (lm, ()))

  def get: MapGet[LazyMap] = MapGet(m => (m, m))

  def modify(f: LazyMap => LazyMap): MapGet[Unit] =
    for {
      m <- get
      _ <- set(f(m))
    } yield ()

  def filter(iter: Traversable[LazyMap], pred: MapGet[Boolean]): Traversable[LazyMap] =
    iter.map(pred.run).collect {
      case (m, true) => m
    }

  def filter(iter: Stream[LazyMap], pred: MapGet[Boolean]): Stream[LazyMap] =
    filter(iter.asInstanceOf[Traversable[LazyMap]], pred).toStream

  def parfilter(iter: Stream[LazyMap], pred: MapGet[Boolean]) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    sealed trait Data
    case class MapData(m: LazyMap) extends Data
    case object EOI extends Data

    val nthreads = Parallelism.globalParallelism
    val inq = new java.util.concurrent.LinkedBlockingQueue[Data]()
    val outq = new java.util.concurrent.LinkedBlockingQueue[Data]()

    Future {
      iter.foreach(m => inq.offer(MapData(m)))
      inq.offer(EOI)
    }

    def submit(): Future[Unit] = Future {
      inq.take match {
        case EOI =>
          inq.offer(EOI)
          outq.offer(EOI)
        case MapData(m) =>
          val (next, b) = pred.run(m)
          if (b) outq.offer(MapData(next))
          submit()
      }
    }
    for (i <- 1 to nthreads) {
      submit()
    }

    def result(i: Int): Stream[LazyMap] = outq.take match {
      case EOI if i == 1 => Stream.empty
      case EOI => result(i - 1)
      case MapData(m) => m #:: result(i)
    }

    result(nthreads)
  }


  def aroundEffect[A, B](before: MapGet[Unit], after: (A, Duration) => MapGet[B])(geta: MapGet[A]): MapGet[B] =
    before.flatMap( _ =>
      geta.timed.flatMap { case (a, dur) =>
        after(a, dur)
      }
    )

  def fold[T](zero: T, maps: Traversable[LazyMap])(action: T => MapGet[T]): T =
    maps.foldLeft(zero) { (t, m) => action(t).result(m) }

  def foreach[T](maps: Iterable[LazyMap], action: Int => MapGet[T]): Int =
    fold(0, maps)(n => action(n).map(_ => n+1))

  def foreach[T](maps: Iterable[LazyMap], action: MapGet[T]): Int =
    foreach(maps, _ => action)

}
