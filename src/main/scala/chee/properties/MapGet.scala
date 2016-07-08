package chee.properties

import better.files._
import java.time.Duration
import chee.util.state._
import chee.util.state.States

object MapGet extends States[LazyMap] {

  val virtualKeys: MapGet[Set[Ident]] = get.map(_.virtualKeys)
  val propertyKeys: MapGet[Set[Ident]] = get.map(_.propertyKeys)
  val allKeys: MapGet[Set[Ident]] = combine(propertyKeys, virtualKeys){ _ ++ _ }

  def idents(includeVirtual: Boolean): MapGet[Seq[Ident]] = {
    if (includeVirtual) allKeys.map(Ident.sort)
    else propertyKeys.map(Ident.sort)
  }

  def find(id: Ident): MapGet[Option[Property]] =
    State(map => map(id))

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

  def add(es: Seq[Property]): MapGet[Unit] =
    modify { m => es.foldLeft(m)(_ + _) }

  def add(e: Property, es: Property*): MapGet[Unit] =
    add(e +: es)

  def remove(ids: Seq[Ident]): MapGet[Unit] =
    modify { m => ids.foldLeft(m)(_ remove _) }

  def remove(id: Ident, ids: Ident*): MapGet[Unit] =
    remove(id +: ids)

  /** Modifies the map by changing {{path}} and {{location}} properties
    * by the given function. */
  def changePath(f: String => String): MapGet[Unit] = {
    val pathOrLocation = (i: Ident) => (i is Ident.location) || (i is Ident.path)
    idents(false).map(_ filter pathOrLocation)
      .flatMap(n => seq(n.map(find)))
      .flatMap { list =>
      val props = list.collect({
        case Some(Property(id, value)) =>
          Property(id, f(value))
      })
      add(props)
    }
  }

  def convert[T](id: Ident, conv: Converter[T]) =
    value(id).map(v => v.map(conv.parse(_)))

  def intValue(id: Ident) = convert(id, IntConverter)

  def concat(ds: Seq[MapGet[String]]): MapGet[String] =
    ds.foldLeft(unit("")) { (acc, e) =>
      combine(acc, e){ (last, s) => last + s }
    }

  def joinEitherBiased[A, B](a: Seq[MapGet[Either[A, B]]]): MapGet[Either[A, List[B]]] = {
    val zero: Either[A, List[B]] = Right(Nil)
    a.toStream.foldRight(unit(zero)) { (e, acc) =>
      combine(e, acc) { (ev, accv) =>
        if (accv.isLeft) accv
        else if (ev.isLeft) Left(ev.left.get)
        else Right(ev.right.get :: accv.right.get)
      }
    }
  }

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

case class MapGetOps[A](self: MapGet[A]) {
  def toMap: MapGet[LazyMap] = self.flatMap(_ => MapGet.get)

  def timed: MapGet[(A, Duration)] = State { m =>
    val ((nextm, a), dur) = chee.Timing.timedResult(self.run(m))
    (nextm, (a, dur))
  }

  def around[B](before: MapGet[Unit], after: (A, Duration) => MapGet[B]): MapGet[B] =
    MapGet.aroundEffect(before, after)(self)

  def add(e: Property, es: Property*): MapGet[A] =
    for {
      a <- self
      _ <- MapGet.add(e, es: _*)
    } yield a

  def add(es: Seq[Property]): MapGet[A] =
    for {
      a <- self
      _ <- MapGet.add(es)
    } yield a

  def remove(ids: Seq[Ident]): MapGet[A] =
    for {
      a <- self
      _ <- MapGet.remove(ids)
    } yield a

  def remove(id: Ident, ids: Ident*): MapGet[A] =
    for {
      a <- self
      _ <- MapGet.remove(id, ids: _*)
    } yield a
}
