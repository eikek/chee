package chee.query

import java.time.Duration
import chee.properties._

trait Progress[T, C] { self =>
  /** Called before an item is processed. */
  def before(n: C): MapGet[Unit]
  /** Called after an item has been processed. */
  def after(n: C, v: T, dur: Duration): MapGet[C]
  /** Called after all items have been processed. */
  def done(c: C, dur: Duration): Unit

  final def andThen(p: Progress[T, C]): Progress[T, C] =
    new Progress[T, C] {
      def before(n: C) = self.before(n).flatMap(_ => p.before(n))
      def after(n: C, v: T, dur: Duration) =
        self.after(n, v, dur).flatMap(x => p.after(x, v, dur))
      def done(count: C, dur: Duration): Unit = {
        self.done(count, dur)
        p.done(count, dur)
      }
    }

  /** Apply `action` to every element in `maps` invoking callbacks of this progress. */
  final def foreach(zero: C, zd: Duration = Duration.ZERO)(maps: Iterable[LazyMap], action: MapGet[T]): (C, Duration) =
    Progress.foreach(zero, zd)(this, maps, action)
}

object Progress {
  class DefaultProgress[T, C] extends Progress[T, C] {
    def before(n: C): MapGet[Unit] = MapGet.unit(())
    def after(n: C, v: T, d: Duration): MapGet[C] = MapGet.unit(n)
    def done(count: C, dur: Duration): Unit = ()
  }

  def before[T, C](m: MapGet[Unit]): Progress[T, C] = new DefaultProgress[T, C] {
    override def before(n: C) = m
  }

  def before[T, C](body: C => Any): Progress[T, C] = new DefaultProgress[T, C] {
    override def before(n: C): MapGet[Unit] = {
      body(n)
      MapGet.unit(())
    }
  }

  def after[T, C](f: (C, T, Duration) => C): Progress[T, C] = new DefaultProgress[T, C] {
    override def after(n: C, t: T, d: Duration): MapGet[C] = {
      MapGet.unit(f(n, t, d))
    }
  }

  def after[T, C](f: C => C): Progress[T, C] = new DefaultProgress[T, C] {
    override def after(n: C, t: T, d: Duration): MapGet[C] = {
      MapGet.unit(f(n))
    }
  }

  def done[T, C](f: (C, Duration) => Any): Progress[T, C] = new DefaultProgress[T, C] {
    override def done(c: C, d: Duration): Unit = f(c, d)
  }

  def empty[T, C]: Progress[T, C] = new DefaultProgress[T, C]

  def seq[T, C](ps: Progress[T, C]*): Progress[T, C] =
    ps.reduce { (a, b) => a andThen b }

  def foreach[T, C](zero: C, zd: Duration = Duration.ZERO)(p: Progress[T, C], maps: Iterable[LazyMap], action: MapGet[T]) = {
    val a: C => MapGet[C] = n => action.around(p.before(n), p.after(n, _, _))
    val t = chee.Timing.timedResult(MapGet.fold(zero, maps)(a))
    p.done(t._1, zd.plus(t._2))
    t
  }
}
