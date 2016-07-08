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

  final def setDone(f: (C, Duration) => Unit): Progress[T, C] =
    Progress.setDone(this, f)

  final def setBefore(f: C => MapGet[Unit]): Progress[T, C] =
    Progress.setBefore(this, f)

  final def setAfter(f: (C, T, Duration) => MapGet[C]): Progress[T, C] =
    Progress.setAfter(this, f)

  /** Apply `action` to every element in `maps` invoking callbacks of this progress. */
  final def foreach(zero: C, zd: Duration = Duration.ZERO)(maps: Traversable[LazyMap], action: MapGet[T]): (C, Duration) =
    Progress.foreach(zero, zd)(this, maps, action)
}

object Progress {
  class EmptyProgress[T, C] extends Progress[T, C] {
    def before(n: C): MapGet[Unit] = MapGet.unit(())
    def after(n: C, v: T, d: Duration): MapGet[C] = MapGet.unit(n)
    def done(count: C, dur: Duration): Unit = ()
  }

  class DelegateProgress[T, C](p: Progress[T, C]) extends Progress[T, C] {
    def before(n: C): MapGet[Unit] = p.before(n)
    def after(n: C, v: T, d: Duration): MapGet[C] = p.after(n, v, d)
    def done(count: C, dur: Duration): Unit = p.done(count, dur)
  }

  def setDone[T, C](p: Progress[T, C], f: (C, Duration) => Unit): Progress[T, C] =
    new DelegateProgress(p) {
      override def done(c: C, d: Duration) = f(c, d)
    }

  def setBefore[T, C](p: Progress[T, C], f: C => MapGet[Unit]): Progress[T, C] =
    new DelegateProgress(p) {
      override def before(n: C) = f(n)
    }

  def setAfter[T, C](p: Progress[T, C], f: (C, T, Duration) => MapGet[C]): Progress[T, C] =
    new DelegateProgress(p) {
      override def after(n: C, v: T, d: Duration) = f(n, v, d)
    }

  def before[T, C](m: MapGet[Unit]): Progress[T, C] = new EmptyProgress[T, C] {
    override def before(n: C) = m
  }

  def before[T, C](body: C => Any): Progress[T, C] = new EmptyProgress[T, C] {
    override def before(n: C): MapGet[Unit] = {
      body(n)
      MapGet.unit(())
    }
  }

  def after[T, C](f: (C, T, Duration) => C): Progress[T, C] = new EmptyProgress[T, C] {
    override def after(n: C, t: T, d: Duration): MapGet[C] = {
      MapGet.unit(f(n, t, d))
    }
  }

  def after[T, C](f: C => C): Progress[T, C] = new EmptyProgress[T, C] {
    override def after(n: C, t: T, d: Duration): MapGet[C] = {
      MapGet.unit(f(n))
    }
  }

  def done[T, C](f: (C, Duration) => Any): Progress[T, C] = new EmptyProgress[T, C] {
    override def done(c: C, d: Duration): Unit = f(c, d)
  }

  def empty[T, C]: Progress[T, C] = new EmptyProgress[T, C]

  def count[T](implicit f: T => Boolean): Progress[T, Int] = after((c, t, _) => if (f(t)) c + 1 else c)

  def seq[T, C](ps: Progress[T, C]*): Progress[T, C] =
    ps.reduce { (a, b) => a andThen b }

  def foreach[T, C](zero: C, zd: Duration = Duration.ZERO)(p: Progress[T, C], maps: Traversable[LazyMap], action: MapGet[T]): (C, Duration) = {
    val a: C => MapGet[C] = n => action.around(p.before(n), p.after(n, _, _))
    val (t, d) = chee.Timing.timedResult(MapGet.fold(zero, maps)(a))
    val time = zd.plus(d)
    p.done(t, time)
    (t, time)
  }
}
