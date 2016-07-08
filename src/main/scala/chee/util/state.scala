package chee.util

import scala.language.implicitConversions
import state.State._

object state {

  implicit def stateOps[S, A](s: State[S, A]) = StateOps(s)

  implicit def stateOptionOps[S, A](s: State[S, Option[A]]) = StateOptionOps(s)

  case class State[S, +A](run: S => (S, A)) { self =>
    def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B] { s =>
      val (next, a) = run(s)
      f(a).run(next)
    }

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def result(s: S): A = {
      val (_, a) = run(s)
      a
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (s, a))
  }

  trait States[S] {
    type Predicate = State[S, Boolean]

    def apply[V](f: S => (S, V)) = State(f)

    def unit[A](a: A): State[S, A] = State(s => (s, a))

    def combine[A, B, C](s1: State[S, A], s2: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- s1
        b <- s2
      } yield f(a, b)

    def seq[A](s: Seq[State[S, A]]): State[S, List[A]] =
      s.foldRight(unit(List[A]())) { (a, l) =>
        combine(a, l)(_ :: _)
      }

    def seq[A](s: State[S, A], more: State[S, A]*): State[S, List[A]] =
      seq(s +: more)

    def get: State[S, S] = State(s => (s, s))

    def set(state: S): State[S, Unit] = State(_ => (state, ()))

    def modify(f: S => S): State[S, Unit] =
      for {
        s <- get
        _ <- set(f(s))
      } yield ()

    def pair[A, B](s1: State[S, A], s2: State[S, B]): State[S, (A, B)] =
      combine(s1, s2)(_ -> _)

    def tuple3[A, B, C](s1: State[S, A], s2: State[S, B], s3: State[S, C]): State[S, (A, B, C)] =
      combine(pair(s1, s2), s3){ case ((a, b), c) => (a,b,c) }


    def not(p: Predicate): Predicate =
      p.map(b => !b)

    def and(ps: Seq[Predicate]): Predicate =
      ps.toStream.foldRight(unit(true))(combineAnd)

    def or(ps: Seq[Predicate]): Predicate =
      ps.toStream.foldRight(unit(false))(combineOr)

    private def boolCombine[S](stop: Boolean, f: (Boolean, Boolean) => Boolean)
      (p1: Predicate, p2: Predicate): Predicate = State { s =>
      val (next1, b1) = p1.run(s)
      if (b1 == stop) (next1, b1)
      else {
        val (next2, b2) = p2.run(next1)
        (next2, f(b1, b2))
      }
    }

    private def combineOr: (Predicate, Predicate) => Predicate =
      boolCombine(true, _ || _)

    private def combineAnd: (Predicate, Predicate) => Predicate =
      boolCombine(false, _ && _)
  }

  case class StateOps[S, A](s: State[S, A]) {
    private object states extends States[S]

    def modify(f: S => S): State[S, A] =
      for {
        a <- s
        _ <- states.modify(f)
      } yield a

    def combine[B, C](s2: State[S, B])(f: (A, B) => C) =
      states.combine(s, s2)(f)

    def when(p: State[S, Boolean]): State[S, Option[A]] = p.flatMap {
      case true => s.map(Some(_))
      case _ => State.unit(None)
    }

    def whenNot(p: State[S, Boolean]) = when(states.not(p))
  }

  case class StateOptionOps[S, A](s: State[S, Option[A]]) {
    def orElse(o: State[S, Option[A]]): State[S, Option[A]] =
      s.flatMap(a => if (a.isDefined) s else o)
  }
}
