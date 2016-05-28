package chee.util

/** A mustache template parser and renderer.
  *
  * To make it compatible to {{{LazyMap}}} that can updates itself
  * when retrieving values, the context given to a template is
  * implemented similiar.
  */
object mustache {
  trait Context {
    def find(key: String): (Context, Option[Value])
    def :: (head: Context): Context = new StackedContext(List(head, this))
    def tail: Context = this match {
      case c: StackedContext =>
        val rest = c.cs.tail
        if (rest.isEmpty) this
        else new StackedContext(rest)
      case _ => this
    }
  }

  private case class StackedContext(val cs: List[Context]) extends Context {
    def find(key: String): (Context, Option[Value]) = {
      @annotation.tailrec
      def loop(in: List[Context], top: List[Context] = Nil): (List[Context], Option[Value]) =
        in match {
          case Nil => (top, None)
          case c :: cs => c.find(key) match {
            case (cnext, None) => loop(cs, cnext :: top)
            case (cnext, v@Some(_)) => ((cnext :: top).reverse ::: in, v)
          }
        }
      val (ncs, v) = loop(cs)
      (new StackedContext(ncs), v)
    }
    override def :: (head: Context) = new StackedContext(head :: cs)
  }

  object Context {
    val empty: Context = new Context {
      def find(key: String) = (this, None)
    }

    def apply(ts: (String, Value)*): Context = new Context {
      val data = Map(ts: _*)
      def find(key: String) = (this, data.get(key))
    }

    def apply(f: String => Option[Value]): Context = new Context {
      def find(key: String) = (this, f(key))
    }
  }
  case class ContextGet[V](run: Context => (Context, V)) {
    def flatMap[B](f: V => ContextGet[B]): ContextGet[B] = ContextGet { ctx =>
      val (next, v) = run(ctx)
      f(v).run(next)
    }
    def map[B](f: V => B): ContextGet[B] =
      flatMap(v => ContextGet.unit(f(v)))

    def result(c: Context): V = {
      val (_, v) = run(c)
      v
    }

    def andThen(next: ContextGet[_]): ContextGet[Unit] =
      for {
        _ <- this
        _ <- next
      } yield ()

    def stacked(c: Context): ContextGet[V] =
      for {
        _ <- ContextGet.stack(c)
        v <- this
        _ <- ContextGet.pop
      } yield v
  }

  object ContextGet {
    def unit[V](v: V): ContextGet[V] = ContextGet(c => (c, v))
    def find(key: String): ContextGet[Option[Value]] = ContextGet(_.find(key))
    def findOrEmpty(key: String): ContextGet[Value] =
      find(key).map(_.getOrElse(Value.of(false)))
    def set(c: Context): ContextGet[Unit] = ContextGet(_ => (c, ()))
    def get: ContextGet[Context] = ContextGet(c => (c, c))
    def modify(f: Context => Context): ContextGet[Unit] =
      for {
        c <- get
        _ <- set(f(c))
      } yield ()
    def stack(head: Context): ContextGet[Unit] =
      modify(c => head :: c)
    def pop: ContextGet[Unit] =
      modify(_.tail)
    def setHead(c: Context): ContextGet[Unit] =
      modify {
        case StackedContext(a :: as) => StackedContext(c :: as)
        case _ => c
      }
  }

  sealed trait Value {
    def isEmpty: Boolean
    def asContext: Context = this match {
      case MapValue(v, _) => v
      case _ => Context(key => if (key == ".") Some(this) else None)
    }
  }
  object Value {
    import language.implicitConversions
    implicit def _fromString(s: String): Value = SimpleValue(s)
    implicit def _fromBoolean(b: Boolean): Value = BoolValue(b)

    def of(s: String): Value = SimpleValue(s)
    def of(s: Option[String]): Value = SimpleValue(s getOrElse "")
    def of(b: Boolean): Value = BoolValue(b)
    def list(vs: Value*): Value = ListValue(vs)
    def map(vs: (String, Value)*): Value = MapValue(Context(vs: _*), vs.isEmpty)
    def lambda(f: Section => ContextGet[String]): Value = LambdaValue(f)
  }
  case class SimpleValue(v: String) extends Value {
    val isEmpty = v.isEmpty
  }
  case class BoolValue(v: Boolean) extends Value {
    val isEmpty = v == false
  }
  case class ListValue(v: Seq[Value]) extends Value {
    lazy val isEmpty = v.isEmpty
  }
  case class MapValue(ctx: Context, isEmpty: Boolean) extends Value
  case class LambdaValue(f: Section => ContextGet[String]) extends Value {
    val isEmpty = false
  }


  sealed trait Element {
    def asString: String = ToString(this)
  }
  case class Literal(text: String) extends Element
  case class Variable(key: String, unescape: Boolean = false) extends Element
  case class Section(key: String, inner: Seq[Element], inverted: Boolean = false) extends Element
  case class Template(els: Seq[Element]) {
    def render(c: Context)(implicit r: Expand[Seq[Element]]): String = {
      val b = new StringBuilder
      r(b append _)(els).result(c)
      b.toString
    }
    def renderTo(c: Context, f: String => Unit)(implicit r: Expand[Seq[Element]]): Unit =
      r(f)(els).result(c)

    def asString: String = ToString(this)
  }
  object Template {
    def apply(e: Element, es: Element*): Template = Template(e +: es)
    def parse(in: String) = Parser.parse(in)
  }

  trait Expand[T] {
    def apply(consume: String => Unit)(e: T): ContextGet[Unit]
    def asString(e: T): ContextGet[String] = ContextGet { ctx =>
      val buf = new StringBuilder
      val (next, _) = apply(buf append _)(e).run(ctx)
      (next, buf.toString)
    }
  }
  object Expand {
    def apply[T <: Element](f: T => ContextGet[String]): Expand[T] = new Expand[T] {
      def apply(consume: String => Unit)(e: T): ContextGet[Unit] =
        f(e).map(consume)
    }

    // only &, <, >, ", and '
    def escapeHtml(s: String): String =
      "&<>\"'".foldLeft(s) { (s, c) =>
        s.replace(c.toString, s"&#${c.toInt};")
      }

    implicit val literalExpand: Expand[Literal] = Expand(e => ContextGet.unit(e.text))

    implicit val variableExpand: Expand[Variable] = Expand {
      case Variable(key, unescape) => ContextGet.findOrEmpty(key).map {
        case SimpleValue(s) => if (unescape) s else escapeHtml(s)
        case BoolValue(b) => if (b) b.toString else "" //??? todo really? seems strange
        case MapValue(_, e) =>
          val s = if (e) "<empty object>" else "<non-empty object>"
          if (unescape) s else escapeHtml(s)
        case ListValue(x) =>
          if (unescape) x.toString else escapeHtml(x.toString)
        case _: LambdaValue =>
          if (unescape) "<lambda>" else escapeHtml("<lambda>")
      }
    }

    implicit lazy val sectionExpand: Expand[Section] = new Expand[Section] {
      def apply(consume: String => Unit)(s: Section): ContextGet[Unit] = {
        val expandInner: ContextGet[Unit] = {
          val r = seqElementExpand
          r(consume)(s.inner)
        }
        ContextGet.findOrEmpty(s.key).flatMap {
          case v if s.inverted =>
            if (v.isEmpty) expandInner
            else ContextGet.unit(())
          case ListValue(vs) =>
            val list = vs.map(v => expandInner.stacked(v.asContext))
            list.foldLeft(ContextGet.unit(()))(_ andThen _)
          case LambdaValue(f) =>
            f(s).map(consume)
          case v if !v.isEmpty =>
            expandInner.stacked(v.asContext)
          case _ => ContextGet.unit(())
        }
      }
    }

    implicit def elementExpand(implicit el: Expand[Literal], ev: Expand[Variable], es: Expand[Section]): Expand[Element] = new Expand[Element] {
      def apply(consume: String => Unit)(e: Element): ContextGet[Unit] = e match {
        case e: Literal => el(consume)(e)
        case e: Variable => ev(consume)(e)
        case e: Section => es(consume)(e)
      }
    }

    implicit def seqElementExpand(implicit r: Expand[Element]): Expand[Seq[Element]] =
      new Expand[Seq[Element]] {
        def apply(consume: String => Unit)(es: Seq[Element]): ContextGet[Unit] =
          es.map(r(consume)).foldLeft(ContextGet.unit(()))(_ andThen _)
      }
  }

  object ToString {
    import Expand.{literalExpand, elementExpand, seqElementExpand}
    implicit val variableExpand: Expand[Variable] = Expand {
      case Variable(key, unescape) =>
        ContextGet.unit(s"""{{${if (unescape) "& " else ""}${key}}}""")
    }
    implicit lazy val sectionExpand: Expand[Section] = new Expand[Section] {
      def apply(consume: String => Unit)(s: Section): ContextGet[Unit] = {
        val r = seqElementExpand
        val start = ContextGet.unit(s"""{{${if (s.inverted) "^" else "#"}${s.key}}}""")
        val end = ContextGet.unit(s"{{/${s.key}}}")
        List(start.map(consume), r(consume)(s.inner), end.map(consume)).reduce(_ andThen _)
      }
    }
    def apply(e: Element): String = {
      val r = implicitly[Expand[Element]]
      r.asString(e).result(Context.empty)
    }
    def apply(t: Template): String = {
      t.render(Context.empty)
    }
  }

  object Parser {
    import fastparse.all._
    import parsing._

    val deliStart: P0 = "{{"
    val deliEnd: P0 = "}}"
    val key: P[String] = P(alphanumPlus("_-.:").rep(1)).!
    val literal: P[Literal] = P(!deliStart ~ AnyChar).rep(1).!.map(Literal(_))
    val variable: P[Variable] = P(deliStart ~ "& ".!.? ~ key ~ deliEnd).map {
      case (unescape, name) => Variable(name, unescape.isDefined)
    }
    def section: P[Section] = P(deliStart ~ ("^" | "#").! ~ key ~ deliEnd).flatMap {
      case (prefix, name) => P(element.rep ~ deliStart ~ "/" ~ name ~ deliEnd).map {
        els => Section(name, els, prefix == "^")
      }
    }
    def element: P[Element] = P(section | variable | literal)
    def template: P[Template] = element.rep(1).map(Template(_))
    def parse(in: String): Either[String, Template] =
      template.parseAll(in)
  }
}
