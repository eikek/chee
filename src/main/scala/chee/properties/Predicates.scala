package chee.properties

import better.files._
import com.typesafe.scalalogging.LazyLogging
import MapGet._

object Predicates extends LazyLogging {

  val True = unit(true)
  val False = unit(false)

  def exists(id: Ident): MapGet[Boolean] =
    find(id).map(_.isDefined)

  def fileExists: MapGet[Boolean] =
    existingPath.map(_.isDefined)

  def checksumMatch(file: File): Predicate =
    value(Ident.checksum).map { s =>
      s == ChecksumExtract.checksum(file)
    }

  def checksumMatch(m1: LazyMap, m2: LazyMap): (LazyMap, LazyMap, Boolean) = {
    val (nextM1, cs1) = value(Ident.checksum).run(m1)
    val (nextM2, cs2) = value(Ident.checksum).run(m2)
    (nextM1, nextM2, cs1 == cs2)
  }

  def prop(p: Prop): Predicate = {
    require(Comp.isDefault(p.comp), s"Comparator `${p.comp.name} not supported.")
    val conv = Value.forIdent(p.ident)
    logger.trace(s"Compare $p with conversion $conv")
    value(p.ident).map {
      case None => false
      case Some(v) =>
        conv.evalString(v, p.comp, p.value) match {
          case Right(b) => b
          case Left(err) => sys.error(err)
        }
    }
  }

  def identprop(p: IdentProp): Predicate =
    value(p.id2).flatMap {
      case Some(ov2) =>
        prop(Prop(p.comp, p.id1 -> ov2))
      case None =>
        unit(false)
    }

  def in(in: In): Predicate =
    value(in.id).map {
      case Some(value) =>
        val contains = in.values.map(_.toLowerCase).toSet
        contains(value)
      case None => false
    }

  def junc(op: Junc.Op, ps: Predicate*): Predicate =
    op match {
      case Junc.And => and(ps.toList)
      case Junc.Or => or(ps.toList)
    }

  def apply(c: Condition): Predicate =
    Pred.createPredicate(c)

  trait Pred[A] {
    def pred(a: A): Predicate
  }

   object Pred {
    def apply[A](f: A => Predicate): Pred[A] = new Pred[A] {
      def pred(a: A) = f(a)
    }

    implicit val _trueCondition: Pred[TrueCondition.type] = Pred(_ => True)
    implicit val _exists: Pred[Exists] = Pred {
      case Exists(id) => exists(id)
    }
    implicit val _prop: Pred[Prop] = Pred(prop)
    implicit val _identProp: Pred[IdentProp] = Pred(identprop)
    implicit val _inPred: Pred[In] = Pred(in)

    implicit def _juncPred: Pred[Junc] = Pred {
      case Junc(op, nodes) =>
        if (nodes.isEmpty) True
        else {
          val r = implicitly[Pred[Condition]]
          junc(op, nodes.map(r.pred): _*)
        }
    }
    implicit def _notPred: Pred[Not] = Pred {
      case Not(c) =>
        val r = implicitly[Pred[Condition]]
        not(r.pred(c))
    }

    implicit def _conditionPred(implicit
      r0: Pred[TrueCondition.type],
      r1: Pred[Exists],
      r2: Pred[Prop],
      r3: Pred[IdentProp],
      r4: Pred[Not],
      r5: Pred[Junc],
      r6: Pred[In]): Pred[Condition] =
      Pred {
        case c@TrueCondition => r0.pred(c)
        case c: Exists => r1.pred(c)
        case c: Prop => r2.pred(c)
        case c: IdentProp => r3.pred(c)
        case c: Not => r4.pred(c)
        case c: Junc => r5.pred(c)
        case c: In => r6.pred(c)
      }

    def createPredicate(c: Condition): Predicate =
      _conditionPred.pred(c)
  }
}
