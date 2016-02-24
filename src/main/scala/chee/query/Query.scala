package chee.query

import chee.properties._
import chee.Collection

case class QuerySettings(comps: Set[Comp], transform: Transform)

object QuerySettings {
  def apply(now: LocalDateTime, colls: Seq[Collection] = Seq.empty): QuerySettings =
    if (colls.isEmpty) QuerySettings(Comp.all, Transform.makeChain(now))
    else QuerySettings(Comp.all, Transform.withCollectionMacro(colls))
}

trait Query {
  def apply(in: String): Either[String, Condition]
  def parse(in: String): Either[String, Condition]
  def process(tree: Condition): Either[String, Condition]
}

object Query {

  def create(settings: QuerySettings): Query = new QueryImpl(settings)

  private class QueryImpl(settings: QuerySettings) extends Query {
    val parser: String => Either[String, Condition] =
      QueryParser(_, settings.comps ++ settings.transform.comps)

    def parse(in: String) = parser(s"(& $in )")

    def apply(in: String) =
      parse(in).right flatMap process

    def process(tree: Condition): Either[String, Condition] =
      try {
        Right(Condition.normalize(settings.transform(tree)))
      } catch {
        case e: Exception => Left(e.getMessage)
      }
  }
}
