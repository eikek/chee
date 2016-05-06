package chee.query

import chee.metadata.MetadataFile
import chee.properties._
import chee.Collection
import com.typesafe.scalalogging.LazyLogging

case class QuerySettings(comps: Set[Comp], transform: Transform)

object QuerySettings {
  def apply(now: LocalDateTime, mf: MetadataFile = MetadataFile.empty, colls: Seq[Collection] = Seq.empty): QuerySettings =
    if (colls.isEmpty) QuerySettings(Comp.all, Transform.makeChain(now, mf))
    else QuerySettings(Comp.all, Transform.withCollectionMacro(colls, mf))
}

trait Query {
  def apply(in: String): Either[String, Condition]
  def parse(in: String): Either[String, Condition]
  def process(tree: Condition): Either[String, Condition]
}

object Query {

  def create(settings: QuerySettings): Query = new QueryImpl(settings)

  private class QueryImpl(settings: QuerySettings) extends Query with LazyLogging {
    val parser = new QueryParser(settings.comps ++ settings.transform.comps)

    def parse(in: String) = {
      val q = s"(& $in )"
      logger.trace(s"Parse query: $q")
      parser.parse(q)
    }

    def apply(in: String) =
      parse(in).right flatMap process

    def process(tree: Condition): Either[String, Condition] =
      try {
        logger.trace(s"Processing condition tree: $tree")
        val c = Right(Condition.normalize(settings.transform(tree)))
        logger.trace(s"Processed tree: $c")
        c
      } catch {
        case e: Exception => Left(e.getMessage)
      }
  }
}
