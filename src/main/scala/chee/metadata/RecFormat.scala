package chee.metadata

object RecFormat {
  sealed trait Element
  sealed trait RecordEl extends Element
  sealed trait Entry extends Element

  case class Field(label: String, value: String, pos: Int) extends RecordEl
  case class Comment(text: String, pos: Int) extends RecordEl with Entry

  sealed trait Descriptor extends Entry
  object Descriptor {
    sealed trait Type {
      def name: String
      def definition: String
    }

    case object Empty extends Descriptor
    case class NonEmpty(els: Vector[RecordEl], pos: Int) extends Descriptor

    def apply(els: Vector[RecordEl], pos: Int): Descriptor =
      if (els.isEmpty) Empty else NonEmpty(els, pos)

    def apply(field: RecordEl*): Descriptor =
      if (field.isEmpty) Empty else NonEmpty(field.toVector, 0)
  }


  case class Record(els: Vector[RecordEl], descriptor: Descriptor, pos: Int) extends Entry {
    def + (f: RecordEl) = copy(els :+ f)

    lazy val fields: Vector[Field] = els.collect {
      case f: Field => f
    }

    def get(label: String): Vector[Field] =
      fields.filter(_.label == label)

    def valueOf(label: String): Vector[String] =
      get(label).map(_.value)

    def valueEq(label: String, test: String): Boolean =
      fields.find(_.value == test).isDefined

    def filter(p: Field => Boolean) =
      copy(els = els filter {
        case f: Field => p(f)
        case _ => false
      })
  }

  object Record {
    def apply(field: RecordEl*): Record =
      Record(field.toVector, Descriptor.Empty, 0)
  }

  object RecordId {
    def unapply(r: Record): Option[(String, Record)] =
      r.valueOf("Checksum").headOption match {
        case Some(c) => Some((c, r))
        case _ => None
      }
  }


  case class Database(els: Vector[Entry]) {
    def + (r: Entry) = Database(els :+ r)
    def ++ (d: Database) = Database(els ++ d.els)

    lazy val records = els.collect {
      case r: Record => r
    }

    def filter(p: Record => Boolean) =
      Database(els filter {
        case r: Record => p(r)
        case _ => false
      })

    def mapPf(pf: PartialFunction[Entry, Entry]): Database = Database {
      els.map(e => if (pf.isDefinedAt(e)) pf(e) else e)
    }
  }

  object Database {
    val Empty = Database(Vector.empty)
  }
}
