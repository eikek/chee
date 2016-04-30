package chee.cli

import com.typesafe.config.Config
import better.files._
import chee.conf._
import chee.{Collection, CollectionConf}
import CollectionEdit.Opts

class CollectionEdit extends ScoptCommand {

  val name = "edit"

  type T = Opts

  val defaults = Opts()

  val parser = new Parser {
    noteW("\nOptions are optional, if not specified an editor opens to edit a "+
      "collection or create a new one. If any option is given, the value is "+
      "set non-interactively.\n")

    opt[String]("name") action { (n, c) =>
      c.copy(newName = Some(n))
    } text ("Set a new name.")

    opt[String]("query") action { (q, c) =>
      c.copy(newQuery = Some(q))
    } text ("Set a new query.")

    opt[String]("description") action { (d, c) =>
      c.copy(newDescription = Some(d))
    } text ("Set a new description.")

    arg[String]("<name>") required() action { (n, c) =>
      c.copy(name = n)
    } textW ("The collection name to edit. Creates a new collection if there is none with this name.")
  }

  private def createTemplate(title: String, coll: Collection) = (s"""# $title
    |# Lines starting with # are ignored. Fill the corresponding fields below
    |# the separator lines (---- … follows this line). Then save the file and
    |# exit to save the data. You can cancel by not changing anything or
    |# removing the query or name. Then the collection is not updated.
    |------------- name follows this line -------------------
    |${coll.name}
    |
    |------------- query follows this line ------------------
    |${coll.query}
    |
    |------------- description follows this line ------------
    |${coll.description}""").stripMargin

  def execInteractive(cfg: Config, opts: Opts): Unit = {
    val conf = cfg.getCollectionConf
    val original = conf.get(opts.name).get
    val name = original.map(_.name) getOrElse (opts.name)
    val temp = prepareFile(name, original)
    if (edit(cfg, temp)) {
      CollectionFileParser.parse(temp) match {
        case c@Some(collection) if c != original =>
          logger.debug(s"Save collection from file ${temp.path}")
          outln(s"Save collection ${collection.name}")
          cfg.makeQuery(collection.query) match {
            case Right(_) =>
              conf.replace(original.map(_.name).getOrElse(""), collection)
            case Left(msg) => chee.UserError(msg)
          }
        case _ =>
          outln("File not modified.")
      }
    } else {
      errln("Editor returned non-zero")
    }
    temp.delete()
  }

  def execAuto(cfg: Config, opts: Opts): Unit = {
    val conf = cfg.getCollectionConf
    conf.get(opts.name).get match {
      case Some(coll) =>
        val newColl = coll.copy(
          name = opts.newName.getOrElse(coll.name),
          query = opts.newQuery.getOrElse(coll.query),
          description = opts.newDescription.getOrElse(coll.description)
        )

        if (coll.query != newColl.query) cfg.makeQuery(newColl.query) match {
          case Left(msg) => chee.UserError(msg)
          case _ =>
        }

        if (coll == newColl) outln("Collection not modified")
        else conf.replace(coll.name, newColl)
        outln(s"Collection ${newColl.name} saved.")

      case None =>
        if (opts.newQuery.isEmpty) {
          errln(s"Collection not found: ${opts.name}")
          errln("Cannot create a new collection, because `query' is missing.")
        } else {
          conf.add(Collection(opts.name, opts.newQuery.get, opts.newDescription.getOrElse("")))
          outln(s"New collection ${opts.name} saved.")
        }
    }
  }

  def exec(cfg: Config, opts: Opts): Unit = {
    opts match {
      case Opts(name, None, None, None) =>
        execInteractive(cfg, opts)
      case _ =>
        execAuto(cfg, opts)
    }
  }

  def prepareFile(name: String, original: Option[Collection]): File = {
    val temp = File.newTemporaryFile(s"chee-$name-", ".coll")
    original match {
      case Some(c) =>
        createTemplate(s"Edit collection '${c.name}'", c) `>:` temp
      case None =>
        createTemplate("Create new collection", Collection(name, "", "")) `>:` temp
    }
  }

  def edit(cfg: Config, file: File): Boolean = {
    import scala.collection.JavaConverters._
    val cmd = getEditor(cfg) :+ file.path.toString
    logger.trace(s"""Running edit command `${cmd.mkString(" ")}'""")
    val pb = new ProcessBuilder(cmd.asJava);
    pb.redirectOutput(ProcessBuilder.Redirect.INHERIT);
    pb.redirectError(ProcessBuilder.Redirect.INHERIT);
    pb.redirectInput(ProcessBuilder.Redirect.INHERIT);
    pb.start().waitFor() == 0
  }

  def getEditor(cfg: Config): Seq[String] = {
    val editor = Option(cfg.getString("chee.programs.editor")).filter(_.nonEmpty) orElse {
      Option(System.getenv.get("EDITOR")).filter(_.nonEmpty)
    }
    editor match {
      case Some(edit) =>
        logger.trace(s"Using editor `$edit'")
        edit.split("\\s+").toSeq
      case None =>
        chee.UserError {
          """No editor found.
          |Please set an editor command in the config file under key
          |`chee.programs.editor' or set the environment variable `EDITOR'.""".stripMargin
        }
    }
  }

  object CollectionFileParser {
    sealed trait Pos
    case object Name extends Pos
    case object Query extends Pos
    case object Description extends Pos
    case object Undefined extends Pos

    val separator = """---+\s?(name|query|description).*?$""".r

    def parse(f: File): Option[Collection] =
      parseLines(f.lines)

    def parseLines(lines: Traversable[String]): Option[Collection] = {
      val text = lines filterNot comments
      val coll = Collection("","","")
      val (c, pos) = text.foldLeft((coll, Undefined:Pos)) { case ((c, pos), line) =>
        line match {
          case separator("name") => (c, Name)
          case separator("query") => (c, Query)
          case separator("description") => (c, Description)
          case _ => pos match {
            case Query => (c.copy(query = (c.query + " " + line).trim), pos)
            case Name => (c.copy(name = (c.name +" "+ line).trim), pos)
            case Description => (c.copy(description = c.description +"\n"+ line), pos)
            case Undefined => (c, pos)
          }
        }
      }
      if (c.name.nonEmpty && c.query.nonEmpty) Some(c.copy(description = c.description.trim))
      else None
    }

    val comments: String => Boolean =
      s => s.startsWith("#")
  }
}

object CollectionEdit {
  case class Opts(
    name: String = "",
    newName: Option[String] = None,
    newQuery: Option[String] = None,
    newDescription: Option[String] = None)
}
