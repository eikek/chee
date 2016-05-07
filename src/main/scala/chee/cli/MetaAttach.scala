package chee.cli

import chee.conf._
import MetaAttach._
import chee.metadata._
import chee.properties.{ Ident, MapGet }
import com.typesafe.config.Config
import LsOptions.{ Opts => LsOpts }
import MapGet._

class MetaAttach extends ScoptCommand with AbstractLs with LockSupport {

  type T = Opts
  val name = "attach"
  val defaults = Opts()

  val parser = new Parser with LsOptions[Opts] {
    addLsOptions(_ updateLsOpts _)

    noteW("\nMeta options")
    opt[Seq[String]]("tags") valueName("tag1,tag2,…,tagN") action { (t, c) =>
      c.copy(tags = t.map(Tag.validated))
    } textW("The tags to attach. Use comma separated list of tag names.")

    opt[String]("comment") action { (co, c) =>
      c.copy(comment = co)
    } textW("A comment to attach.")

    opt[Unit]("drop-comment") action { (_, c) =>
      c.copy(dropComment = true)
    } textW ("Removes the comment. Cannot be uset with `--comment'.")

    opt[Unit]("drop-tags") action { (_, c) =>
      c.copy(dropTags = true)
    } textW ("Removes the tags. Cannot be uset with `--tags'.")

    opt[Unit]("drop-all") action { (_, c) =>
      c.copy(dropTags = true, dropComment = true)
    } textW("Drop tags and comments. This is short for `--drop-tags --drop-comment'.")

    addQuery(_ updateLsOpts _)

    checkConfig { cfg =>
      cfg.dropComment match {
        case true if cfg.comment.nonEmpty =>
          failure("Either --drop-comment or --comment is allowed.")
        case _ => success
      }
    }
    checkConfig { cfg =>
      cfg.dropTags match {
        case true if cfg.tags.nonEmpty =>
          failure("Either --drop-tags or --tags is allowed.")
        case _ => success
      }
    }
    checkConfig { cfg =>
      if (cfg.tags == defaults.tags &&
        cfg.dropTags == defaults.dropTags &&
        cfg.comment == defaults.comment &&
        cfg.dropComment == defaults.dropComment)
        failure("A modification of comment or tags is required.")
      else success
    }
  }

  def progress(msg: String) = valueForce(Ident.filename).map { f =>
    outln(msg.format(f))
  }

  def attachAction(opts: Opts): MapGet[Boolean] = {
    val tagAction = opts match {
      case TagOpts(true, _) => mapget.removeTags
      case TagOpts(_, tags) => mapget.setTags(tags)
      case _ => unit(())
    }
    val commentAction = opts match {
      case CommentOpts(true, _) => mapget.setComment("")
      case CommentOpts(_, c) => mapget.setComment(c)
      case x => unit(())
    }
    seq(progress("Attaching to %s …"), tagAction, commentAction).map(_ => true)
  }

  def exec(cfg: Config, opts: Opts) = {
    val results = find(cfg, opts.lsOpts)
    if (opts.dropEntry) {
      val data = MapGet.filter(results, progress("Drop metadata for %s …").map(_ => true))
      cfg.getMetadataFile.delete(data)
    } else {
      val data = MapGet.filter(results, attachAction(opts))
      cfg.getMetadataFile.write(data)
    }
    outln("Metadata file written.")
  }
}

object MetaAttach {

  case class Opts(
    lsOpts: LsOpts = LsOpts(),
    tags: Seq[Tag] = Seq.empty,
    comment: String = "",
    dropComment: Boolean = false,
    dropTags: Boolean = false
  ) {
    def updateLsOpts(f: LsOpts => LsOpts) =
      copy(lsOpts = f(lsOpts))

    def dropEntry = dropComment && dropTags
  }

  object TagOpts {
    def unapply(opts: Opts): Option[(Boolean, Seq[Tag])] =
      if (opts.tags.isEmpty && opts.dropTags == false) None
      else Some((opts.dropTags, opts.tags))
  }

  object CommentOpts {
    def unapply(opts: Opts): Option[(Boolean, String)] =
      if (opts.comment.isEmpty && opts.dropComment == false) None
      else Some((opts.dropComment, opts.comment))
  }

}
