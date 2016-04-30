package chee.it

import chee.cli.Command
import com.typesafe.scalalogging.LazyLogging

trait ErrorLogger extends Command {
  self: LazyLogging =>

  override def errln(m: String): Unit = {
    super.errln(m)
    logger.error(m)
  }
}
