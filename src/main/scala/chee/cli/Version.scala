package chee.cli

import com.typesafe.config.Config
import chee.doc.CheeDocInfo

object Version extends Command {

  val name = "version"

  def exec(cfg: Config, args: Array[String]): Unit = {
    outln(s"${CheeDocInfo.projectName} v${CheeDocInfo.version} " +
      s"(build ${CheeDocInfo.commit}, ${CheeDocInfo.buildTime})")
  }
}
