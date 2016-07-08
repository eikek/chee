package chee.cli

import com.typesafe.config.Config
import chee.CheeApi

class Info extends Command {
  val name = "info"

  def exec(cfg: Config, args: Array[String]): Unit = {
    val api = CheeApi(cfg)
    val info = api.getLocationInfo.get

    for ((loc, count) <- info.count.toSeq.sortBy(_._1.pathAsString)) {
      outln(s"$loc: $count")
    }
    outln(s"All: ${info.count.values.sum}")
  }
}
