package chee

import chee.conf.CheeConf

class LoggingProperty extends ch.qos.logback.core.PropertyDefinerBase {

  private val config = CheeConf.defaultConfig

  @scala.beans.BeanProperty
  var name: String = ""

  def getPropertyValue(): String = name match {
    case "logfile" => config.getString("chee.logFile")
    case "loglevel" => config.getString("chee.logLevel")
    case _ => ""
  }
}
