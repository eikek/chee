package chee

class LoggingProperty extends ch.qos.logback.core.PropertyDefinerBase {

  @scala.beans.BeanProperty
  var name: String = ""

  def getPropertyValue(): String = name match {
    case "logfile" => CheeConf.config.getString("chee.logFile")
    case "loglevel" => CheeConf.config.getString("chee.logLevel")
    case _ => ""
  }
}
