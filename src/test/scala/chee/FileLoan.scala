package chee

import better.files._
import java.util.UUID

trait FileLoan {

  def withNewFile(code: File => Any): Unit = {
    val file = chee.TestInfo.targetDir / UUID.randomUUID().toString
    file.path.toFile.deleteOnExit()
    try code(file) finally {
      if (file.exists)
        file.delete()
    }
  }

  def withExistingFile(code: File => Any): Unit = withNewFile { f =>
    f.createIfNotExists()
    code(f)
  }
}
