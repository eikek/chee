package chee

/** An error due to user input */
class UserError(msg: String) extends RuntimeException(msg)

object UserError {

  def apply(msg: String): Nothing = throw create(msg)

  def create(msg: String) = new UserError(msg)
}
