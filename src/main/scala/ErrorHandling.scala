import scala.util.Try

object ErrorHandling {

  case class User(id: Int, name: String, email: Option[String])

  val users = List(
    User(1, "Alice", Some("alice@example.com")),
    User(2, "Bob", None),
    User(3, "Carol", Some("carol@example.com"))
  )

  def findUserEmail(id: Int): Option[String] =
    users.find(u => u.id == id).flatMap(_.email)

  // ===== Exercise 2: Either =====
  def validateEmail(email: String): Either[String, String] = {
    if (email.length < 5) Left("Email too short")
    else if (!email.contains("@")) Left("Email must contain @")
    else Right(email)
  }

  def registerUser(name: String, email: String): Either[String, User] =
    validateEmail(email).flatMap { e =>
      Right(User(0, name, Some(e)))
  }

    // ===== Exercise 3: Try =====
    // Try wraps code that might throw an exception
    // Success(value) or Failure(exception)

  def parseAndDouble(input: String): Either[String, Int] = {
     Try(input.toInt * 2)
       .toEither
       .left.map(e => s"Cannot parse '$input' as number")
  }

  def main(args: Array[String]): Unit = {
    println(findUserEmail(1))
    println(findUserEmail(2))
    println(findUserEmail(99))
    println(registerUser("Alice", "alice@example.com"))
    println(registerUser("Bob", "bob"))
    println(registerUser("Carol", "notanemail"))
    println(parseAndDouble("21")) // Right(42)
    println(parseAndDouble("abc")) // Left(Cannot parse 'abc' as number)
    println(parseAndDouble("")) // Left(Cannot parse '' as number)
  }

}