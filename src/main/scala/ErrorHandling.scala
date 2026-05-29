object ErrorHandling {

  case class User(id: Int, name: String, email: Option[String])

  val users = List(
    User(1, "Alice", Some("alice@example.com")),
    User(2, "Bob", None),
    User(3, "Carol", Some("carol@example.com"))
  )

  def findUserEmail(id: Int): Option[String] =
    users.find(u => u.id == id).flatMap(_.email)

  def main(args: Array[String]): Unit = {
    println(findUserEmail(1))
    println(findUserEmail(2))
    println(findUserEmail(99))
  }

}