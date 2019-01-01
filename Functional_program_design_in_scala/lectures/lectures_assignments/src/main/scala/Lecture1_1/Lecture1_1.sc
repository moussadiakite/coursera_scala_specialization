object JsonRepresentation{

  abstract class JSON

  case class JSeq(elems: List[JSON]) extends JSON

  case class JObj(bindings: Map[String, JSON]) extends JSON

  case class JNum(num: Double) extends JSON

  case class JStr(str: String) extends JSON

  case class JBool(b: Boolean) extends JSON

  case object JNull extends JSON

  def show(json: JSON): String = json match {
    case JSeq(elems) => "[\n" +
                        (elems map show).mkString(",\n") +
                        "\n]"
    case JObj(bindings) => "{\n" + (for{
                           (str, json) <- bindings
                           } yield "\"" + str + "\": " + show(json)).mkString(",\n") +
                           "\n}"
    case JNum(num) => num.toString
    case JStr(str) => "\"" + str + "\""
    case JBool(b) => b.toString
    case JNull => "Null"
  }

  val data = JObj(Map(
    "firstName" -> JStr("Jonh"),
    "lastName" -> JStr("Smith"),
    "address" -> JObj(Map(
      "streetAddress" -> JStr("21 2nd street"),
      "state" -> JStr("NY"),
      "postalCode" -> JNum(10021)
    )),
    "phoneNumbers" -> JSeq(List(
      JObj(Map(
        "type" -> JStr("home"), "number" -> JStr("212 555-1234")
      )),
      JObj(Map(
        "type" -> JStr("fax"), "number" -> JStr("646 555-4567")
      ))
    ))
  ))

  show(data)
}