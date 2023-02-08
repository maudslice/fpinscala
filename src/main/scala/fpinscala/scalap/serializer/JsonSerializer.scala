package fpinscala.scalap.serializer


/**
  * @date 2023-02-07 15:08
  * @author chenzhr
  * @Description
  */
trait JsonSerializer[T]:
  def serialize(o: T): String

  extension (a: T)
    def toJson: String = serialize(a)
end JsonSerializer

object JsonSerializer:
  given stringSerializer: JsonSerializer[String] with
    override def serialize(o: String): String = s"\"$o\""

  given intSerializer: JsonSerializer[Int] with
    override def serialize(o: Int): String = o.toString

  given longSerializer: JsonSerializer[Long] with
    override def serialize(o: Long): String = o.toString

  given booleanSerializer: JsonSerializer[Boolean] with
    override def serialize(o: Boolean): String = o.toString

  given listSerializer[T](using JsonSerializer[T]): JsonSerializer[List[T]] with
    override def serialize(o: List[T]): String =
      s"[${o.map(_.toJson).mkString(", ")}]"

end JsonSerializer
