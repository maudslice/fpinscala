package fpinscala.scalap.serializer

/**
  * @date 2023-02-07 15:17
  * @author chenzhr
  * @Description
  */
object ToJsonMethods:
  extension [T](a: T)(using jser: JsonSerializer[T])
    def toJson: String  = jser.serialize(a)
