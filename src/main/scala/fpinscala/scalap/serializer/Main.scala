package fpinscala.scalap.serializer

/**
  * @date 2023-02-07 16:15
  * @author chenzhr
  * @Description
  */
object Main:
  def main(args: Array[String]): Unit = {
    val addresses = AddressBook(
      List(
        Contact(
          "Bob Smith", List(
            Address(
              "12345 Main Street",
              "San Francisco",
              "CA",
              94105
            ), Address(
              "500 State Street",
              "Los Angeles",
              "CA",
              90007
            )
          ), List(
            Phone(
              1,
              5558881234
            ), Phone(
              49,
              5558413323
            )
          )
        )
      )
    )

    println(addresses.toJson)
  }
