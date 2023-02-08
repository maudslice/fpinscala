package fpinscala.scalap.serializer

/**
  * @date 2023-02-07 15:25
  * @author chenzhr
  * @Description
  */
case class Address(street: String, city: String, state: String, zip: Int)

object Address:
  given addressSerializer: JsonSerializer[Address] with
    override def serialize(o: Address): String =
      import ToJsonMethods.toJson as asJson
      s"""
        |{
        |  "street":${o.street.asJson},
        |  "city":${o.city.asJson},
        |  "state":${o.state.asJson},
        |  "zip":${o.zip.asJson}
        |}
        |""".stripMargin
end Address

case class Phone(countryCode: Int, phoneNumber: Long)

object Phone:
  given phoneSerializer: JsonSerializer[Phone] with
    override def serialize(o: Phone): String =
      import ToJsonMethods.toJson as asJson
      s"""
         |{
         |  "countryCode":${o.countryCode.asJson},
         |  "phoneNumber":${o.phoneNumber.asJson}
         |}
         |""".stripMargin
end Phone

case class Contact(name: String, addresses: List[Address], phones: List[Phone])

object Contact:
  given contactSerializer: JsonSerializer[Contact] with
    override def serialize(o: Contact): String =
      import ToJsonMethods.toJson as asJson
      s"""
        |{
        |  "name":${o.name.asJson},
        |  "addresses":${o.addresses.asJson},
        |  "phones":${o.phones.asJson}
        |}
        |""".stripMargin
end Contact

case class AddressBook(contacts: List[Contact])

object AddressBook:
  given addressBookSerializer: JsonSerializer[AddressBook] with
    override def serialize(o: AddressBook): String =
      import ToJsonMethods.toJson as asJson
      s"""
         |{
         |  "contracts":${o.contacts.asJson}
         |}
         |""".stripMargin
end AddressBook
