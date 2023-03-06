package fpinscala.exercises.mytest

/**
  * @date 2023-03-06 11:04
  * @author chenzhr
  * @Description
  */
object TypeclassDemo {
  trait NonZero[A] {
    def nonZero(a: A): Boolean
  }

  object NonZero {
    def create[A](f: A => Boolean): NonZero[A] = new NonZero[A] {
      override def nonZero(a: A): Boolean = f(a)
    }

    implicit val IntNZInstance: NonZero[Int] = create {
      case 0 => false
      case _ => true
    }
  }

  class NonZeroOps[A](a: A)(implicit ev: NonZero[A]) {
    def isNonZero: Boolean = ev.nonZero(a)
  }

  object ToNonZeroOps {
    implicit def toNonZeroOps[A](a: A)(implicit ev: NonZero[A]): NonZeroOps[A] = new NonZeroOps[A](a)
  }

  def main(args: Array[String]): Unit = {
    import ToNonZeroOps.*
    implicit val stringNZInstance: NonZero[String] = NonZero.create {
      case "" => false
      case _ => true
    }
    implicit val booleanNZInstance: NonZero[Boolean] = NonZero.create(b => b)
    implicit def listNZInstance[A]: NonZero[List[A]] = NonZero.create {
      case Nil => false
      case _ => true
    }

    println(
      s"""
        |${10.isNonZero}
        |${"".isNonZero}
        |${"kaka!".isNonZero}
        |${List().isNonZero}
        |${List(1, 2, 3).isNonZero}
        |${true.isNonZero}
        |""".stripMargin)

  }

}
