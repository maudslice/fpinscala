package fpinscala.exercises.parsing

import fpinscala.answers.testing.exhaustive.{Gen, Prop}

import scala.util.matching.Regex


trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def string(s: String): Parser[String]

//  val numA = char('a').many.map(_.size)
  // numA.run("aaa") = Right(3)
  // numA.run("b") = Right(0)

  def succeed[A](a: A): Parser[A]

  def regex(r: Regex): Parser[String]


  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A]

    // 执行选择逻辑, 若p1失败, 才对p2进行求值, 所以p2需要是惰性的
    infix def or(p2: => Parser[A]): Parser[A]

    def |(p2: => Parser[A]): Parser[A] = p or p2

    def map[B](f: A => B): Parser[B] =
      p.flatMap(f andThen succeed)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      p.flatMap(a => p2.map(b => (a, b)))

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      p.product(p2).map((a, b) => f(a, b))

    // 返回p中元素的列表
    def many: Parser[List[A]] =
      // 若map2总是对p2进行求值, many函数将永远不会停止, p2需要是惰性的
      p.map2(p.many)(_ :: _) | succeed(Nil)

    def many1: Parser[List[A]] =
      p.map2(p.many)(_ :: _)

    // 返回部分由p分析成功的字符串输入
    def slice: Parser[String]

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def map2ViaFlatMap[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      p.flatMap(a => p2.map(b => f(a, b)))


  object Laws {
    // 法则1: map应具有结构保持的特性, 一个失败的分析器不能进过map以后变成一个成功的分析器, 反之亦然
    // 也就是说 map(p)(a => a) == p
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p1.run(s) == p2.run(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  def remaining: String = ???

  def slice(n: Int) = ???

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next()
    else ""

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()):
  def push(loc: Location, msg: String): ParseError = ???

  def label(s: String): ParseError = ???

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] = ???

  val nConsecutiveAs: Parser[Int] = ???
