package fpinscala.scalap

/**
  * @date 2023-01-30 15:55
  * @author chenzhr
  * @Description
  */
class Rational(n: Int, d: Int) extends Ordered[Rational] :
  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val number = n / g
  val denom = d / g

  def this(n: Int) = this(n, 1)

  def +(that: Rational): Rational = Rational(
    number * that.denom + that.number * denom,
    denom * that.denom
  )

  def +(i: Int): Rational = Rational(number + i * denom, denom)

  def -(that: Rational): Rational = Rational(
    number * that.denom - that.number * denom,
    denom * that.denom
  )

  def -(i: Int): Rational = Rational(number - i * denom, denom)


  override def toString: String = s"$number/$denom"

  override def compare(that: Rational): Int =
    (this.number * that.denom) - (that.number * this.denom)

  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

object Rational

def apply(n: Int, d: Int): Rational = new Rational(n, d)

extension (x: Int)
  def +(y: Rational) = Rational(x) + y
  def -(y: Rational) = Rational(x) - y

@main def t(): Unit =
  val a = Rational(1, 2)
  val b = Rational(1, 3)
  println(a + b)
  println(a + 1)
  println(1 + b)