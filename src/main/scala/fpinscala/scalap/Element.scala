package fpinscala.scalap

/**
  * @date 2023-01-31 13:20
  * @author chenzhr
  * @Description
  */
import Element.elem
abstract class Element:
  def contents: Vector[String]
  def height: Int = contents.length
  def width: Int = if height == 0 then 0 else contents(0).length

  def widen(w: Int): Element =
    if w <= width then this
    else
      val left = elem(' ', (w - width) / 2, height)
      val right = elem(' ', w - width - left.width, height) 
      left beside this beside right

  def heighten(h: Int): Element =
    if h <= height then this
    else
      val top = elem(' ', width, (h - height) / 2)
      val bot = elem(' ', width, h - height - top.height)
      top above this above bot

  def above(that: Element): Element =
    val this1 = widen(that.width)
    val that1 = that.widen(width)
    elem(this1.contents ++ that1.contents)

  def beside(that: Element): Element =
    val this1 = heighten(that.height)
    val that1 = that.heighten(height)
    elem(
      for (line1, line2) <- this1.contents.zip(that1.contents)
        yield line1 + line2)

  override def toString: String = contents.mkString("\n")

end Element

object Element:
  private class VectorElement(val contents: Vector[String]) extends Element

  private class LineElement(s: String) extends Element :
    val contents: Vector[String] = Vector(s)
    override def width: Int = s.length
    override def height: Int = 1

  private class UniformElement(ch: Char, override val width: Int, override val height: Int) extends Element :
    val line: String = ch.toString * width
    def contents: Vector[String] = Vector.fill(height)(line)

  def elem(contents: Vector[String]): Element =
    VectorElement(contents)

  def elem(s: String): Element =
    LineElement(s)

  def elem(ch: Char, height: Int, width: Int): Element =
    UniformElement(ch, height, width)

end Element

@main def m() = {
  val element = elem("--") above elem("-----")
  println(element)
}
