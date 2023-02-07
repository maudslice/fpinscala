package fpinscala.scalap

/**
  * @date 2023-01-31 15:07
  * @author chenzhr
  * @Description
  */

import Element.elem

object Spiral:
  val space: Element = elem(" ")
  val corner: Element = elem("+")

  def spiral(nEdges: Int, direction: Int): Element =
    if nEdges == 1 then
      elem("+")
    else
      val sp = spiral(nEdges - 1, (direction + 3) % 4)
      def verticalBar = elem('|', 1, sp.height)
      def horizontalBar = elem('-', sp.width, 1)

      if direction == 0 then
        (corner beside horizontalBar) above (sp beside space)
      else if direction == 1 then
        (sp above space) beside (corner above verticalBar)
      else if direction == 2 then
        (space beside sp) above (horizontalBar beside corner)
      else
        (verticalBar above corner) beside (space above sp)

  def main(args: Array[String]): Unit =
    val nSides = args(0).toInt
    println(spiral(nSides, 0))

end Spiral

