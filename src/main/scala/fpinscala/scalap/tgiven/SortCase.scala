package fpinscala.scalap.tgiven

/**
  * @date 2023-02-07 9:45
  * @author chenzhr
  * @Description
  */
object SortCase:
  trait Ord[T]:
    def compare(x: T, y: T): Int
    def lteq(x: T, y: T): Boolean = compare(x, y) < 1

  object Ord:
    // 用with关键字可以代替new Ord[Int]
    given intOrd: Ord[Int] with
      override def compare(x: Int, y: Int): Int =
        if x == y then 0 else if x > y then 1 else -1

    given strOrd: Ord[String] with
      override def compare(x: String, y: String): Int = x.compareTo(y)

    // val age = 42
    // 声明val的时候, 是提供名称(age), 编译器来推断类型(Int)
    // 对上下文参数而言, 这个过程是相反的, 给出类型, 然后编译器基于可用的上下文参数, 来合成一个用于表示该类型的名称, 是通过类型来查找上下文参数的, 所以可以使用匿名的方式来声明:
    given Ord[Double] with
      override def compare(x: Double, y: Double): Int =
        if x == y then 0 else if x > y then 1 else -1

  end Ord

  def isort[T](xs: List[T])(using ord: Ord[T]): List[T] =
    if xs.isEmpty then Nil
    else insert(xs.head, isort(xs.tail))

  def insert[T](x: T, xs: List[T])(using ord: Ord[T]): List[T] =
    if xs.isEmpty || ord.lteq(x, xs.head) then x :: xs
    else xs.head :: insert(x, xs.tail)

  def main(args: Array[String]): Unit =
    // 通过使用上下文参数, 可以简化函数调用, 不再需要显式的提供用given声明的参数
    // 如果当前作用域存在满足类型的上下文参数, 编译器就会将这个值传递给该函数
    println(isort(List("mango", "jackfruit", "durian"))) // 在Ord的伴生对象中已经用given声明了String的比较方法
    // 如果编译器在规定的作用域内没有找到满足条件的上下文参数, 就会立即在上下文对象中寻找
    // Ord[String], 会在Ord和String的伴生对象中寻找, String不能编辑, 所以将given放在Ord的伴生对象中是比较合适的
    println(isort(List(5, 2, 6, 1, 28,9)))

end SortCase

