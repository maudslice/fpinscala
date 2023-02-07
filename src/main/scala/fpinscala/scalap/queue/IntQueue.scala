package fpinscala.scalap.queue

/**
  * @date 2023-02-01 9:16
  * @author chenzhr
  * @Description
  */
abstract class IntQueue:
  def get(): Int
  def put(x: Int): Unit

class BasicIntQueue extends IntQueue:
  import scala.collection.mutable.ArrayBuffer
  private val buf = ArrayBuffer.empty[Int]

  override def get(): Int = buf.remove(0)

  def put(x :Int): Unit = buf += x

end BasicIntQueue

trait Doubling extends IntQueue:
  // 对于需求实现叠加修改的特质, 需要将方法声明为abstract override
  // 这样的修饰组合只能用于特质成员, 不能用于类成员
  // 它的涵义是该特质必须混入某个拥有该方法具体定义的类中
  abstract override def put(x: Int): Unit = super.put(2 * x)

trait Incrementing extends IntQueue:
  abstract override def put(x: Int): Unit = super.put(x + 1)

trait Filtering extends IntQueue:
  abstract override def put(x: Int): Unit =
    if x >= 0 then super.put(x)
