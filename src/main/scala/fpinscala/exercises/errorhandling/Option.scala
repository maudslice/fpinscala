package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter

import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None => None
    case Some(a) => Some(f(a))

  def getOrElse[B >: A](default: => B): B = this match
    case None => default
    case Some(a) => a

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def flatMap_1[B](f: A => Option[B]): Option[B] = this match
    case None => None
    case Some(a) => f(a)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case None => ob
    case _ => this

  def orElse_1[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match
    case Some(a) if f(a) => this
    case _ => None

  def filter_1(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  // 计算方差
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  // 编译器会自动把 aa <- a这样的绑定操作转换为一系列的map和flatmap操作
  // 可以在任何使用map和flatmap的地方使用for推导表达式
  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))

  def sequence_1[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)((aa, acc) => aa :: acc))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((a, acc) => map2(f(a), acc)((aa, acc) => aa :: acc))

  def traverse2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse2(t)(f))((h, t) => h :: t)

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(x => x)