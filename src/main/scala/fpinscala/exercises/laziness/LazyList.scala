package fpinscala.exercises.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toListRecursive: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive

  def toList: List[A] =
    @annotation.tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] = ll match
      case Empty => acc.reverse
      case Cons(h, t) => go(t(), h() :: acc)

    go(this, Nil)



  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => Empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty

  def takeWhile_1(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean = this match
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)

  def forAll_1(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, acc) => if p(a) then cons(a, acc) else acc)

  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)){
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this){
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // zipWith的特例
  def zip[B](that: LazyList[B]): LazyList[(A, B)] =
    zipWith(that)((_, _))

  // 持续遍历, 直到lazyList中没有更多的元素
  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)){
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    }

  def zipAllWith[B, C](that: LazyList[B])(f: (Option[A], Option[B]) => C): LazyList[C] =
    unfold((this, that)){
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()), Option.empty[B]), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some(f(Option.empty[A], Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
    }

  def zipAllViaZipAllWith[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = zipAllWith(that)((_, _))

  // s.startsWith(s2), 逐一对比s和s2中的每一个元素
  // 直到s2耗竭, 若s的每一个元素都和s2相等, 则返回true;
  // 若s在s2为空之前就已经耗竭, 返回false
  // zipAll组合s和s2成一个tuple2, takeWhile检查s有没有提前耗竭, forAll检查s和s2中的元素是否相等, 若任意一方耗竭, 都会提前终止
  def startsWith[B](s: LazyList[B]): Boolean =
    zipAll(s).takeWhile(_(1).isDefined).forAll((a, b) => a == b)

  // 返回lazylist的所有后缀, 包括他本身
  def tails: LazyList[LazyList[A]] =
    unfold(this){
      case Cons(h, t) => Some((Cons(h, t), t()))
      case Empty => None
    }.append(LazyList(empty))

  def hasSubsequence[A](l: LazyList[A]): Boolean =
    tails.exists(ll => ll.startsWith(l))




object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  // 无限流, 一个引用自身的LazyList
  val ones: LazyList[Int] = LazyList.cons(1, ones)

  // 对ones进行泛化, 根据给定的值返回一个无限流
  def continually[A](a: A): LazyList[A] =
    lazy val single: LazyList[A] = LazyList.cons(a, single)
    single

  // 写一个函数生成一个整数无限流,从n开始,然后n+1、n+2,等等.
  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(current, next))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some(a, s) => cons(a, unfold(s)(f))
      case None => empty

  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(z).fold(empty)((p: (A, S)) => cons(p._1, unfold(p._2)(f)))

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(z).map((p: (A, S)) => cons(p._1, unfold(p._2)(f))).getOrElse(empty)

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)){
      case (current, next) => Some((current, (next, current + next)))
    }

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(n => Some(n, n + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(())(_ => Some(a, ()))

  lazy val onesViaUnfold: LazyList[Int] = unfold(())(_ => Some(1, ()))
