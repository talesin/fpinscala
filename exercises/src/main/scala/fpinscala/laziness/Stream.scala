package fpinscala.laziness

import scala.annotation.tailrec
import scala.{Stream => _}

trait Stream[+A] {

  def toList: List[A] =
    this.foldRight(List[A]())(_ :: _)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
    case _  => Empty
  }


  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n < n => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, as) => if (p(a)) Cons(() => a, () => as) else Empty)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t)  => p(h()) && t().forAll(p)
    case Empty => true
  }

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case Empty => None
  }

  def headOption2: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((a, bs) => Stream.cons(f(a),  bs))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, as) => if (p(a)) Stream.cons(a, as) else as)

  def append[B >: A](b: => B): Stream[B] =
    foldRight[Stream[B]](Stream.cons[B](b, Empty))((a, bs) => Stream.cons(a, bs))

//  def flatMap[B](f: A => Stream[B]): Stream[B] =
//    foldRight[Stream[B]](Empty)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}