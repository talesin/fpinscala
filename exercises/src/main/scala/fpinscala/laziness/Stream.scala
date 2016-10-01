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

  def take2(n: Int): Stream[A] =
    Stream.unfold((this,n)){
      case (Empty,_) => None
      case (_,i) if i <= 0 => None
      case (Cons(h, t), i) => Some(h(), (t(),i-1))
    }


  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, as) => if (p(a)) Cons(() => a, () => as) else Empty)

  def takeWhile3(p: A => Boolean): Stream[A] =
    Stream.unfold(this){
      case Empty => None
      case Cons(h, t) => if (p(h())) Some(h(), t()) else None
    }

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

  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this){
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, as) => if (p(a)) Stream.cons(a, as) else as)

  def append[B >: A](b: => B): Stream[B] =
    foldRight[Stream[B]](Stream.cons[B](b, Empty))(Stream.cons(_, _))

  def join[B >: A](bs: Stream[B]): Stream[B] =
    foldRight[Stream[B]](bs)(Stream.cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)(f(_).join(_))

  def startsWith[B](bs: Stream[B]): Boolean =
    zipAll(bs).takeWhile(_._2.isDefined).forAll{ case(a,b) => a == b }

  def tail: Stream[A] = drop(1)

  def tails: Stream[Stream[A]] =
    Stream.unfold((this,true)){
      case (_, false) => None
      case (Empty, true) => Some(Empty, (Empty, false))
      case (xs, _) => Some(xs, (xs.tail, true))
    }

  def scanRight[B >: A](z: B)(f: (A,B) => B): Stream[B] =
    this.foldRight(Stream(z))((a, bs) => bs match {
      case Cons(h, _) => Stream.cons(f(a, h()), bs)
    })

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  def zipWith[B,C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this,bs)){
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(ah, at), Cons(bh, bt)) => Some(f(ah(), bh()), (at(), bt()))
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this,bs)){
      case (Empty, Empty) => None
      case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
      case (Cons(ah, at), Empty) => Some((Some(ah()), None), (at(), Empty))
      case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (Empty, bt()))
    }

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


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }

  val ones: Stream[Int] = constant(1)

  def constant[A](a: A): Stream[A] = unfold(a)(x => Some(x,x))

  def from(n: Int): Stream[Int] = unfold(n)(x => Some(x, x+1))

  def fibs(a: Int, b: Int): Stream[Int] = unfold((a,b)){ case (x,y) => Some(x, (y, x+y))}

}