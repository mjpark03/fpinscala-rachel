


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /*
    3.1
  */
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /*
    3.2
  */
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  /*
    3.3
  */
  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => Cons(head, tail)
  }
  
  /*
    3.4
  */
  def drop[A](list: List[A], n: Int): List[A] = {
    if(n <= 0) list
    else {
      list match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n-1)
      }
    }
  }

  /*
    3.5
  */
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => list
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /*
    3.9
  */
  def length[A](list: List[A]): Int = {
    foldRight(list, 0)((_, acc) => acc + 1)
  }
i
}
