package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new IllegalArgumentException("Can't do tail of Nil")
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Cons(h, Nil)
      case Cons(_, tail) => Cons(h, tail)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0)
      l
    else {
      l match {
        case Nil => Nil
        case Cons(_, tail) => drop(l, n-1)
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case other => other
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
      case Nil => throw new IllegalArgumentException("Can't do init of Nil")
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, count) => count+1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  def sum3(ns: List[Int]): Int = List.foldLeft(ns, 0)(_+_)

  def product3(ns: List[Double]): Double = List.foldLeft(ns, 1.0)(_*_)

  def length2(ns: List[_]): Int = List.foldLeft(ns, 0)((count, _) => count + 1)

  def reverse[A](ns: List[A]): List[A] = List.foldLeft[A, List[A]](ns, Nil)((acc, h) => Cons(h, acc))

  // TODO - foldLeft in terms of foldRight and vv.

  def append2[A](left: List[A], right: List[A]): List[A] = List.foldRight(left, right)((item, tail) => Cons(item, tail))

  def concatenate[A](lists: List[List[A]]): List[A] = {
    List.foldRight[List[A], List[A]](lists, Nil)((list, tail) => append2(list, tail))
  }

  def addOne(list: List[Int]): List[Int] = List.foldRight[Int, List[Int]](list, Nil)((head, tail) => Cons(head + 1, tail))

  def doubleToString(list: List[Double]): List[String] =
    List.foldRight[Double, List[String]](list, Nil)((head, tail) => Cons(head.toString, tail))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    List.foldRight[A, List[B]](as, Nil)((head, tail) => Cons(f(head), tail))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    List.foldRight[A, List[A]](as, Nil)((head, tail) => if (f(head)) Cons(head, tail) else tail )

  def removeOdds(ints: List[Int]) = filter(ints)(_ % 2 == 0)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = List.foldRight[A, List[B]](as, Nil)((item, acc) => append2(f(item), acc))

  def filter2[A](as: List[A])(f: A => Boolean) = flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  def sumLists(as: List[Int], bs: List[Int]): List[Int] = {
    (as, bs) match {
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Cons(aHead + bHead, sumLists(aTail, bTail))
      case _ => Nil
    }
  }

  def zipWith[A, B](as: List[A], bs: List[B]): List[(A, B)] = {
    (as, bs) match {
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Cons((aHead, bHead), zipWith(aTail, bTail))
      case _ => Nil
    }
  }

  /* True if sup starts with the items in sub */
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => aHead == bHead && startsWith(aTail, bTail)
      case _ => false
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    List.startsWith(sup, sub) || (sup match {
      case Cons(_, tail) => hasSubsequence(tail, sub)
      case Nil => false
    })
  }
}
