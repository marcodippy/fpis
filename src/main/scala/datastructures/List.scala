package datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(dbs: List[Double]): Double = dbs match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //[3.2] Implement the function tail for removing the first element of a List (constant time)
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  //[3.3] Implement the function setHead for replacing the first element of a List with a different value
  def setHead[A](l: List[A], e: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(e, xs)
  }

  //[3.4] Generalize tail to the function drop, which removes the first n elements from a list.
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case _ => l
  }

  //[3.5] Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }

  //[3.6] Implement a function, init, that returns a List consisting of all but the last element of a List.
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](l: List[A])(z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs)(z)(f))
  }

  def sum2(l: List[Int]): Int = foldRight(l)(0)(_ + _)

  def product2(l: List[Double]): Double = foldRight(l)(1.0)(_ * _)

  //[3.9] compute the lenvgth of a list using foldRight
  def length[A](l: List[A]): Int = foldRight(l)(0)((_, acc) => 1 + acc)

  //[3.10] write the foldLeft function (tailRecursive)
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  @tailrec
  def foldLeftCurried[A, B](l: List[A])(z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeftCurried(xs)(f(z, x))(f)
  }

  //[3.11] write sum, product and length using foldLeft
  def sumFL(l: List[Int]): Int = foldLeftCurried(l)(0)(_ + _)

  def productFL(l: List[Double]): Double = foldLeftCurried(l)(1.0)(_ * _)

  def lengthFL[A](l: List[A]): Int = foldLeftCurried(l)(0)((acc, _) => acc + 1)

  //[3.12] Write a function that returns the reverse of a list
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((a, b) => Cons(b, a))

  //[3.13] write foldLeft in terms of foldRight and foldRight via foldLeft
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l)((b: B) => b)((a, g) => b => g(f(b, a)))(z)

  //[3.14] write append in terms of either foldLeft or foldRight
  def appendFR[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1)(l2)(Cons(_, _))

  //[3.15] Write a function that concatenates a list of lists into a single list.
  //Its runtime should be linear in the total length of all lists
  def concat[A](l: List[List[A]]): List[A] = foldRight(l)(List[A]())(append(_, _))

  //[3.16] Write a function that transforms a list of integers by adding 1 to each element
  def addOne(l: List[Int]): List[Int] = foldRight(l)(List[Int]())((a, b) => Cons(a + 1, b))

  //[3.17] Write a function that turns each value in a List[Double] into a String
  def dblToStr(l: List[Double]): List[String] = foldRight(l)(List[String]())((a, b) => Cons(a.toString, b))

  //[3.18] Write a function map that generalizes modifying each element in a list while maintaining the structure of the list
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l)(List[B]())((a, b) => Cons(f(a), b))

  //[3.19] Write a function filter that removes elements from a list unless they satisfy a given predicate.
  //Use it to remove all odd numbers from a List[Int].
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l)(List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

  //[3.20] Write a function flatMap that works like map except that the function given will return
  //a list instead of a single result, and that list should be inserted into the final resulting list.
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  //[3.21] Use flatMap to implement filter
  def filterFM[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) Cons(a, Nil) else Nil)

  //[3.22] Write a function that accepts two lists and constructs a new list by adding corresponding elements.
  //For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
  def sumLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, sumLists(xs1, xs2))
  }

  //[3.23] Generalize the function you just wrote so that it’s not specific to integers or addition.
  //Name your generalized function zipWith.
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
  }

  //[3.24] implement hasSubsequence for checking whether a List contains another List as a subsequence
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, Cons(_, _)) => false
    case (Cons(_, _), Nil) => true
    case (Nil, Nil) => true
    case (Cons(x1, xs1), Cons(x2, xs2)) => hasSubsequence(xs1, if (x1 == x2) xs2 else sub)
  }

  //alternative implementation
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequenceWithStartsWith[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequenceWithStartsWith(t, sub)
  }
}