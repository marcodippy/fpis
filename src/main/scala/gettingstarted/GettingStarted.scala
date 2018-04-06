package gettingstarted

import scala.annotation.tailrec

object GettingStarted {
  //[2.1] Write a recursive function to get the nth Fibonacci number
  def fibonacci(n: Int): Int = {

    @tailrec
    def go(n: Int, prev: Int, cur: Int): Int = n match {
      case 1 => prev
      case _ => go(n - 1, cur, cur + prev)
    }

    go(n, 0, 1)
  }

  //[2.2] Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1)
        true
      else if (ordered(as(n), as(n + 1)))
        go(n + 1)
      else
        false

    go(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  //[2.3] Curry: converts a function f of two arguments into a function of one argument that partially applies f.
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  //[2.4] Uncurry: reverses the transformation of curry
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  //[2.5] Function composition: Implement the higher-order function that composes two functions
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}