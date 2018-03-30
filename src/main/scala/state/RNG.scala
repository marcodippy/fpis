package state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, SimpleRNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  //[6.1] Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
  //Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart.
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i) -> r
  }

  //[6.2] Write a function to generate a Double between 0 and 1, not including 1.
  //Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double.
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1)) -> r
  }

  //[6.3] Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    (i, d) -> r2
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (id, r) = intDouble(rng)
    (id.swap, r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    (d1, d2, d3) -> r4
  }

  //[6.4] Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(c: Int, l: List[Int], r: RNG): (List[Int], RNG) = {
      if (c == 0) l -> r
      else {
        val (i, r2) = r.nextInt
        go(c - 1, i :: l, r2)
      }
    }

    go(count, List.empty[Int], rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      f(a) -> rng2
    }

  //[6.5] Use map to reimplement double in a more elegant way. See exercise 6.2.
  def doubleWithMap: Rand[Double] = {
    val r: Rand[Int] = rng => nonNegativeInt(rng)
    map(r)(i => i / (Int.MaxValue.toDouble + 1))
  }

  //[6.6] Write the implementation of map2 based on the following signature.
  //This function takes two actions, ra and rb, and a function f for combining their results, and returns a new action that combines them:
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r2) = ra(rng)
      val (b, r3) = rb(r2)
      f(a, b) -> r3
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  //[6.7] Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
  //Implement sequence for combining a List of transitions into a single transition.
  //Use it to reimplement the ints function you wrote before.
  //For the latter, you can use the standard library function List.fill(n)(x) to make a list with repeated n times.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsWithSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  //[6.8] Implement flatMap, and then use it to implement nonNegativeLessThan.
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r2) = f(rng)
      g(a)(r2)
    }

  def nonNegativeLessThanWithFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThanWithFlatMap(n)
    })

  //[6.9] Reimplement map and map2 in terms of flatMap.
  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a =>
      map(rb)(b => f(a, b))
    )
}