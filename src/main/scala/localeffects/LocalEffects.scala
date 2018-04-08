package localeffects

sealed trait ST[S, A] {
  self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] =
    new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] =
    new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}


sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] =
    new ST[S, Unit] {
      def run(s: S): (Unit, S) = {
        cell = a
        ((), s)
      }
    }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(new STRef[S, A] {
      var cell: A = a
    })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A] {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  //[14.1] Add a combinator on STArray to fill the array from a Map where each key in the map represents an index into the array,
  // and the value under that key is written to the array at that index.
  // For example, xs.fill(Map(0->"a", 2->"b")) should write the value "a" at index 0 in the array xs and "b" at index 2.
  // Use the existing combinators to write your implementation.
  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) {
      case ((i, a), acc) => acc.flatMap(_ => write(i, a))
    }

  def swap(i: Int, j: Int): ST[S, Unit] =
    for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()

}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value: Array[A] = Array.fill(sz)(v)
    })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value: Array[A] = xs.toArray
    })
}

object Quicksort {
  def noop[S]: ST[S, Unit] = ST[S, Unit](())

  //[14.2] Write the purely functional versions of partition and qs.
  def partition[S](arr: STArray[S, Int], n: Int, r: Int, pivot: Int): ST[S, Int] =
    for {
      vp <- arr.read(pivot)
      _ <- arr.swap(pivot, r)
      j <- STRef(n)
      _ <- (n until r).foldLeft(noop[S])((s, i) =>
        for {
          _ <- s
          vi <- arr.read(i)
          _ <- if (vi < vp) for {
            vj <- j.read
            _ <- arr.swap(i, vj)
            _ <- j.write(vj + 1)
          } yield () else noop[S]
        } yield ())
      x <- j.read
      _ <- arr.swap(x, r)
    } yield x

  def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = {
    if (n < r)
      for {
        pi <- partition(a, n, r, n + (n - r) / 2)
        _ <- qs(a, n, pi - 1)
        _ <- qs(a, pi + 1, r)
      } yield ()
    else noop[S]
  }


  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S]: ST[S, List[Int]] = for {
          arr <- STArray.fromList(xs)
          size <- arr.size
          _ <- qs(arr, 0, size - 1)
          sorted <- arr.freeze
        } yield sorted
      })
}


//[14.3] Give the same treatment to scala.collection.mutable.HashMap as we’ve given here to references and arrays.
// Come up with a minimal set of primitive combinators for creating and manipulating hash maps.

import scala.collection.mutable

sealed abstract class STMap[S, K, V] {
  protected def value: mutable.HashMap[K, V]

  def size: ST[S, Int] = ST(value.size)

  def +=(kv: (K, V)): ST[S, Unit] = ST({
    value += kv
    ()
  })

  def -=(k: K): ST[S, Unit] = ST({
    value -= k
    ()
  })

  def apply(k: K): ST[S, V] = ST(value(k))

  def get(k: K): ST[S, Option[V]] = ST(value.get(k))

}

object STMap {
  def empty[S, K, V]: ST[S, STMap[S, K, V]] =
    ST(new STMap[S, K, V] {
      val value = mutable.HashMap.empty[K, V]
    })

  def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] =
    ST(new STMap[S, K, V] {
      val value = (mutable.HashMap.newBuilder[K, V] ++= m).result
    })
}