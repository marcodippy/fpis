package parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object NonBlocking {

  sealed trait Future[+A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown() }
      latch.await()
      ref.get
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit = {
      val _ = es.submit(new Callable[Unit] {
        def call = r
      })
    }

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A, B]](es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = {
          cond(es) {
            result => {
              if (result)
                eval(es) {
                  t(es)(cb)
                }
              else
                eval(es) {
                  f(es)(cb)
                }
            }
          }
        }
      }

    //[7.11] Implement choiceN and then choice in terms of choiceN.
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = {
          n(es) {
            i =>
              eval(es) {
                choices(i)(es)(cb)
              }
          }
        }
      }

    def choiceWithChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit = {
          pa(es)(a => eval(es)(cb(f(a))))
        }
      }

    //[7.12] implement choiceMap
    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => new Future[V] {
        def apply(cb: V => Unit): Unit =
          key(es) { k => choices(k)(es)(cb) }
      }

    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          pa(es)(a => f(a)(es)(cb))
      }

    //[7.14] implement join
    def join[A](a: Par[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          a(es)(pa => pa(es)(cb))
      }

    def joinWithFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(identity)

    def flatMapWithJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      join(map(pa)(f))

    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
      def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
    }
  }

}
