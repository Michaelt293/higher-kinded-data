package hkdata.monoid

import cats.implicits._, cats._, cats.derived._

case class Max[A](getMax: A) extends AnyVal

object Max {
  def instance[A](emp: A, comb: (A, A) => A) =
    new Monoid[Max[A]] {
      val empty = Max(emp)
      def combine(m1: Max[A], m2: Max[A]) =
        Max(
          comb(m1.getMax, m2.getMax)
        )
    }

  implicit def intMaxMonoid =
    instance(Int.MinValue, (x: Int, y: Int) => if (x < y) y else x)

  implicit def longMaxMonoid =
    instance(Long.MinValue, (x: Long, y: Long) => if (x < y) y else x)

  implicit def floatMaxMonoid =
    instance(Float.MinValue, (x: Float, y: Float) => if (x < y) y else x)

  implicit val maxFunctor: Functor[Max] = {
    import derived.auto.functor._
    derived.semi.functor
  }

  implicit val maxApplicative: Applicative[Max] =
    new Applicative[Max] {
      def ap[A, B](pf: Max[A => B])(pa: Max[A]): Max[B] =
        (pf, pa) match {
          case (Max(f), Max(b)) => Max(f(b))
        }

      def pure[A](a: A): Max[A] = Max(a)
    }
}
