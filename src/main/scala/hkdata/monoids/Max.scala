package hkdata.monoids

import cats.Monoid

case class Max[A](getMax: A) extends AnyVal

object Max {
  private def instance[A](emp: A, comb: (A, A) => A) =
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

  implicit def doubleMaxMonoid =
    instance(Double.MinValue, (x: Double, y: Double) => if (x < y) y else x)
}
