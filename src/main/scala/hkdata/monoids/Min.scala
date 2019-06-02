package hkdata.monoids

import cats.Monoid

case class Min[A](getMin: A) extends AnyVal

object Min {
  def instance[A](emp: A, comb: (A, A) => A) =
    new Monoid[Min[A]] {
      val empty = Min(emp)
      def combine(m1: Min[A], m2: Min[A]) =
        Min(
          comb(m1.getMin, m2.getMin)
        )
    }

  implicit def intMinMonoid =
    instance(Int.MaxValue, (x: Int, y: Int) => if (x > y) y else x)

  implicit def longMinMonoid =
    instance(Long.MaxValue, (x: Long, y: Long) => if (x > y) y else x)

  implicit def floatMinMonoid =
    instance(Float.MaxValue, (x: Float, y: Float) => if (x > y) y else x)

  implicit def doubleMinMonoid =
    instance(Double.MaxValue, (x: Double, y: Double) => if (x > y) y else x)
}
