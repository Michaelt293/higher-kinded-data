package hkdata.monoids

import cats.implicits._, cats.Monoid

case class MinOption[A](getMinOption: Option[A]) extends AnyVal

object MinOption {
  def minOptionNumeric[A](implicit numeric: Numeric[A]) =
    new Monoid[MinOption[A]] {
      val empty = MinOption(None)
      def combine(m1: MinOption[A], m2: MinOption[A]) =
        MinOption(
          (m1.getMinOption, m2.getMinOption)
            .mapN((x: A, y: A) => numeric.min(x, y))
        )
    }
}
