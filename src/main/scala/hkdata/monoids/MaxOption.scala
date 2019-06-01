package hkdata.monoids

import cats.Monoid, cats.implicits._

case class MaxOption[A](getMaxOption: Option[A]) extends AnyVal

object MaxOption {
  def minOptionNumeric[A](implicit numeric: Numeric[A]) =
    new Monoid[MaxOption[A]] {
      val empty = MaxOption(None)
      def combine(m1: MaxOption[A], m2: MaxOption[A]) =
        MaxOption(
          (m1.getMaxOption, m2.getMaxOption)
            .mapN((x: A, y: A) => numeric.max(x, y))
        )
    }
}
