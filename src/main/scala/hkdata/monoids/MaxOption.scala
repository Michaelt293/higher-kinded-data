package hkdata.monoids

import cats.Monoid

case class MaxOption[A](getMaxOption: Option[A]) extends AnyVal

object MaxOption {
  implicit def maxOptionMonoid[A](implicit ordering: Ordering[A]) =
    new Monoid[MaxOption[A]] {
      val empty = MaxOption(None)
      def combine(m1: MaxOption[A], m2: MaxOption[A]) =
        MaxOption((m1.getMaxOption, m2.getMaxOption) match {
          case (Some(x), Some(y)) => Some(ordering.max(x, y))
          case (x, y)             => x.orElse(y)
        })
    }
}
