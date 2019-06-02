package hkdata.monoids

import cats.Monoid

case class MinOption[A](getMinOption: Option[A]) extends AnyVal

object MinOption {
  implicit def minOptionMonoid[A](implicit ordering: Ordering[A]) =
    new Monoid[MinOption[A]] {
      val empty = MinOption(None)
      def combine(m1: MinOption[A], m2: MinOption[A]) =
        MinOption((m1.getMinOption, m2.getMinOption) match {
          case (Some(x), Some(y)) => Some(ordering.min(x, y))
          case (x, y)             => x.orElse(y)
        })
    }
}
