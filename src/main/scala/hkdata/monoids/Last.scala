package hkdata.monoids

import cats.Monoid

case class Last[A](getLast: A) extends AnyVal

object Last {
  implicit def lastMonoid[A] =
    new Monoid[Last[Option[A]]] {
      val empty = Last(None)
      def combine(l1: Last[Option[A]], l2: Last[Option[A]]) =
        Last(l2.getLast.orElse(l1.getLast))
    }
}
