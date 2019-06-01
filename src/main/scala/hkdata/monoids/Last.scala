package hkdata.monoids

import cats.Monoid

case class Last[A](getLast: A) extends AnyVal

object Last {
  def lastMonoid[A] =
    new Monoid[Last[Option[A]]] {
      val empty = Last(None)
      def combine(m1: Last[Option[A]], m2: Last[Option[A]]) =
        Last(m2.getLast.orElse(m1.getLast))
    }
}
