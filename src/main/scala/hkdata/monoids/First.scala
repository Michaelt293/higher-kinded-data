package hkdata.monoid

import cats.Monoid

case class First[A](getFirst: A) extends AnyVal

object First {
  def firstMonoid[A] =
    new Monoid[First[Option[A]]] {
      val empty = First(None)
      def combine(m1: First[Option[A]], m2: First[Option[A]]) =
        First(m1.getFirst.orElse(m2.getFirst))
    }
}
