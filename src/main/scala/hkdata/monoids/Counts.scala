package hkdata.monoids

import cats.Monoid
import cats.implicits._

case class Counts[A](getCounts: Map[A, Int]) extends AnyVal

object Counts {
  implicit def countMonoid[A] =
    new Monoid[Counts[A]] {
      def empty = Counts(Map.empty[A, Int])
      def combine(p1: Counts[A], p2: Counts[A]) =
        Counts(p1.getCounts |+| p2.getCounts)
    }
}
