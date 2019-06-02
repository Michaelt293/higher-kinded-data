package hkdata

import cats._
import cats.implicits._
import shapeless.{::, HList, HNil, Generic}

trait CombineHKD[A] {
  type Out

  def emptyHDK: Out
  def toCombiningF(a: A): Out
  def combineHKD(out1: Out, out2: Out): Out
}

object CombineHKD {
  type Aux[A, B] = CombineHKD[A] { type Out = B }

  def instance[A, B](emp: B, toF: A => B, comb: (B, B) => B): Aux[A, B] =
    new CombineHKD[A] {
      type Out = B

      def emptyHDK: B = emp
      def toCombiningF(a: A): B = toF(a)
      def combineHKD(b1: B, b2: B): B = comb(b1, b2)
    }

  implicit val hnilCombineHKD: Aux[HNil, HNil] =
    instance(HNil, identity, (_, _) => HNil)

  implicit def hconsCombineHKD[A, F[_], G[_], T <: HList, T1 <: HList](
      implicit
      field: CombineFields.Aux[F[A], G[A]],
      hkd: CombineHKD.Aux[T, T1],
      monoid: Monoid[G[A]]
  ): Aux[F[A] :: T, G[A] :: T1] =
    instance(
      monoid.empty :: hkd.emptyHDK, {
        case h :: t =>
          field.toCombiningF(h) :: hkd.toCombiningF(t)
      }, {
        case (h1 :: t1, h2 :: t2) =>
          (h1 |+| h2) :: hkd.combineHKD(t1, t2)
      }
    )

  implicit def genericCombineHKD[A, ARepr <: HList, B, BRepr <: HList](
      implicit
      genA: Generic.Aux[A, ARepr],
      genB: Generic.Aux[B, BRepr],
      hkd: CombineHKD.Aux[ARepr, BRepr]
  ): Aux[A, B] = {
    instance(
      genB.from(hkd.emptyHDK),
      a => genB.from(hkd.toCombiningF(genA.to(a))),
      (b1, b2) => genB.from(hkd.combineHKD(genB.to(b1), genB.to(b2)))
    )
  }

  implicit class FoldableCombineHKDOps[F[_], HKD[_[_]], G[_]](
      data: F[HKD[G]]
  ) {
    def concatHKD[H[_]](
        implicit
        foldable: Foldable[F],
        hkd: CombineHKD.Aux[HKD[G], HKD[H]]
    ): HKD[H] =
      foldable.foldLeft(data, hkd.emptyHDK) { (h, g) =>
        hkd.combineHKD(h, hkd.toCombiningF(g))
      }
  }
}
