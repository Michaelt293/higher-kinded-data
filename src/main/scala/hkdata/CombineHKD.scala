package hkdata

import cats._
import cats.implicits._
import shapeless.{::, HList, HNil, Generic}

trait CombineHKD[A] {
  type Out

  def emptyHDK: Out
  def toCombiningF(a: A): Out
  def combineHKD(a1: Out, a2: Out): Out
}

object CombineHKD {
  import monoids._

  type Aux[A, B] = CombineHKD[A] { type Out = B }

  def instance[A, B](emp: B, toF: A => B, comb: (B, B) => B): Aux[A, B] =
    new CombineHKD[A] {
      type Out = B

      def emptyHDK: B = emp
      def toCombiningF(a: A): B = toF(a)
      def combineHKD(a1: B, a2: B): B = comb(a1, a2)
    }

  implicit val hnilCombineHKD: Aux[HNil, HNil] =
    instance(HNil, identity, (_, _) => HNil)

  implicit def hconsCombineHKD[A, F[_], G[_], T <: HList, T1 <: HList](
      implicit
      monoid: Monoid[G[A]],
      tAux: Aux[T, T1],
      hAux: CombineFields.Aux[F[A], G[A]]
  ): Aux[F[A] :: T, G[A] :: T1] =
    instance({ monoid.empty :: tAux.emptyHDK }, {
      case h :: t =>
        hAux.toCombiningF(h) :: tAux.toCombiningF(t)
    }, {
      case (h1 :: t1, h2 :: t2) =>
        (h1 |+| h2) :: tAux.combineHKD(t1, t2)
    })

  implicit def genericCombineHKD[A, ARepr <: HList, B, BRepr <: HList](
      implicit
      genA: Generic.Aux[A, ARepr],
      genB: Generic.Aux[B, BRepr],
      aux: Aux[ARepr, BRepr]
  ): Aux[A, B] = {
    instance(
      genB.from(aux.emptyHDK),
      a1 => genB.from(aux.toCombiningF(genA.to(a1))),
      (x, y) => genB.from(aux.combineHKD(genB.to(x), genB.to(y)))
    )
  }

  implicit class FunctorCombineHKDOps[F[_], HKD[_[_]], G[_]](
      data: F[HKD[G]]
  ) {
    def concatHKD[H[_]](
        implicit
        foldable: Foldable[F],
        aux: Aux[HKD[G], HKD[H]]
    ): HKD[H] =
      foldable.foldLeft(data, aux.emptyHDK) { (b, a) =>
        aux.combineHKD(b, aux.toCombiningF(a))
      }
  }
}
