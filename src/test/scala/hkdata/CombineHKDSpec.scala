package hkdata

import cats._
import cats.implicits._
import org.scalatest._

class CombineHKDSpec extends FlatSpec with Matchers {
  import CombineHKD._
  import monoid.Mean

  case class Account[F[_]](deposit: F[Int], withdrawl: F[Int])

  val account1 = Account[Option](Some(10), None)
  val account2 = Account[Option](Some(20), None)

  val accountId1 = Account[Id](10, 400)
  val accountId2 = Account[Id](20, 500)

  "validate" should "succeed when both fields are nonEmpty" in {
    account1.combineHKD[List](account2) shouldEqual Account(
      List(10, 20),
      List.empty[Int]
    )
  }

  it should "succeed when both fi" in {
    account1.combineHKD[Option](account2) shouldEqual Account[Option](
      Some(30),
      None
    )
  }

  it should "succeed n both fi" in {

    // type OptionId[A] = Option[Id[A]]

    val result: Account[Option] = Account[Option](
      Some(30),
      Some(900)
    )

    accountId1
      .combineHKD[Option](accountId2) shouldEqual result
  }

  it should "succeed n bo fi" in {

    type MeanSum[A] = (Mean[A], Id[A])

    val result: Account[MeanSum] = Account[MeanSum](
      (Mean(30, 2), 30),
      (Mean(900, 2), 900)
    )

    val test: List[Account[Id]] = List(accountId1, accountId2)
  
    

    accountId1
      .combineHKD[MeanSum](accountId2) shouldEqual test.concatHKD[MeanSum]

  }
}
