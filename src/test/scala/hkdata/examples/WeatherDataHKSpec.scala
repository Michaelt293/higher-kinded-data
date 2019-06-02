package hkdata.examples

import cats._
import cats.implicits._
import org.scalatest._

class WeatherDataHKSpec extends FlatSpec with Matchers {
  import hkdata.CombineHKD._
  import hkdata.monoids._

  "completeWeatherData" should "succeed with Set" in {
    val expected: WeatherDataHK[Set] =
      WeatherDataHK(
        Set(27.7, 26.7, 27.2, 21.4, 23.1),
        Set(19.8, 21.1, 19.9, 21.4, 22.0),
        Set(4, 11, 8, 14)
      )

    val result: WeatherDataHK[Set] =
      WeatherDataHK.completeWeatherData.concatHKD[Set]

    result shouldEqual expected
  }

  it should "succeed with List" in {
    val expected: WeatherDataHK[List] =
      WeatherDataHK(
        List(21.4, 23.1, 26.7, 27.2, 27.7),
        List(19.9, 19.8, 21.4, 22.0, 21.1),
        List(4, 11, 8, 8, 14)
      )

    val result: WeatherDataHK[List] =
      WeatherDataHK.completeWeatherData.concatHKD[List]

    result shouldEqual expected
  }

  it should "succeed with Mean" in {
    val expected: WeatherDataHK[Mean] =
      WeatherDataHK(
        Mean(126.10000000000001, 5),
        Mean(104.19999999999999, 5),
        Mean(45, 5)
      )

    val result: WeatherDataHK[Mean] =
      WeatherDataHK.completeWeatherData.concatHKD[Mean]

    result shouldEqual expected
  }

  it should "succeed with Max" in {
    val expected: WeatherDataHK[Max] =
      WeatherDataHK(Max(27.7), Max(22.0), Max(14))

    val result: WeatherDataHK[Max] =
      WeatherDataHK.completeWeatherData.concatHKD[Max]

    result shouldEqual expected
  }

  it should "succeed with Min" in {
    val expected: WeatherDataHK[Min] =
      WeatherDataHK(Min(21.4), Min(19.8), Min(4))

    val result: WeatherDataHK[Min] =
      WeatherDataHK.completeWeatherData.concatHKD[Min]

    result shouldEqual expected
  }

  it should "succeed with Product" in {
    val expected: WeatherDataHK[Product] =
      WeatherDataHK(
        Product(9944562.640319997),
        Product(3914147.3976),
        Product(39424)
      )

    val result: WeatherDataHK[Product] =
      WeatherDataHK.completeWeatherData.concatHKD[Product]

    result shouldEqual expected
  }

  it should "succeed with Sum" in {
    val expected: WeatherDataHK[Sum] =
      WeatherDataHK[Sum](
        Sum(126.10000000000001),
        Sum(104.19999999999999),
        Sum(45)
      )

    val result: WeatherDataHK[Sum] =
      WeatherDataHK.completeWeatherData.concatHKD[Sum]

    result shouldEqual expected
  }

  it should "succeed with Counts" in {
    val expected: WeatherDataHK[Counts] =
      WeatherDataHK(
        Counts(Map(27.7 -> 1, 26.7 -> 1, 27.2 -> 1, 21.4 -> 1, 23.1 -> 1)),
        Counts(Map(19.8 -> 1, 21.1 -> 1, 19.9 -> 1, 21.4 -> 1, 22.0 -> 1)),
        Counts(Map(11 -> 1, 4 -> 1, 8 -> 2, 14 -> 1))
      )

    val result: WeatherDataHK[Counts] =
      WeatherDataHK.completeWeatherData.concatHKD[Counts]

    result shouldEqual expected
  }

  it should "succeed with MaxOption" in {
    val expected: WeatherDataHK[MaxOption] =
      WeatherDataHK(
        MaxOption(Some(27.7)),
        MaxOption(Some(22.0)),
        MaxOption(Some(14))
      )

    val result: WeatherDataHK[MaxOption] =
      WeatherDataHK.completeWeatherData.concatHKD[MaxOption]

    result shouldEqual expected
  }

  it should "succeed with MinOption" in {
    val expected: WeatherDataHK[MinOption] =
      WeatherDataHK(
        MinOption(Some(21.4)),
        MinOption(Some(19.8)),
        MinOption(Some(4))
      )

    val result: WeatherDataHK[MinOption] =
      WeatherDataHK.completeWeatherData.concatHKD[MinOption]

    result shouldEqual expected
  }

  it should "succeed with Id" in {
    val expected: WeatherDataHK[Id] =
      WeatherDataHK[Id](126.10000000000001, 104.19999999999999, 45)

    val result: WeatherDataHK[Id] =
      WeatherDataHK.completeWeatherData.concatHKD[Id]

    result shouldEqual expected
  }

  it should "succeed with (Mean[A],Max[A])" in {
    type MeanMax[A] = (Mean[A], Max[A])

    val expected: WeatherDataHK[MeanMax] =
      WeatherDataHK[MeanMax](
        (Mean(126.10000000000001, 5), Max(27.7)),
        (Mean(104.19999999999999, 5), Max(22.0)),
        (Mean(45, 5), Max(14))
      )

    val result: WeatherDataHK[MeanMax] =
      WeatherDataHK.completeWeatherData.concatHKD[MeanMax]

    result shouldEqual expected
  }

  it should "succeed with (Mean[A],Max[A], Min[A])" in {
    type MeanMaxMin[A] = (Mean[A], Max[A], Min[A])

    val expected: WeatherDataHK[MeanMaxMin] =
      WeatherDataHK[MeanMaxMin](
        (Mean(126.10000000000001, 5), Max(27.7), Min(21.4)),
        (Mean(104.19999999999999, 5), Max(22.0), Min(19.8)),
        (Mean(45, 5), Max(14), Min(4))
      )

    val result: WeatherDataHK[MeanMaxMin] =
      WeatherDataHK.completeWeatherData.concatHKD[MeanMaxMin]

    result shouldEqual expected
  }

  it should "succeed with (Mean[A],Max[A], Min[A], Id[A])" in {
    type MeanMaxMinId[A] = (Mean[A], Max[A], Min[A], Id[A])

    val expected: WeatherDataHK[MeanMaxMinId] =
      WeatherDataHK[MeanMaxMinId](
        (Mean(126.10000000000001, 5), Max(27.7), Min(21.4), 126.10000000000001),
        (Mean(104.19999999999999, 5), Max(22.0), Min(19.8), 104.19999999999999),
        (Mean(45, 5), Max(14), Min(4), 45)
      )

    val result: WeatherDataHK[MeanMaxMinId] =
      WeatherDataHK.completeWeatherData.concatHKD[MeanMaxMinId]

    result shouldEqual expected
  }

  "partialWeatherData" should "succeed with Set" in {
    val expected: WeatherDataHK[Set] =
      WeatherDataHK(
        Set(28.6, 22.2, 21.8, 27.1, 26.5),
        Set(19.9, 19.8, 21.4),
        Set(66, 91, 154)
      )

    val result: WeatherDataHK[Set] =
      WeatherDataHK.partialWeatherData.concatHKD[Set]

    result shouldEqual expected
  }

  it should "succeed with List" in {
    val expected: WeatherDataHK[List] =
      WeatherDataHK(
        List(28.6, 27.1, 26.5, 22.2, 21.8),
        List(19.9, 19.8, 21.4),
        List(66, 91, 154)
      )

    val result: WeatherDataHK[List] =
      WeatherDataHK.partialWeatherData.concatHKD[List]

    result shouldEqual expected
  }

  it should "succeed with Mean" in {
    val expected: WeatherDataHK[Mean] =
      WeatherDataHK(Mean(126.2, 5), Mean(61.1, 5), Mean(311, 5))

    val result: WeatherDataHK[Mean] =
      WeatherDataHK.partialWeatherData.concatHKD[Mean]

    result shouldEqual expected
  }

  it should "succeed with MeanOption" in {
    val expected: WeatherDataHK[MeanOption] =
      WeatherDataHK(
        MeanOption(Some(126.2), 5),
        MeanOption(Some(61.1), 3),
        MeanOption(Some(311), 3)
      )

    val result: WeatherDataHK[MeanOption] =
      WeatherDataHK.partialWeatherData.concatHKD[MeanOption]

    result shouldEqual expected
  }

  it should "succeed with Product" in {
    val expected: WeatherDataHK[Product] =
      WeatherDataHK(
        Product(9940097.9964),
        Product(8432.027999999998),
        Product(924924)
      )

    val result: WeatherDataHK[Product] =
      WeatherDataHK.partialWeatherData.concatHKD[Product]

    result shouldEqual expected
  }

  it should "succeed with Sum" in {
    val expected: WeatherDataHK[Sum] =
      WeatherDataHK(Sum(126.2), Sum(61.1), Sum(311))

    val result: WeatherDataHK[Sum] =
      WeatherDataHK.partialWeatherData.concatHKD[Sum]

    result shouldEqual expected
  }

  it should "succeed with Counts" in {
    val expected: WeatherDataHK[Counts] =
      WeatherDataHK(
        Counts(Map(28.6 -> 1, 22.2 -> 1, 21.8 -> 1, 27.1 -> 1, 26.5 -> 1)),
        Counts(Map(19.8 -> 1, 19.9 -> 1, 21.4 -> 1)),
        Counts(Map(91 -> 1, 66 -> 1, 154 -> 1))
      )

    val result: WeatherDataHK[Counts] =
      WeatherDataHK.partialWeatherData.concatHKD[Counts]

    result shouldEqual expected
  }

  it should "succeed with Counts[Option[A]]" in {
    type CountsOpt[A] = Counts[Option[A]]

    val expected: WeatherDataHK[CountsOpt] =
      WeatherDataHK[CountsOpt](
        Counts(
          Map(
            Some(28.6) -> 1,
            Some(26.5) -> 1,
            Some(27.1) -> 1,
            Some(22.2) -> 1,
            Some(21.8) -> 1
          )
        ),
        Counts(
          Map(Some(19.8) -> 1, Some(19.9) -> 1, Some(21.4) -> 1, None -> 2)
        ),
        Counts(Map(Some(66) -> 1, None -> 2, Some(91) -> 1, Some(154) -> 1))
      )

    val result: WeatherDataHK[CountsOpt] =
      WeatherDataHK.partialWeatherData.concatHKD[CountsOpt]

    result shouldEqual expected
  }

  it should "succeed with MaxOption" in {
    val expected: WeatherDataHK[MaxOption] =
      WeatherDataHK(
        MaxOption(Some(28.6)),
        MaxOption(Some(21.4)),
        MaxOption(Some(154))
      )

    val result: WeatherDataHK[MaxOption] =
      WeatherDataHK.partialWeatherData.concatHKD[MaxOption]

    result shouldEqual expected
  }

  it should "succeed with MinOption" in {
    val expected: WeatherDataHK[MinOption] =
      WeatherDataHK(
        MinOption(Some(21.8)),
        MinOption(Some(19.8)),
        MinOption(Some(66))
      )

    val result: WeatherDataHK[MinOption] =
      WeatherDataHK.partialWeatherData.concatHKD[MinOption]

    result shouldEqual expected
  }

  it should "succeed with First" in {
    type FirstOpt[A] = First[Option[A]]

    val expected: WeatherDataHK[FirstOpt] =
      WeatherDataHK[FirstOpt](
        First(Some(28.6)),
        First(Some(19.9)),
        First(Some(66))
      )

    val result: WeatherDataHK[FirstOpt] =
      WeatherDataHK.partialWeatherData.concatHKD[FirstOpt]

    result shouldEqual expected
  }

  it should "succeed with Last" in {
    type LastOpt[A] = Last[Option[A]]

    val expected: WeatherDataHK[LastOpt] =
      WeatherDataHK[LastOpt](
        Last(Some(21.8)),
        Last(Some(21.4)),
        Last(Some(154))
      )

    val result: WeatherDataHK[LastOpt] =
      WeatherDataHK.partialWeatherData.concatHKD[LastOpt]

    result shouldEqual expected
  }

  it should "succeed with Option" in {
    val expected: WeatherDataHK[Option] =
      WeatherDataHK[Option](Some(126.2), Some(61.1), Some(311))

    val result: WeatherDataHK[Option] =
      WeatherDataHK.partialWeatherData.concatHKD[Option]

    result shouldEqual expected
  }
}
