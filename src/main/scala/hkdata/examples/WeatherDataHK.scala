package hkdata.examples

import cats.Id

case class WeatherDataHK[F[_]](
    temp: F[Float],
    dewPoint: F[Float],
    windSpeed: F[Int]
)

object WeatherDataHK {
  type WeatherData = WeatherDataHK[Id]
  type OptionWeatherData = WeatherDataHK[Option]

  val completeWeatherData: List[WeatherData] = List(
    WeatherDataHK[Id](21.4f, 19.9f, 4),
    WeatherDataHK[Id](23.1f, 19.8f, 11),
    WeatherDataHK[Id](26.7f, 21.4f, 8),
    WeatherDataHK[Id](27.2f, 22.0f, 8),
    WeatherDataHK[Id](27.7f, 21.1f, 14)
  )

  val partialWeatherData: List[OptionWeatherData] = List(
    WeatherDataHK[Option](Some(21.4f), Some(19.9f), Some(4)),
    WeatherDataHK[Option](Some(23.1f), Some(19.8f), Some(11)),
    WeatherDataHK[Option](Some(26.7f), Some(21.4f), Some(8)),
    WeatherDataHK[Option](Some(27.2f), Some(22.0f), Some(8)),
    WeatherDataHK[Option](Some(27.7f), Some(21.1f), Some(14))
  )
}
