package squants.emissions

import squants._
import squants.space.{SquareMeters, SquareFeet}

/**
 * Created by clayteeter on 2/1/17.
 */
final class EmissionsIntensity private (val value: Double, val unit: EmissionsIntensityUnit)
    extends Quantity[EmissionsIntensity] {

  def dimension = EmissionsIntensity

}
object EmissionsIntensity extends Dimension[EmissionsIntensity] {
  private[emissions] def apply[A](n: A, unit: EmissionsIntensityUnit)(implicit num: Numeric[A]) = new EmissionsIntensity(num.toDouble(n), unit)
  def apply = parse _
  def name = "EnergyIntensity"
  def primaryUnit = KilogramCO2ePerSquareMeter
  def siUnit = KilogramCO2ePerSquareMeter
  def units = Set(KilogramCO2ePerSquareMeter, KilogramCO2ePerSquareFeet)

}

trait EmissionsIntensityUnit extends UnitOfMeasure[EmissionsIntensity] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = EmissionsIntensity(n, this)
}

object KilogramCO2ePerSquareFeet extends EmissionsIntensityUnit {
  val conversionFactor = SquareFeet.conversionFactor
  val symbol = Kilograms.symbol + "Co2/" + SquareFeet.symbol
}

object KilogramCO2ePerSquareMeter extends EmissionsIntensityUnit with PrimaryUnit with SiUnit {
  val symbol = Kilograms.symbol + "Co2/" + SquareMeters.symbol
}

object EmissionsIntensityConversions {
  lazy val toKilogramCO2ePerSquareMeter = KilogramCO2ePerSquareMeter(1)
  lazy val toKilogramCO2ePerSquareFeet = KilogramCO2ePerSquareFeet(1)

  implicit class EmissionsConversions[A](n: A)(implicit num: Numeric[A]) {

    def toKilogramsCO2ePerSquareMeter = KilogramCO2ePerSquareMeter(n)
    def toKilogramsCO2ePerSquareFeet = KilogramCO2ePerSquareFeet(n)
  }
}
