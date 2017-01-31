package squants.energy

import squants._
import squants.radio.WattsPerSquareMeter
import squants.space.{SquareFeet, AreaConversions, SquareMeters}

/**
  * Created by clayteeter on 1/31/17.
  */

final class EnergyIntensity private (val value: Double, val unit: EnergyIntensityUnit)
  extends Quantity[EnergyIntensity] {

  def dimension = EnergyIntensity

  def *(that: Area): Energy = WattHours(toWattsPerSquareMeter * that.toSquareMeters)

  def toWattsPerSquareMeter = to(WattHoursPerSquareMeter)
  def toJoulePerSquareMeter = to(JoulesPerSquareMeter)
  def toKBtuPerSquareFoot = to(KBtuPerSquareFoot)
}

object EnergyIntensity extends Dimension[EnergyIntensity] {
  private[energy] def apply[A](n: A, unit: EnergyIntensityUnit)(implicit num: Numeric[A]) = new EnergyIntensity(num.toDouble(n), unit)
  def apply = parse _
  def name = "EnergyIntensity"
  def primaryUnit = WattHoursPerSquareMeter
  def siUnit = JoulesPerSquareMeter
  def units = Set(WattHoursPerSquareMeter, JoulesPerSquareMeter, KBtuPerSquareFoot)
}

trait EnergyIntensityUnit extends UnitOfMeasure[EnergyIntensity] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = EnergyIntensity(n, this)
}

object WattHoursPerSquareMeter extends EnergyIntensityUnit with PrimaryUnit {
  val symbol = WattHours.symbol + "/" + SquareMeters.symbol
}

object JoulesPerSquareMeter extends EnergyIntensityUnit with SiUnit {
  val symbol = Joules.symbol + "/" + SquareMeters.symbol
  val conversionFactor = Joules.conversionFactor * 3600.0
}

object KBtuPerSquareFoot extends EnergyIntensityUnit {
  val symbol = KBtus.symbol + "/" + SquareFeet.symbol
  val conversionFactor = MetricSystem.Kilo * EnergyConversions.btuMultiplier * AreaConversions.squareFoot
}

object EnergyIntensityConversions {
  lazy val wattHoursPerSquareMeter = wattHoursPerSquareMeter(1)
  lazy val joulePerCubicMeter = joulePerCubicMeter(1)
  lazy val kBtuPerSquareFoot = kBtuPerSquareFoot(1)

  implicit class EnergyIntensityConversions[A](n: A)(implicit num: Numeric[A]) {
    def wattHoursPerSquareMeter = WattHoursPerSquareMeter(n)
    def joulePerCubicMeter = JoulesPerCubicMeter(n)
    def kBtuPerSquareFoot = KBtuPerSquareFoot(n)
  }

  implicit object EnergyIntensityNumeric extends AbstractQuantityNumeric[EnergyIntensity](EnergyIntensity.primaryUnit)
}
