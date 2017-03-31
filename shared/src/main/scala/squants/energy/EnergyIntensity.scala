package squants.energy

import squants._
import squants.radio.WattsPerSquareMeter
import squants.space.{SquareFeet, AreaConversions, SquareMeters}

/**
 *
 */

//TODO: This would be great as a macro

final class EnergyIntensity private (val value: Double, val unit: EnergyIntensityUnit)
    extends Quantity[EnergyIntensity] {

  def dimension = EnergyIntensity

  def *(that: Area): Energy = WattHours(toWattHoursPerSquareMeter * that.toSquareMeters)

  def toWattHoursPerSquareMeter = to(WattHoursPerSquareMeter)
  def toKilowattHoursPerSquareMeter = to(KilowattHoursPerSquareMeter)
  def toMegawattHoursPerSquareMeter = to(MegawattHoursPerSquareMeter)

  def toWattHoursPerSquareFeet = to(WattHoursPerSquareFeet)
  def toKilowattHoursPerSquareFeet = to(KilowattHoursPerSquareFeet)
  def toMegawattHoursPerSquareFeet = to(MegawattHoursPerSquareFeet)

  def toJoulesPerSquareMeter = to(JoulesPerSquareMeter)
  def toKilojoulesPerSquareMeter = to(KilojoulesPerSquareMeter)
  def toMegajoulesPerSquareMeter = to(MegajoulesPerSquareMeter)

  def toJoulesPerSquareFeet = to(JoulesPerSquareFeet)
  def toKilojoulesPerSquareFeet = to(KilojoulesPerSquareFeet)
  def toMegajoulesPerSquareFeet = to(MegajoulesPerSquareFeet)

  def toBtusPerSquareMeter = to(BtusPerSquareMeter)
  def toKilobtusPerSquareMeter = to(KilobtusPerSquareMeter)
  def toMegabtusPerSquareMeter = to(MegabtusPerSquareMeter)

  def toBtusPerSquareFeet = to(BtusPerSquareFeet)
  def toKilobtusPerSquareFeet = to(KilobtusPerSquareFeet)
  def toMegabtusPerSquareFeet = to(MegabtusPerSquareFeet)

  def toThermsPerSquareMeter = to(ThermsPerSquareMeter)
  def toKilothermsPerSquareMeter = to(KilothermsPerSquareMeter)
  def toMegathermsPerSquareMeter = to(MegathermsPerSquareMeter)

  def toThermsPerSquareFeet = to(ThermsPerSquareFeet)
  def toKilothermsPerSquareFeet = to(KilothermsPerSquareFeet)
  def toMegathermsPerSquareFeet = to(MegathermsPerSquareFeet)
}

object EnergyIntensity extends Dimension[EnergyIntensity] {
  private[energy] def apply[A](n: A, unit: EnergyIntensityUnit)(implicit num: Numeric[A]) = new EnergyIntensity(num.toDouble(n), unit)
  def apply = parse _
  def name = "EnergyIntensity"
  def primaryUnit = WattHoursPerSquareMeter
  def siUnit = JoulesPerSquareMeter
  def units = Set(
    WattHoursPerSquareMeter, KilowattHoursPerSquareMeter, MegawattHoursPerSquareMeter,
    WattHoursPerSquareFeet, KilowattHoursPerSquareFeet, MegawattHoursPerSquareFeet,

    JoulesPerSquareMeter, KilojoulesPerSquareMeter, MegajoulesPerSquareMeter,
    JoulesPerSquareFeet, KilojoulesPerSquareFeet, MegajoulesPerSquareFeet,

    BtusPerSquareMeter, KilobtusPerSquareMeter, MegabtusPerSquareMeter,
    BtusPerSquareFeet, KilobtusPerSquareFeet, MegabtusPerSquareFeet,

    ThermsPerSquareMeter, KilothermsPerSquareMeter, MegathermsPerSquareMeter,
    ThermsPerSquareFeet, KilothermsPerSquareFeet, MegathermsPerSquareFeet

  )
}

trait EnergyIntensityUnit extends UnitOfMeasure[EnergyIntensity] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = EnergyIntensity(n, this)
}

// Watts Feet

object WattHoursPerSquareFeet extends EnergyIntensityUnit {
  val conversionFactor = SquareFeet.conversionFactor
  val symbol = WattHours.symbol + "/" + SquareFeet.symbol
}

object KilowattHoursPerSquareFeet extends EnergyIntensityUnit {
  val conversionFactor = MetricSystem.Kilo * SquareFeet.conversionFactor
  val symbol = KilowattHours.symbol + "/" + SquareFeet.symbol
}

object MegawattHoursPerSquareFeet extends EnergyIntensityUnit {
  val conversionFactor = MetricSystem.Mega * SquareFeet.conversionFactor
  val symbol = MegawattHours.symbol + "/" + SquareFeet.symbol
}

// Watts Meter

object WattHoursPerSquareMeter extends EnergyIntensityUnit with PrimaryUnit {
  val symbol = WattHours.symbol + "/" + SquareMeters.symbol
}

object KilowattHoursPerSquareMeter extends EnergyIntensityUnit {
  val conversionFactor = MetricSystem.Kilo
  val symbol = KilowattHours.symbol + "/" + SquareMeters.symbol
}

object MegawattHoursPerSquareMeter extends EnergyIntensityUnit {
  val conversionFactor = MetricSystem.Mega
  val symbol = MegawattHours.symbol + "/" + SquareMeters.symbol
}

// Joules Feet

object JoulesPerSquareFeet extends EnergyIntensityUnit with SiUnit {
  val symbol = Joules.symbol + "/" + SquareMeters.symbol
  val conversionFactor = Joules.conversionFactor * SquareFeet.conversionFactor
}

object KilojoulesPerSquareFeet extends EnergyIntensityUnit with SiUnit {
  val symbol = Kilojoules.symbol + "/" + SquareMeters.symbol
  val conversionFactor = Joules.conversionFactor * MetricSystem.Kilo * SquareFeet.conversionFactor
}

object MegajoulesPerSquareFeet extends EnergyIntensityUnit with SiUnit {
  val symbol = Megajoules.symbol + "/" + SquareMeters.symbol
  val conversionFactor = Joules.conversionFactor * MetricSystem.Mega * SquareFeet.conversionFactor
}

// Joules Feet

object JoulesPerSquareMeter extends EnergyIntensityUnit with SiUnit {
  val symbol = Joules.symbol + "/" + SquareMeters.symbol
  val conversionFactor = Joules.conversionFactor
}

object KilojoulesPerSquareMeter extends EnergyIntensityUnit with SiUnit {
  val symbol = Kilojoules.symbol + "/" + SquareMeters.symbol
  val conversionFactor = Joules.conversionFactor * MetricSystem.Kilo
}

object MegajoulesPerSquareMeter extends EnergyIntensityUnit with SiUnit {
  val symbol = Megajoules.symbol + "/" + SquareMeters.symbol
  val conversionFactor = Joules.conversionFactor * MetricSystem.Mega
}

// Btus Feet

object BtusPerSquareFeet extends EnergyIntensityUnit {
  val symbol = BritishThermalUnits.symbol + "/" + SquareFeet.symbol
  val conversionFactor = EnergyConversions.btuMultiplier * SquareFeet.conversionFactor
}

object KilobtusPerSquareFeet extends EnergyIntensityUnit {
  val symbol = KBtus.symbol + "/" + SquareFeet.symbol
  val conversionFactor = MetricSystem.Kilo * EnergyConversions.btuMultiplier * SquareFeet.conversionFactor
}

object MegabtusPerSquareFeet extends EnergyIntensityUnit {
  val symbol = MBtus.symbol + "/" + SquareFeet.symbol
  val conversionFactor = MetricSystem.Mega * EnergyConversions.btuMultiplier * SquareFeet.conversionFactor
}

// Btus Meter

object BtusPerSquareMeter extends EnergyIntensityUnit {
  val symbol = BritishThermalUnits.symbol + "/" + SquareMeters.symbol
  val conversionFactor = EnergyConversions.btuMultiplier
}

object KilobtusPerSquareMeter extends EnergyIntensityUnit {
  val symbol = KBtus.symbol + "/" + SquareMeters.symbol
  val conversionFactor = MetricSystem.Kilo * EnergyConversions.btuMultiplier
}

object MegabtusPerSquareMeter extends EnergyIntensityUnit {
  val symbol = MBtus.symbol + "/" + SquareMeters.symbol
  val conversionFactor = MetricSystem.Mega * EnergyConversions.btuMultiplier
}

// Therms Feet

object ThermsPerSquareFeet extends EnergyIntensityUnit {
  val symbol = BritishThermalUnits.symbol + "/" + SquareFeet.symbol
  val conversionFactor = EnergyConversions.thermMultiplier * SquareFeet.conversionFactor
}

object KilothermsPerSquareFeet extends EnergyIntensityUnit {
  val symbol = KBtus.symbol + "/" + SquareFeet.symbol
  val conversionFactor = MetricSystem.Kilo * EnergyConversions.thermMultiplier * SquareFeet.conversionFactor
}

object MegathermsPerSquareFeet extends EnergyIntensityUnit {
  val symbol = MBtus.symbol + "/" + SquareFeet.symbol
  val conversionFactor = MetricSystem.Mega * EnergyConversions.thermMultiplier * SquareFeet.conversionFactor
}

// Therms Meter
object ThermsPerSquareMeter extends EnergyIntensityUnit {
  val symbol = BritishThermalUnits.symbol + "/" + SquareFeet.symbol
  val conversionFactor = EnergyConversions.thermMultiplier
}

object KilothermsPerSquareMeter extends EnergyIntensityUnit {
  val symbol = KBtus.symbol + "/" + SquareFeet.symbol
  val conversionFactor = MetricSystem.Kilo * EnergyConversions.thermMultiplier
}

object MegathermsPerSquareMeter extends EnergyIntensityUnit {
  val symbol = MBtus.symbol + "/" + SquareFeet.symbol
  val conversionFactor = MetricSystem.Mega * EnergyConversions.thermMultiplier
}

object EnergyIntensityConversions {
  lazy val wattHourPerSquareMeter = WattHoursPerSquareMeter(1)
  lazy val kilowattHourPerSquareMeter = KilowattHoursPerSquareMeter(1)
  lazy val megawattHourPerSquareMeter = MegawattHoursPerSquareMeter(1)

  lazy val wattHourPerSquareFeet = WattHoursPerSquareFeet(1)
  lazy val kilowattHourPerSquareFeet = KilowattHoursPerSquareFeet(1)
  lazy val megawattHourPerSquareMeet = MegawattHoursPerSquareFeet(1)

  lazy val joulePerSquareMeter = JoulesPerSquareMeter(1)
  lazy val kilojoulePerSquareMeter = KilojoulesPerSquareMeter(1)
  lazy val megajoulePerSquarecMeter = MegajoulesPerSquareMeter(1)

  lazy val joulePerSquareFeet = JoulesPerSquareFeet(1)
  lazy val kilojoulePerSquareFeet = KilojoulesPerSquareFeet(1)
  lazy val megajoulePerSquareFeet = MegajoulesPerSquareFeet(1)

  lazy val btuPerSquareMeter = BtusPerSquareMeter(1)
  lazy val kilobtuPerSquareMeter = KilobtusPerSquareMeter(1)
  lazy val megabtuPerSquareMeter = MegabtusPerSquareMeter(1)

  lazy val btuPerSquareFeet = BtusPerSquareFeet(1)
  lazy val kilobtuPerSquareFeet = KilobtusPerSquareFeet(1)
  lazy val megabtuPerSquareFeet = MegabtusPerSquareFeet(1)

  lazy val thermPerSquareMeter = ThermsPerSquareMeter(1)
  lazy val kilothermPerSquareMeter = KilothermsPerSquareMeter(1)
  lazy val megathermPerSquareMeter = MegathermsPerSquareMeter(1)

  lazy val thermPerSquareFeet = ThermsPerSquareFeet(1)
  lazy val kilothermPerSquareFeet = KilothermsPerSquareFeet(1)
  lazy val megathermPerSquareFeet = MegathermsPerSquareFeet(1)

  implicit class EnergyIntensityConversions[A](n: A)(implicit num: Numeric[A]) {
    def wattsHoursPerSquareMeter = WattHoursPerSquareMeter(n)
    def kilowattsHoursPerSquareMeter = KilowattHoursPerSquareMeter(n)
    def megawattsHoursPerSquareMeter = MegawattHoursPerSquareMeter(n)

    def wattsHoursPerSquareFeet = WattHoursPerSquareFeet(n)
    def kilowattsHoursPerSquareFeet = KilowattHoursPerSquareFeet(n)
    def megawattsHoursPerSquareFeet = MegawattHoursPerSquareFeet(n)

    def joulesPerSquareMeter = JoulesPerSquareMeter(n)
    def kilojoulesPerSquareMeter = KilojoulesPerSquareMeter(n)
    def megajoulesPerSquareMeter = MegajoulesPerSquareMeter(n)

    def joulesPerSquareFeet = JoulesPerSquareFeet(n)
    def kilojoulesPerSquareFeet = KilojoulesPerSquareFeet(n)
    def megajoulesPerSquareFeet = MegajoulesPerSquareFeet(n)

    def btusPerSquareMeter = BtusPerSquareMeter(n)
    def kilobtusPerSquareMeter = KilobtusPerSquareMeter(n)
    def megabtusPerSquareMeter = MegabtusPerSquareMeter(n)

    def btusPerSquareFeet = BtusPerSquareFeet(n)
    def kilobtusPerSquareFeet = KilobtusPerSquareFeet(n)
    def megabtusPerSquareFeet = MegabtusPerSquareFeet(n)

    def thermsPerSquareFeet = ThermsPerSquareFeet(n)
    def kilothermsPerSquareFeet = KilothermsPerSquareFeet(n)
    def megathermsPerSquareFeet = MegathermsPerSquareFeet(n)

    def thermsPerSquareMeter = ThermsPerSquareMeter(n)
    def kilothermsPerSquareMeter = KilothermsPerSquareMeter(n)
    def megathermsPerSquareMeter = MegathermsPerSquareMeter(n)

  }

  implicit object EnergyIntensityNumeric extends AbstractQuantityNumeric[EnergyIntensity](EnergyIntensity.primaryUnit)
}
