/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package squants.energy

import squants._
import squants.electro.{ Coulombs, ElectricCharge, ElectricPotential, Volts }
import squants.mass.{ ChemicalAmount, Kilograms }
import squants.motion.Newtons
import squants.space.CubicMeters
import squants.thermal.{ JoulesPerKelvin, Kelvin, ThermalCapacity }
import squants.time.{ Time, _ }

/**
 * Represents a quantity of energy
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[squants.energy.WattHours]]
 */
final class Energy private (val value: Double, val unit: EnergyUnit)
    extends Quantity[Energy]
    with TimeIntegral[Power]
    with SecondTimeIntegral[PowerRamp] {

  def dimension = Energy

  protected def timeDerived = Watts(toWattHours)
  protected def time = Hours(1)

  def /(that: Length): Force = Newtons(toJoules / that.toMeters)
  def /(that: Force): Length = Meters(toJoules / that.toNewtons)
  def /(that: Mass): SpecificEnergy = Grays(toJoules / that.toKilograms)
  def /(that: SpecificEnergy): Mass = Kilograms(toJoules / that.toGrays)
  def /(that: Volume): EnergyDensity = JoulesPerCubicMeter(toJoules / that.toCubicMeters)
  def /(that: EnergyDensity): Volume = CubicMeters(toJoules / that.toJoulesPerCubicMeter)
  def /(that: ElectricCharge): ElectricPotential = Volts(this.toJoules / that.toCoulombs)
  def /(that: ElectricPotential): ElectricCharge = Coulombs(this.toJoules / that.toVolts)
  def /(that: Temperature): ThermalCapacity = JoulesPerKelvin(toJoules / that.toKelvinDegrees)
  def /(that: ThermalCapacity) = Kelvin(toJoules / that.toJoulesPerKelvin)

  def /(that: ChemicalAmount) = ??? // return MolarEnergy
  def /(that: Angle) = ??? // return Torque (dimensionally equivalent to energy as Angles are dimensionless)
  def /(that: Area) = ??? // Insolation, Energy Area Density

  def /(that: TimeSquared): PowerRamp = this / that.time1 / that.time2
  def /(that: PowerRamp): TimeSquared = (this / that.timeIntegrated) * time

  def toWattHours = to(WattHours)
  def toKilowattHours = to(KilowattHours)
  def toMegawattHours = to(MegawattHours)
  def toGigawattHours = to(GigawattHours)

  def toJoules = to(Joules)
  def toPicojoules = to(Picojoules)
  def toNanojoules = to(Nanojoules)
  def toMicrojoules = to(Microjoules)
  def toMillijoules = to(Millijoules)
  def toKilojoules = to(Kilojoules)
  def toMegajoules = to(Megajoules)
  def toGigajoules = to(Gigajoules)
  def toTerajoules = to(Terajoules)

  def toBtus = to(BritishThermalUnits)
  def toKBtus = to(KBtus)
  def toMBtus = to(MBtus)
  def toMMBtus = to(MMBtus)
  def toErgs = to(Ergs)
  def toTherms = to(Therms)
  def toNGCfs = to(NGCfs)
  def toNGCCfs = to(NGCCfs)
  def toNGKCfs = to(NGKCfs)
  def toNGMCfs = to(NGMCfs)
  def toNGm3s = to(NGm3s)

  def toOilNo1USGs = to(OilNo1USGs)
  def toOilNo1UKGs = to(OilNo1UKGs)
  def toOilNo1Ls = to(OilNo1Ls)

  def toOilNo2USGs = to(OilNo2USGs)
  def toOilNo2UKGs = to(OilNo2UKGs)
  def toOilNo2Ls = to(OilNo2Ls)

  def toOilNo4USGs = to(OilNo4USGs)
  def toOilNo4UKGs = to(OilNo4UKGs)
  def toOilNo4Ls = to(OilNo4Ls)

  def toOilNo6USGs = to(OilNo6USGs)
  def toOilNo6UKGs = to(OilNo6UKGs)
  def toOilNo6Ls = to(OilNo6Ls)

  def toDieselUSGs = to(DieselUSGs)
  def toDieselUKGs = to(DieselUKGs)
  def toDieselLs = to(DieselLs)

  def toKeroseneUSGs = to(KeroseneUSGs)
  def toKeroseneUKGs = to(KeroseneUKGs)
  def toKeroseneLs = to(KeroseneLs)

  def toPropaneUSGs = to(PropaneUSGs)
  def toPropaneUKGs = to(PropaneUKGs)
  def toPropaneLs = to(PropaneLs)
  def toPropaneCfs = to(PropaneCfs)
  def toPropaneCCfs = to(PropaneCCfs)
  def toPropaneLKCs = to(PropaneKCfs)

  def toSteamLbs = to(SteamLbs)

  def toCHWTonHs = to(CHWTonHs)

  def toCoalATons = to(CoalATons)
  def toCoalALbs = to(CoalALbs)
  def toCoalATonnes = to(CoalATonnes)

  def toCoalBitTons = to(CoalBitTons)
  def toCoalBitLbs = to(CoalBitLbs)
  def toCoalBitTonnes = to(CoalBitTonnes)

  def toCokeTons = to(CokeTons)
  def toCokeLbs = to(CokeLbs)
  def toCokeTonnes = to(CokeTonnes)

  def toWoodTones = to(WoodTons)
  def toWoodTonnes = to(WoodTonnes)
}

/**
 * Companion object for [[squants.energy.Energy]]
 */
object Energy extends Dimension[Energy] {
  private[energy] def apply[A](n: A, unit: EnergyUnit)(implicit num: Numeric[A]) = new Energy(num.toDouble(n), unit)
  def apply(load: Power, time: Time): Energy = load * time
  def apply = parse _

  def name = "Energy"
  def primaryUnit = WattHours
  def siUnit = Joules
  def units = Set(WattHours, KilowattHours, MegawattHours, GigawattHours,
    Joules, Picojoules, Nanojoules, Microjoules, Millijoules,
    Kilojoules, Megajoules, Gigajoules, Terajoules,
    BritishThermalUnits, KBtus, MBtus, MMBtus, Ergs, Therms, NGCfs,
    NGCCfs, NGKCfs, NGMCfs, NGm3s, OilNo1USGs, OilNo1UKGs, OilNo1Ls,
    OilNo2USGs, OilNo2UKGs, OilNo2Ls, OilNo4USGs, OilNo4UKGs, OilNo4Ls,
    OilNo6USGs, OilNo6UKGs, OilNo6Ls, DieselUSGs, DieselUKGs, DieselLs,
    PropaneUSGs, PropaneUKGs, PropaneLs,PropaneCfs, PropaneCCfs, PropaneKCfs,
    KeroseneUSGs, KeroseneUKGs, KeroseneLs, SteamLbs, SteamKLbs, SteamMLbs,
    CHWTonHs, CoalATons,CoalATonnes, CoalALbs, CoalAKLbs, CoalAMLbs,
    CoalBitTons, CoalBitTonnes, CoalBitLbs, CoalBitKLbs, CoalBitMLbs,
    CokeTons, CokeLbs, CokeKLbs, CokeMLbs, CokeTonnes, WoodTons, WoodTonnes)
}

/**
 * Base trait for units of [[squants.energy.Energy]]
 */
trait EnergyUnit extends UnitOfMeasure[Energy] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = Energy(n, this)
}

object WattHours extends EnergyUnit with PrimaryUnit {
  val symbol = "Wh"
}

object KilowattHours extends EnergyUnit {
  val conversionFactor = Watts.conversionFactor * MetricSystem.Kilo
  val symbol = "KWh"
}

object MegawattHours extends EnergyUnit {
  val conversionFactor = Watts.conversionFactor * MetricSystem.Mega
  val symbol = "MWh"
}

object GigawattHours extends EnergyUnit {
  val conversionFactor = Watts.conversionFactor * MetricSystem.Giga
  val symbol = "GWh"
}

object Joules extends EnergyUnit with SiUnit {
  val conversionFactor = 1.0 / Time.SecondsPerHour
  val symbol = "J"
}

object Picojoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Pico
  val symbol = "pJ"
}

object Nanojoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Nano
  val symbol = "nJ"
}

object Microjoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Micro
  val symbol = "µJ"
}

object Millijoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Milli
  val symbol = "mJ"
}

object Kilojoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Kilo
  val symbol = "kJ"
}

object Megajoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Mega
  val symbol = "MJ"
}

object Gigajoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Giga
  val symbol = "GJ"
}

object Terajoules extends EnergyUnit {
  val conversionFactor = Joules.conversionFactor * MetricSystem.Tera
  val symbol = "TJ"
}

object BritishThermalUnits extends EnergyUnit {
  val conversionFactor = EnergyConversions.btuMultiplier
  val symbol = "Btu"
}

object KBtus extends EnergyUnit {
  val conversionFactor = EnergyConversions.btuMultiplier * MetricSystem.Kilo
  val symbol = "KBtu"
}

//MBTU is equivalent to MMBTU as both refer to 1 Million BTUs. Confusing, yes, but MM means thousand thousand
object MBtus extends EnergyUnit {
  val conversionFactor = EnergyConversions.btuMultiplier * MetricSystem.Mega
  val symbol = "MBtu"
}

object MMBtus extends EnergyUnit {
  val conversionFactor = EnergyConversions.btuMultiplier * MetricSystem.Mega
  val symbol = "MMBtu"
}

object Ergs extends EnergyUnit {
  val conversionFactor = 100.0 * Nanojoules.conversionFactor
  val symbol = "erg"
}

object Therms extends EnergyUnit {
  val conversionFactor = EnergyConversions.thermMultiplier
  val symbol = "therms"
}

object NGCfs extends EnergyUnit {
  val conversionFactor = EnergyConversions.NGCfMultiplier
  val symbol = "NGcf"
}

object NGCCfs extends EnergyUnit {
  val conversionFactor = EnergyConversions.NGCfMultiplier * MetricSystem.Centi
  val symbol = "NGCcf"
}

object NGKCfs extends EnergyUnit {
  val conversionFactor = EnergyConversions.NGCfMultiplier * MetricSystem.Kilo
  val symbol = "NGKcf"
}

object NGMCfs extends EnergyUnit {
  val conversionFactor = EnergyConversions.NGCfMultiplier * MetricSystem.Mega
  val symbol = "NGMcf"
}

object NGm3s extends EnergyUnit {
  val conversionFactor = EnergyConversions.NGm3Multiplier
  val symbol = "NGm3"
}

object OilNo1USGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.No1USGMultiplier
  val symbol = "No1USG"
}
object OilNo1UKGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.No1UKGMultiplier
  val symbol = "No1UKG"
}
object OilNo1Ls extends EnergyUnit {
  val conversionFactor = EnergyConversions.No1LMultiplier
  val symbol = "No1L"
}

object OilNo2USGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.No2USGMultiplier
  val symbol = "No2USG"
}
object OilNo2UKGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.No2UKGMultiplier
  val symbol = "No2UKG"
}
object OilNo2Ls extends EnergyUnit {
  val conversionFactor = EnergyConversions.No2LMultiplier
  val symbol = "No2L"
}

object OilNo4USGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.No4USGMultiplier
  val symbol = "No4USG"
}
object OilNo4UKGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.No4UKGMultiplier
  val symbol = "No4UKG"
}
object OilNo4Ls extends EnergyUnit {
  val conversionFactor = EnergyConversions.No4LMultiplier
  val symbol = "No4L"
}

object OilNo6USGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.No6USGMultiplier
  val symbol = "No6USG"
}
object OilNo6UKGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.No6UKGMultiplier
  val symbol = "No6UKG"
}
object OilNo6Ls extends EnergyUnit {
  val conversionFactor = EnergyConversions.No6LMultiplier
  val symbol = "No6L"
}

object DieselUSGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.DieselUSGMultiplier
  val symbol = "DieselUSG"
}
object DieselUKGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.DieselUKGMultiplier
  val symbol = "DieselUKG"
}
object DieselLs extends EnergyUnit {
  val conversionFactor = EnergyConversions.DieselLMultiplier
  val symbol = "DieselL"
}


object KeroseneUSGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.KeroseneUSGMultiplier
  val symbol = "KeroseneUSG"
}
object KeroseneUKGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.KeroseneUKGMultiplier
  val symbol = "KeroseneUKG"
}
object KeroseneLs extends EnergyUnit {
  val conversionFactor = EnergyConversions.KeroseneLMultiplier
  val symbol = "KeroseneL"
}


object PropaneUSGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.PropaneUSGMultiplier
  val symbol = "PropaneUSG"
}
object PropaneUKGs extends EnergyUnit {
  val conversionFactor = EnergyConversions.PropaneUKGMultiplier
  val symbol = "PropaneUKG"
}
object PropaneLs extends EnergyUnit {
  val conversionFactor = EnergyConversions.PropaneLMultiplier
  val symbol = "PropaneL"
}

object PropaneCfs extends EnergyUnit {
  val conversionFactor = EnergyConversions.PropaneCfMultiplier
  val symbol = "PropaneCf"
}
object PropaneCCfs extends EnergyUnit {
  val conversionFactor = EnergyConversions.PropaneCfMultiplier * MetricSystem.Centi
  val symbol = "PropaneCCf"
}
object PropaneKCfs extends EnergyUnit {
  val conversionFactor = EnergyConversions.PropaneCfMultiplier * MetricSystem.Kilo
  val symbol = "PropaneKCf"
}

object SteamLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.SteamLbMultiplier
  val symbol = "SteamLb"
}

object SteamKLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.SteamLbMultiplier * MetricSystem.Kilo
  val symbol = "SteamKLb"
}

object SteamMLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.SteamLbMultiplier * MetricSystem.Mega
  val symbol = "SteamMlb"
}

object CHWTonHs extends EnergyUnit {
  val conversionFactor = EnergyConversions.CHWTonHMultiplier
  val symbol = "CHWTonH"
}

object CoalATons extends EnergyUnit {
  val conversionFactor = EnergyConversions.CoalATonMultiplier
  val symbol = "CoalATon"
}

object CoalALbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.CoalALbMultiplier
  val symbol = "CoalALb"
}

object CoalAKLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.CoalALbMultiplier * MetricSystem.Kilo
  val symbol = "CoalAKLb"
}

object CoalAMLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.CoalALbMultiplier * MetricSystem.Mega
  val symbol = "CoalAMLb"
}

object CoalATonnes extends EnergyUnit {
  val conversionFactor = EnergyConversions.CoalATonneMultiplier
  val symbol = "CoalATonne"
}


object CoalBitTons extends EnergyUnit {
  val conversionFactor = EnergyConversions.CoalBitTonMultiplier
  val symbol = "CoalBitTon"
}

object CoalBitLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.CoalBitLbMultiplier
  val symbol = "CoalBitLb"
}

object CoalBitKLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.CoalBitLbMultiplier * MetricSystem.Kilo
  val symbol = "CoalBitKLb"
}

object CoalBitMLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.CoalBitLbMultiplier * MetricSystem.Mega
  val symbol = "CoalBitMLb"
}

object CoalBitTonnes extends EnergyUnit {
  val conversionFactor = EnergyConversions.CoalBitTonneMultiplier
  val symbol = "CoalBitTonne"
}

object CokeTons extends EnergyUnit {
  val conversionFactor = EnergyConversions.CokeTonMultiplier
  val symbol = "CokeTon"
}

object CokeLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.CokeLbMultiplier
  val symbol = "CokeLb"
}

object CokeKLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.CokeLbMultiplier * MetricSystem.Kilo
  val symbol = "CokeKLb"
}

object CokeMLbs extends EnergyUnit {
  val conversionFactor = EnergyConversions.CokeLbMultiplier * MetricSystem.Mega
  val symbol = "CokeMLb"
}

object CokeTonnes extends EnergyUnit {
  val conversionFactor = EnergyConversions.CokeTonneMultiplier
  val symbol = "CokeTonne"
}

object WoodTons extends EnergyUnit {
  val conversionFactor = EnergyConversions.WoodTonMultiplier
  val symbol = "WoodTon"
}

object WoodTonnes extends EnergyUnit {
  val conversionFactor = EnergyConversions.WoodTonneMultiplier
  val symbol = "WoodTonne"
}





object EnergyConversions {
  lazy val wattHour = WattHours(1)
  lazy val Wh = wattHour
  lazy val kilowattHour = KilowattHours(1)
  lazy val KWh = kilowattHour
  lazy val megawattHour = MegawattHours(1)
  lazy val MWh = megawattHour
  lazy val gigawattHour = GigawattHours(1)
  lazy val GWh = gigawattHour

  lazy val joule = Joules(1)
  lazy val picojoule = Picojoules(1)
  lazy val nanojoule = Nanojoules(1)
  lazy val microjoule = Microjoules(1)
  lazy val millijoule = Millijoules(1)
  lazy val kilojoule = Kilojoules(1)
  lazy val megajoule = Megajoules(1)
  lazy val gigajoule = Gigajoules(1)
  lazy val terajoule = Terajoules(1)

  lazy val btu = BritishThermalUnits(1)
  lazy val btuMultiplier = 2.930710701722222e-1

  lazy val thermMultiplier = 29300.1
  lazy val NGCfMultiplier = 0.3006909
  lazy val NGm3Multiplier = 10639.359

  lazy val No1USGMultiplier = 40.7368788e-3
  lazy val No1UKGMultiplier = 48.9214745e-3
  lazy val No1LMultiplier = 10.76157e-3

  lazy val No2USGMultiplier = 40.4438077e-3
  lazy val No2UKGMultiplier = 48.5694962e-3
  lazy val No2LMultiplier = 10.684199e-3

  lazy val No4USGMultiplier = 42.7883762e-3
  lazy val No4UKGMultiplier = 51.3850299e-3
  lazy val No4LMultiplier = 11.303458e-3

  lazy val No6USGMultiplier = 43.9606605e-3
  lazy val No6UKGMultiplier = 52.7929434e-3
  lazy val No6LMultiplier = 11.613234e-3

  lazy val DieselUSGMultiplier = 40.4438077e-3
  lazy val DieselUKGMultiplier = 48.5694962e-3
  lazy val DieselLMultiplier = 10.684199e-3

  lazy val KeroseneUSGMultiplier = 39.5645945e-3
  lazy val KeroseneUKGMultiplier = 47.5135611e-3
  lazy val KeroseneLMultiplier = 10.451794e-3

  lazy val PropaneUSGMultiplier = 26.962538e-3
  lazy val PropaneUKGMultiplier = 32.3796641e-3
  lazy val PropaneLMultiplier = 7.1227993e-3
  lazy val PropaneCfMultiplier = .7373668e-3

  lazy val SteamLbMultiplier = .3499269e-3
  lazy val CHWTonHMultiplier = 3.5168528e-3
  lazy val CoalATonMultiplier = 7353.1531506-3
  lazy val CoalATonneMultiplier = 8105.8636991e-3
  lazy val CoalALbMultiplier = 3.6765766-3
  lazy val CoalBitTonMultiplier = 7306.2617794e-3
  lazy val CoalBitTonneMultiplier = 8054.1791505e-3
  lazy val CoalBitLbMultiplier = 3.6531309e-3
  lazy val CokeTonMultiplier = 7268.1625403e-3
  lazy val CokeTonneMultiplier = 8012.2699874e-3
  lazy val CokeLbMultiplier = 3.6340813e-3
  lazy val WoodTonMultiplier = 5122.8823066e-3
  lazy val WoodTonneMultiplier = 4647.2279597e-3


  implicit class EnergyConversions[A](n: A)(implicit num: Numeric[A]) {
    def J = Joules(n)
    def joules = Joules(n)
    def pJ = Picojoules(n)
    def picojoules = Picojoules(n)
    def nJ = Nanojoules(n)
    def nanojoules = Nanojoules(n)
    def µJ = Microjoules(n)
    def microjoules = Microjoules(n)
    def mJ = Millijoules(n)
    def milljoules = Millijoules(n)
    def kJ = Kilojoules(n)
    def kilojoules = Kilojoules(n)
    def MJ = Megajoules(n)
    def megajoules = Megajoules(n)
    def GJ = Gigajoules(n)
    def gigajoules = Gigajoules(n)
    def TJ = Terajoules(n)
    def terajoules = Terajoules(n)

    def Wh = WattHours(n)
    def KWh = KilowattHours(n)
    def MWh = MegawattHours(n)
    def GWh = GigawattHours(n)
    def Btu = BritishThermalUnits(n)
    def KBtu = KBtus(n)
    def MBtu = MBtus(n)
    def MMBtu = MMBtus(n)
    def ergs = Ergs(n)

    def therm = Therms(n)
    def NGcf = NGCfs(n)
    def NGCcf = NGCCfs(n)
    def NGKcf = NGKCfs(n)
    def NGMcf = NGMCfs(n)
    def NGm3 = NGm3s(n)

    def No1USG = OilNo1USGs(n)
    def No1UKG = OilNo1UKGs(n)
    def No1L = OilNo1Ls(n)

    def No2USG = OilNo2USGs(n)
    def No2UKG = OilNo2UKGs(n)
    def No2L = OilNo2Ls(n)

    def No4USG = OilNo4USGs(n)
    def No4UKG = OilNo4UKGs(n)
    def No4L = OilNo4Ls(n)

    def No6USG = OilNo6USGs(n)
    def No6UKG = OilNo6UKGs(n)
    def No6L = OilNo6Ls(n)

    def DieselUSG = DieselUSGs(n)
    def DieselUKG = DieselUKGs(n)
    def DieselL = DieselLs(n)

    def KeroseneUSG = KeroseneUSGs(n)
    def KeroseneUKG = KeroseneUKGs(n)
    def KeroseneL = KeroseneLs(n)

    def SteamLb = SteamLbs(n)
    def CHWTonH = CHWTonHs(n)
    def CoalATon = CoalATons(n)
    def CoalATonne = CoalATonnes(n)
    def CoalALb = CoalALbs(n)
    def CoalAKLb = CoalAKLbs(n)

    def CoalAMLb = CoalAMLbs(n)
    def CoalBitTonne = CoalBitTonnes(n)
    def CoalBitTon = CoalBitTons(n)
    def CoalBitLb = CoalBitLbs(n)
    def CoalBitKLb = CoalBitKLbs(n)
    def CoalBitMLb = CoalBitMLbs(n)

    def CokeTon = CokeTons(n)
    def CokeLb = CokeLbs(n)
    def CokeKLb = CokeKLbs(n)
    def CokeMLb = CokeMLbs(n)

    def WoodTon = WoodTons(n)
    def WoodTonne = WoodTonnes(n)

    def wattHours = WattHours(n)
    def kilowattHours = KilowattHours(n)
    def megawattHours = MegawattHours(n)
    def gigawattHours = GigawattHours(n)
  }

  implicit class EnergyStringConversions(s: String) {
    def toEnergy = Energy(s)
  }

  implicit object EnergyNumeric extends AbstractQuantityNumeric[Energy](Energy.primaryUnit)
}