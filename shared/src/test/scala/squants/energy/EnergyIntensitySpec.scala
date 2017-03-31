package squants.energy

import org.scalatest.{FlatSpec, Matchers}
import squants.MetricSystem

/**
 * Created by clayteeter on 1/31/17.
 */
class EnergyIntensitySpec extends FlatSpec with Matchers {
  behavior of "Energy Intensity"

  it should "create values using UOM factories" in {
    WattHoursPerSquareMeter(1).toWattHoursPerSquareMeter should be(1)
    KilowattHoursPerSquareMeter(1).toKilowattHoursPerSquareMeter should be(1)
    MegawattHoursPerSquareMeter(1).toMegawattHoursPerSquareMeter should be(1)

    JoulesPerSquareMeter(1).toJoulesPerSquareMeter should be(1)
    KilojoulesPerSquareMeter(1).toKilojoulesPerSquareMeter should be(1)
    MegajoulesPerSquareMeter(1).toMegajoulesPerSquareMeter should be(1)

    BtusPerSquareFeet(1).toBtusPerSquareFeet should be(1)
    KilobtusPerSquareFeet(1).toKilobtusPerSquareFeet should be(1)
    MegabtusPerSquareFeet(1).toMegabtusPerSquareFeet should be(1)

  }

  it should "create values from property formated Strings" in {
    Energy("10.22 Wh/mSQ").get should be(WattHoursPerSquareMeter(10.22))
    Energy("10.22 kWh/mSQ").get should be(KilowattHoursPerSquareMeter(10.22))
    Energy("10.22 MWh/mSQ").get should be(MegawattHoursPerSquareMeter(10.22))

    Energy("10.22 J/mSQ").get should be(JoulesPerSquareMeter(10.22))
    Energy("10.22 kJ/mSQ").get should be(KilojoulesPerSquareMeter(10.22))
    Energy("10.22 MJ/mSQ").get should be(MegajoulesPerSquareMeter(10.22))

    Energy("10.22 btu/ftSQ").get should be(BtusPerSquareFeet(10.22))
    Energy("10.22 kbtu/ftSQ").get should be(KilobtusPerSquareFeet(10.22))
    Energy("10.22 Mbtu/ftSQ").get should be(MegabtusPerSquareFeet(10.22))
  }

  it should "property convert to all supported Units of Measure" in {
    val x = WattHoursPerSquareMeter(1)
    x.toWattHoursPerSquareMeter should be(1)
    x.toKilowattHoursPerSquareMeter should be(1 / MetricSystem.Kilo)
    x.toMegawattHoursPerSquareMeter should be(1 / MetricSystem.Mega)

    x.toJoulesPerSquareMeter should be(3600)
    x.toKilojoulesPerSquareMeter should be(3600 / MetricSystem.Kilo +- 0.0000001)
    x.toMegajoulesPerSquareMeter should be(3600 / MetricSystem.Mega +- 0.0000001)

    x.toBtusPerSquareFeet should be(36.727986868114776)
    x.toKilobtusPerSquareFeet should be(36.72798930 / MetricSystem.Kilo +- 0.0000001)
    x.toMegabtusPerSquareFeet should be(36.72798930 / MetricSystem.Mega +- 0.0000001)
  }
}
