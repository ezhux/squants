/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package squants.mass

import org.scalatest.{FlatSpec, Matchers}
import squants.motion._
import squants.space.{CubicMeters, SquareMeters}
import squants.time.Seconds
import squants.{MetricSystem, QuantityParseException}

import scala.language.postfixOps

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 */
class MassSpec extends FlatSpec with Matchers {

  behavior of "Mass and its Units of Measure"

  it should "create values using UOM factories" in {
    Micrograms(10.22).toMicrograms should be(10.22)
    Milligrams(10.22).toMilligrams should be(10.22)
    Grams(10.22).toGrams should be(10.22)
    Kilograms(10.22).toKilograms should be(10.22)
    Tonnes(10.22).toTonnes should be(10.22)
    Ounces(10.22).toOunces should be(10.22)
    Pounds(10.22).toPounds should be(10.22)
    Kilopounds(10.22).toKilopounds should be(10.22)
    Megapounds(10.22).toMegapounds should be(10.22)
  }

  it should "create values from properly formatted Strings" in {
    Mass("10.22 mcg").get should be(Micrograms(10.22))
    Mass("10.22 mg").get should be(Milligrams(10.22))
    Mass("10.22 g").get should be(Grams(10.22))
    Mass("10.22 kg").get should be(Kilograms(10.22))
    Mass("10.22 t").get should be(Tonnes(10.22))
    Mass("10.22 oz").get should be(Ounces(10.22))
    Mass("10.22 lb").get should be(Pounds(10.22))
    Mass("10.22 klb").get should be(Kilopounds(10.22))
    Mass("10.22 Mlb").get should be(Megapounds(10.22))
    Mass("10.45 zz").failed.get should be(QuantityParseException("Unable to parse Mass", "10.45 zz"))
    Mass("zz g").failed.get should be(QuantityParseException("Unable to parse Mass", "zz g"))
  }

  it should "properly convert to all supported Units of Measure" in {
    val x = Grams(1)
    x.toMicrograms should be(1 / MetricSystem.Micro)
    x.toMilligrams should be(1 / MetricSystem.Milli)
    x.toGrams should be(1)
    x.toKilograms should be(1 / MetricSystem.Kilo)
    x.toTonnes should be(1 / MetricSystem.Mega)
    x.toPounds should be(1 / Pounds.conversionFactor)
    x.toOunces should be(1 / Ounces.conversionFactor)

    Grams(1000) should be(Kilograms(1))
    Kilograms(0.45359237) should be(Pounds(1))
    Ounces(16) should be(Pounds(1))
  }

  it should "return properly formatted strings for all supported Units of Measure" in {
    Micrograms(1).toString(Micrograms) should be("1.0 mcg")
    Milligrams(1).toString(Milligrams) should be("1.0 mg")
    Grams(1).toString(Grams) should be("1.0 g")
    Kilograms(1).toString(Kilograms) should be("1.0 kg")
    Tonnes(1).toString(Tonnes) should be("1.0 t")
    Pounds(1).toString(Pounds) should be("1.0 lb")
    Ounces(1).toString(Ounces) should be("1.0 oz")
  }

  it should "return Momentum when multiplied by Velocity" in {
    Kilograms(1) * MetersPerSecond(1) should be(NewtonSeconds(1))
  }

  it should "return Force when multiplied by Acceleration" in {
    Kilograms(1) * MetersPerSecondSquared(1) should be(Newtons(1))
  }

  it should "return Volume when divided by Density" in {
    Kilograms(1) / KilogramsPerCubicMeter(1) should be(CubicMeters(1))
  }

  it should "return Time when divided by MassFlowRate" in {
    Kilograms(1) / KilogramsPerSecond(1) should be(Seconds(1))
  }

  it should "return MassFlowRate when divided by Time" in {
    Kilograms(1) / Seconds(1) should be(KilogramsPerSecond(1))
  }

  it should "return Density when divided by Volume" in {
    Kilograms(1) / CubicMeters(1) should be(KilogramsPerCubicMeter(1))
  }

  it should "return Area when divided by AreaDensity" in {
    Kilograms(1) / SquareMeters(1) should be(KilogramsPerSquareMeter(1))
  }

  it should "return AreaDensity when divided by Area" in {
    Kilograms(1) / KilogramsPerSquareMeter(1) should be(SquareMeters(1))
  }

  behavior of "MassConversions"

  it should "provide aliases for single unit values" in {
    import MassConversions._

    microgram should be(Micrograms(1))
    milligram should be(Milligrams(1))
    gram should be(Grams(1))
    kilogram should be(Kilograms(1))
    tonne should be(Tonnes(1))
    pound should be(Pounds(1))
    ounce should be(Ounces(1))
  }

  it should "provide implicit conversion from Double" in {
    import MassConversions._

    val d = 10.22
    d.mcg should be(Micrograms(d))
    d.mg should be(Milligrams(d))
    d.milligrams should be(Milligrams(d))
    d.g should be(Grams(d))
    d.grams should be(Grams(d))
    d.kg should be(Kilograms(d))
    d.kilograms should be(Kilograms(d))
    d.tonnes should be(Tonnes(d))
    d.pounds should be(Pounds(d))
    d.ounces should be(Ounces(d))
  }

  it should "provide implicit conversions from arbitrary Numeric types" in {
    import MassConversions._

    val i = 10
    i.grams should be(Grams(i))

    val bd = BigDecimal(10.22)
    bd.grams should be(Grams(bd))
  }

  it should "provide implicit conversions from String" in {
    import MassConversions._

    "10.45 mcg".toMass.get should be(Micrograms(10.45))
    "10.45 mg".toMass.get should be(Milligrams(10.45))
    "10.45 g".toMass.get should be(Grams(10.45))
    "10.45 kg".toMass.get should be(Kilograms(10.45))
    "10.45 t".toMass.get should be(Tonnes(10.45))
    "10.45 lb".toMass.get should be(Pounds(10.45))
    "10.45 oz".toMass.get should be(Ounces(10.45))
    "10.45 zz".toMass.failed.get should be(QuantityParseException("Unable to parse Mass", "10.45 zz"))
    "zz oz".toMass.failed.get should be(QuantityParseException("Unable to parse Mass", "zz oz"))
  }

  it should "provide Numeric support" in {
    import MassConversions._

    val ms = List(Grams(1000), Kilograms(10))
    ms.sum should be(Kilograms(11))
  }
}
