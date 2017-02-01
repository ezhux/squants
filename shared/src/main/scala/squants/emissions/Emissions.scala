package squants.emissions

import squants._
import squants.mass.Mass
import squants.space.{SquareMeters, SquareFeet}

/**
  * Created by clayteeter on 2/1/17.
  */

final class Emissions private (val value: Double, val unit: EmissionsUnit)
  extends Quantity[Emissions] {

  def dimension = Emissions

}


object Emissions extends Dimension[Emissions] {
  private[emissions] def apply[A](n: A, unit: EmissionsUnit)(implicit num: Numeric[A]) = new Emissions(num.toDouble(n), unit)

  def apply = parse _
  def name = "Emissions"
  def primaryUnit = MegagramCO2e
  def siUnit = MegagramCO2e
  def units = Set(
    MegagramCO2e, KilogramCO2e, GramCO2e
  )
}

trait EmissionsUnit extends UnitOfMeasure[Emissions] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = Emissions(n, this)
}

object MegagramCO2e extends EmissionsUnit with PrimaryUnit with SiUnit {
  val symbol = "MgCO2e"
}

object KilogramCO2e extends EmissionsUnit {
  val symbol = "kgCO2e"
  val conversionFactor = 1000D
}

object GramCO2e extends EmissionsUnit {
  val symbol = "gCO2e"
  val conversionFactor = 1000000D
}

object EmissionsConversions {
  lazy val toGramCO2e = GramCO2e(1)
  lazy val toKilogramCO2e = KilogramCO2e(1)
  lazy val toMegagramCO2e = MegagramCO2e(1)

  implicit class EmissionsConversions[A](n: A)(implicit num: Numeric[A]) {

    def toGramsCO2e = GramCO2e(n)
    def toKilogramsCO2e = KilogramCO2e(n)
    def toMegagramsCO2e = MegagramCO2e(n)
  }
}



