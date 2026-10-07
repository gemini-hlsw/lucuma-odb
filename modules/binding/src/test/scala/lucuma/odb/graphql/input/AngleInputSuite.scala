// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import grackle.Value
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.math.arb.ArbDeclination.given
import lucuma.core.math.arb.ArbRightAscension.given
import lucuma.odb.graphql.binding.Matcher
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class AngleInputSuite extends ScalaCheckSuite {

  private def µas(m: Matcher[? <: Angle], v: Value): Either[String, Long] =
    m.validate(v).map(_.toMicroarcseconds)

  private def µas(m: Matcher[? <: Angle], v: Double): Either[String, Long] =
    µas(m, Value.FloatValue(v))

  // A decimal output value, sent back either as an exact string or as a JSON number.
  private def inputs(bd: BigDecimal): List[Value] =
    List(Value.StringValue(bd.toString), Value.FloatValue(bd.toDouble))

  List(
    ("milliarcseconds", AngleInput.Milliarcseconds, 1.001, 1_001L),
    ("arcseconds",      AngleInput.ArcSeconds,      1.001, 1_001_000L),
    ("arcminutes",      AngleInput.ArcMinutes,      1.5,   90_000_000L),
    ("degrees",         AngleInput.Degrees,         1.001, 3_603_600_000L),
    ("microseconds",    AngleInput.Microseconds,    1.5,   30L),
    ("milliseconds",    AngleInput.Milliseconds,    1.5,   22_500L),
    ("seconds",         AngleInput.Seconds,         1.5,   22_500_000L),
    ("minutes",         AngleInput.Minutes,         1.5,   1_350_000_000L),
    ("hours",           AngleInput.Hours,           1.5,   81_000_000_000L)
  ).foreach { case (name, matcher, value, expected) =>
    test(s"$name keep decimals") {
      assertEquals(µas(matcher, value), Right(expected))
    }
  }

  test("sub-unit fractions are rounded to the nearest unit") {
    assertEquals(µas(AngleInput.Milliarcseconds, 0.0014), Right(1L))
    assertEquals(µas(AngleInput.Milliarcseconds, 0.0019), Right(2L))
    assertEquals(µas(AngleInput.Milliseconds, 0.0019), Right(30L))
  }

  test("negative values wrap around") {
    assertEquals(µas(AngleInput.Milliarcseconds, -1.5), Right(Angle.µasPer360 - 1_500L))
    assertEquals(µas(AngleInput.Milliarcseconds, -0.0019), Right(Angle.µasPer360 - 2L))
  }

  test("values beyond the Long range wrap around the circle") {
    assertEquals(µas(AngleInput.Hours, Value.StringValue("3000000000")), Right(0L))
    assertEquals(µas(AngleInput.Hours, Value.StringValue("3000000001.5")), Right(Angle.µasPer180 / 8))
    assertEquals(µas(AngleInput.Degrees, Value.StringValue("36000000000000000000.5")), Right(Angle.µasPer180 / 360))
  }

  // Output formulas are the ones in AngleMapping (service module).
  private def arcOutput(a: Angle, µasPerUnit: Long): BigDecimal =
    BigDecimal(a.toMicroarcseconds) / µasPerUnit

  List(
    ("milliarcseconds", AngleInput.Milliarcseconds, 1_000L),
    ("arcseconds",      AngleInput.ArcSeconds,      1_000_000L),
    ("arcminutes",      AngleInput.ArcMinutes,      60_000_000L),
    ("degrees",         AngleInput.Degrees,         Angle.µasPerDegree)
  ).foreach { case (name, matcher, µasPerUnit) =>
    property(s"AngleInput $name output round-trips") {
      forAll { (a: Angle) =>
        inputs(arcOutput(a, µasPerUnit)).foreach { v =>
          assertEquals(µas(matcher, v), Right(a.toMicroarcseconds), v)
        }
      }
    }
  }

  List(
    ("microseconds", AngleInput.Microseconds, 1L),
    ("milliseconds", AngleInput.Milliseconds, 1_000L),
    ("seconds",      AngleInput.Seconds,      1_000_000L),
    ("minutes",      AngleInput.Minutes,      60_000_000L),
    ("hours",        AngleInput.Hours,        3_600_000_000L)
  ).foreach { case (name, matcher, µsPerUnit) =>
    property(s"AngleInput $name output round-trips") {
      forAll { (h: HourAngle) =>
        inputs(arcOutput(h, µsPerUnit * 15L)).foreach { v =>
          assertEquals(µas(matcher, v), Right(h.toMicroarcseconds), v)
        }
      }
    }
  }

  // Output formulas are the ones in RightAscensionMapping and DeclinationMapping (service module).
  // Grackle passes every schema field, in schema order, with AbsentValue for the unset ones.
  private def oneOf(names: List[String], field: (String, Value)): Value =
    Value.ObjectValue(names.map(n => n -> (if n == field._1 then field._2 else Value.AbsentValue)))

  private def ra(field: (String, Value)): Either[String, RightAscension] =
    RightAscensionInput.Binding.validate(oneOf(List("microseconds", "degrees", "hours", "hms"), field))

  private def dec(field: (String, Value)): Either[String, Declination] =
    DeclinationInput.Binding.validate(oneOf(List("microarcseconds", "degrees", "dms"), field))

  property("RightAscensionInput hours output round-trips") {
    forAll { (r: RightAscension) =>
      inputs(BigDecimal(r.toHourAngle.toDoubleHours)).foreach { v =>
        assertEquals(ra("hours" -> v), Right(r), v)
      }
    }
  }

  property("RightAscensionInput degrees output round-trips") {
    forAll { (r: RightAscension) =>
      inputs(BigDecimal(r.toAngle.toDoubleDegrees)).foreach { v =>
        assertEquals(ra("degrees" -> v), Right(r), v)
      }
    }
  }

  property("DeclinationInput degrees output round-trips") {
    forAll { (d: Declination) =>
      inputs(BigDecimal(d.toAngle.toDoubleDegrees)).foreach { v =>
        assertEquals(dec("degrees" -> v), Right(d), v)
      }
    }
  }

}
