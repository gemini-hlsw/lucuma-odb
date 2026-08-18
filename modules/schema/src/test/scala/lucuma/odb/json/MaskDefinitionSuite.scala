// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import eu.timepit.refined.types.string.NonEmptyString
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.enums.MosSlitPriority
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Redshift
import lucuma.core.math.syntax.units.*
import lucuma.core.model.mos.MosMaskProvenance
import lucuma.core.model.mos.MosObjectId
import lucuma.odb.data.MaskDefinition
import lucuma.odb.data.MaskSlit
import munit.FunSuite

class MaskDefinitionSuite extends FunSuite:
  import maskDefinition.given

  private def slit(along: Angle, across: Angle, tilt: Angle): MaskSlit =
    MaskSlit(
      id               = MosObjectId(1),
      coordinates      = Coordinates.Zero,
      x                = BigDecimal(0),
      y                = BigDecimal(0),
      width            = Angle.fromDoubleArcseconds(1.0),
      length           = Angle.fromDoubleArcseconds(4.0),
      offsetAlongSlit  = along,
      offsetAcrossSlit = across,
      tilt             = tilt,
      priority         = MosSlitPriority.Medium,
      magnitude        = BrightnessValue.unsafeFrom(BigDecimal("20.5")),
      redshift         = Some(Redshift(BigDecimal("0.158")))
    )

  private val negative =
    slit(
      along  = Angle.fromDoubleArcseconds(-1.5),
      across = Angle.fromDoubleArcseconds(-2.8),
      tilt   = Angle.fromDoubleDegrees(-30.0)
    )

  // Angle wraps at a full turn, so an unsigned encoding would report -1.5" as
  // 1295998.5" and send every consumer six turns of arc off target.
  test("negative displacements and tilts encode as negative"):
    val j = negative.asJson
    assertEquals(j.hcursor.downField("offsetAlongSlit").get[BigDecimal]("arcseconds"), Right(BigDecimal("-1.5")))
    assertEquals(j.hcursor.downField("offsetAcrossSlit").get[BigDecimal]("arcseconds"), Right(BigDecimal("-2.8")))
    assertEquals(j.hcursor.downField("tilt").get[BigDecimal]("degrees"), Right(BigDecimal("-30")))
    assertEquals(j.hcursor.downField("tilt").get[String]("dms"), Right("-30:00:00.000000"))
    assert(j.hcursor.downField("offsetAlongSlit").get[Long]("microarcseconds").exists(_ < 0))
    assert(j.hcursor.downField("offsetAlongSlit").get[String]("hms").exists(_.startsWith("-")))

  test("positive displacements are unaffected"):
    val j = slit(Angle.fromDoubleArcseconds(1.5), Angle.Angle0, Angle.fromDoubleDegrees(30.0)).asJson
    assertEquals(j.hcursor.downField("offsetAlongSlit").get[BigDecimal]("arcseconds"), Right(BigDecimal("1.5")))
    assertEquals(j.hcursor.downField("offsetAcrossSlit").get[BigDecimal]("arcseconds"), Right(BigDecimal("0")))
    assertEquals(j.hcursor.downField("tilt").get[BigDecimal]("degrees"), Right(BigDecimal("30")))

  test("a slit survives a round trip"):
    assertEquals(negative.asJson.as[MaskSlit], Right(negative))

  private def definition(slits: List[MaskSlit]): MaskDefinition =
    MaskDefinition(
      name          = NonEmptyString.unsafeFrom("GS2015AQ023-01"),
      instrument          = Instrument.GmosSouth,
      pixelScale          = BigDecimal("0.16").pixelScale,
      pointing            = Coordinates.Zero,
      positionAngle       = Angle.fromDoubleDegrees(160.1),
      dispersionDirection = MosDispersionDirection.Horizontal,
      hasTiltedSlits      = false,
      provenance          = MosMaskProvenance.Empty.copy(softwareVersion = Some("gmmps-1.5.3")),
      slits               = slits
    )

  private val alignmentBox =
    slit(Angle.Angle0, Angle.Angle0, Angle.Angle0)
      .copy(width = Angle.fromDoubleArcseconds(2.0), priority = MosSlitPriority.Acquisition)

  test("summary fields exclude alignment boxes"):
    val s1 = slit(Angle.Angle0, Angle.Angle0, Angle.Angle0)
    val s2 = s1.copy(width = Angle.fromDoubleArcseconds(2.0))
    val j  = definition(List(alignmentBox, s1, s2)).asJson
    assertEquals(j.hcursor.get[Int]("scienceSlitCount"), Right(2))
    assertEquals(j.hcursor.get[Int]("acquisitionSlitCount"), Right(1))
    assertEquals(j.hcursor.downField("averageSlitWidth").get[BigDecimal]("arcseconds"), Right(BigDecimal("1.5")))

  test("a design with no science slits has no average width"):
    val j = definition(List(alignmentBox)).asJson
    assertEquals(j.hcursor.get[Int]("scienceSlitCount"), Right(0))
    assert(j.hcursor.downField("averageSlitWidth").focus.exists(_.isNull))

  test("a definition survives a round trip"):
    val d = definition(List(alignmentBox, negative))
    assertEquals(d.asJson.as[MaskDefinition], Right(d))
