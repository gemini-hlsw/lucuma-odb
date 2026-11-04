// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.model.Configuration.ObservingMode
import munit.FunSuite

import configurationrequest.query.given

/**
 * The `ObservingMode` decoder reads what the `configuration { observingMode { ... } }` GraphQL
 * selection returns: the mode type, plus a sub-object for each mode that has parameters.  It
 * dispatches on the mode, so a mode with no parameters (only IGRINS-2 long slit now) needs no
 * sub-object, and a mode with one reports that sub-object's own error rather than masking it.
 */
class ConfigurationRequestSuite extends FunSuite:

  private def decode(mode: String, subObject: (String, Json)*): Decoder.Result[ObservingMode] =
    Json.obj(
      (("instrument" -> Json.Null) :: ("mode" -> Json.fromString(mode)) :: subObject.toList)*
    ).as[ObservingMode]

  // The encoder writes exactly what the GraphQL selection returns, so it round trips.
  private def roundTrip(m: ObservingMode): Unit =
    assertEquals(m.asJson.as[ObservingMode], Right(m))

  test("GNIRS imaging round trips its camera and filters"):
    roundTrip(ObservingMode.GnirsImaging(GnirsCamera.LongRed, List(GnirsFilter.Order6, GnirsFilter.H2)))

  test("Flamingos-2 imaging round trips its filters"):
    roundTrip(ObservingMode.Flamingos2Imaging(List(Flamingos2Filter.Y, Flamingos2Filter.J)))

  test("GHOST IFU round trips its resolution mode"):
    GhostResolutionMode.values.foreach(r => roundTrip(ObservingMode.GhostIfu(r)))

  test("GHOST IFU without its sub-object is not decoded by name"):
    assert(decode("GHOST_IFU").isLeft)

  test("IGRINS-2 long slit decodes by name"):
    assertEquals(decode("IGRINS_2_LONG_SLIT"), Right(ObservingMode.Igrins2LongSlit))

  // A mode with parameters is read from its own sub-object, selected by the mode.
  test("GMOS North long slit decodes from its sub-object"):
    assertEquals(
      decode("GMOS_NORTH_LONG_SLIT", "gmosNorthLongSlit" -> json"""{ "grating": "B1200_G5301" }"""),
      Right(ObservingMode.GmosNorthLongSlit(GmosNorthGrating.B1200_G5301))
    )

  test("a visitor mode decodes from the visitor sub-object"):
    assert(
      decode(
        "ALOPEKE_SPECKLE",
        "visitor" -> json"""{ "mode": "ALOPEKE_SPECKLE", "radius": { "microarcseconds": 1000000 } }"""
      ).exists:
        case ObservingMode.Visitor(_, _) => true
        case _                           => false
    )

  // Exchange observations have no `Configuration.ObservingMode`.  `ConfigurationService` relies
  // on this failing, and reports it against the one observation rather than the whole program.
  test("an exchange mode reports the mode it could not decode"):
    assert(decode("EXCHANGE_KECK").left.exists(_.message.contains("couldn't decode mode: EXCHANGE_KECK")))

  // A string that is not an `ObservingModeType` at all fails earlier, in the enum decoder.
  test("a mode that is not an observing mode type at all is rejected"):
    assert(decode("NO_SUCH_MODE").left.exists(_.message.contains("NO_SUCH_MODE")))

  // The whole point of dispatching on the mode: a broken sub-object used to be swallowed by the
  // `orElse` chain and misreported as an unrecognized mode.
  test("a sub-object that fails to decode reports its own error, not an unknown mode"):
    val result = decode("GMOS_NORTH_LONG_SLIT", "gmosNorthLongSlit" -> json"""{ "grating": "NOT_A_GRATING" }""")
    assert(result.isLeft)
    assert(!result.left.exists(_.message.contains("couldn't decode mode")), result.toString)

  test("a mode whose sub-object is missing entirely reports that, not an unknown mode"):
    val result = decode("GMOS_NORTH_LONG_SLIT")
    assert(result.isLeft)
    assert(!result.left.exists(_.message.contains("couldn't decode mode")), result.toString)
