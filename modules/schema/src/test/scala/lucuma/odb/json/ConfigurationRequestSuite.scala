// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.literal.*
import lucuma.core.model.Configuration.ObservingMode
import munit.FunSuite

import configurationrequest.query.given

/**
 * The `ObservingMode` decoder reads what the `configuration { observingMode { ... } }` GraphQL
 * selection returns: every mode-specific sub-object, plus the mode type.  A mode with no
 * parameters has no sub-object of its own, so it is recognized by name alone -- and an
 * unrecognized name reaches `ConfigurationService`, which used to report it as an internal error
 * that failed the whole program's canonicalization.
 */
class ConfigurationRequestSuite extends FunSuite:

  private def decodeByName(mode: String): Decoder.Result[ObservingMode] =
    json"""
      {
        "instrument": null,
        "mode": $mode,
        "gmosNorthLongSlit": null,
        "gmosSouthLongSlit": null,
        "gmosNorthImaging": null,
        "gmosSouthImaging": null,
        "gnirsLongSlit": null,
        "gnirsIfu": null,
        "visitor": null
      }
    """.as[ObservingMode]

  test("GNIRS imaging decodes by name"):
    assertEquals(decodeByName("GNIRS_IMAGING"), Right(ObservingMode.GnirsImaging))

  test("Flamingos-2 imaging decodes by name"):
    assertEquals(decodeByName("FLAMINGOS_2_IMAGING"), Right(ObservingMode.Flamingos2Imaging))

  test("GHOST IFU decodes by name"):
    assertEquals(decodeByName("GHOST_IFU"), Right(ObservingMode.GhostIfu))

  test("IGRINS-2 long slit decodes by name"):
    assertEquals(decodeByName("IGRINS_2_LONG_SLIT"), Right(ObservingMode.Igrins2LongSlit))

  test("an unknown mode reports the mode it could not decode"):
    assert(decodeByName("NO_SUCH_MODE").left.exists(_.message.contains("couldn't decode mode: NO_SUCH_MODE")))
