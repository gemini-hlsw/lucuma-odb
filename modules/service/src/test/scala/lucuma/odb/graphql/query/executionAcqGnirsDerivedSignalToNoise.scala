// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.GnirsFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.refined.*

/**
 * The acquisition signal-to-noise follows the ITC brightness classification: Very Bright
 * 30, Bright 20, Faint 10.  The value is published as the *effective*
 * `acquisition.exposureTimeMode`, with `explicitExposureTimeMode` left null, so the
 * science user sees it in the acquisition editor.
 */
class executionAcqGnirsDerivedSignalToNoise extends ExecutionTestSupportForGnirs:

  // Classification comes from the integration time of the fixed-S/N first pass, keyed here
  // on the first science filter so each test can pick a classification:
  //   Y → 0.3s × 1  = 0.3s → Very Bright
  //   J → 2s   × 3  = 6s   → Bright
  //   K → 30s  × 1  = 30s  → Faint
  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode match
      case InstrumentMode.GnirsImaging(ExposureTimeMode.SignalToNoiseMode(_, _), filter, _, _, _, _, _) =>
        filter match
          case GnirsFilter.Y      => IntegrationTime(300.msTimeSpan, 1.refined).some
          case GnirsFilter.J      => IntegrationTime(2.secTimeSpan,  3.refined).some
          case GnirsFilter.K      => IntegrationTime(30.secTimeSpan, 1.refined).some
          case GnirsFilter.H2     => IntegrationTime(2.secTimeSpan,  3.refined).some
          case GnirsFilter.Order4 => IntegrationTime(5.secTimeSpan,  1.refined).some
          case _                  => none
      case InstrumentMode.GnirsImaging(ExposureTimeMode.TimeAndCountMode(time, count, _), _, _, _, _, _, _) =>
        IntegrationTime(time, count).some
      case _ => none

  private def imagingObs(filter: String): IO[Observation.Id] =
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          filters: [ { filter: $filter } ]
        }
      """
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createObservationWithModeAs(pi, p, List(t), mode)
    yield o

  private def acquisitionQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          observingMode {
            gnirsImaging {
              acquisition {
                exposureTimeMode {
                  signalToNoise { value at { nanometers } }
                  timeAndCount { count }
                }
                explicitExposureTimeMode {
                  signalToNoise { value }
                }
              }
            }
          }
        }
      }
    """

  private def expected(effective: Json, explicit: Json): Json =
    json"""
      {
        "observation": {
          "observingMode": {
            "gnirsImaging": {
              "acquisition": {
                "exposureTimeMode": $effective,
                "explicitExposureTimeMode": $explicit
              }
            }
          }
        }
      }
    """

  // The derived `at` is the science exposure time mode's wavelength, stamped when the
  // observing mode was created; only the value follows the classification.
  private def derived(sn: Json): Json =
    json"""{ "signalToNoise": { "value": $sn, "at": { "nanometers": 500.000 } }, "timeAndCount": null }"""

  private def setAcquisition(oid: Observation.Id, acquisition: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { observingMode: { gnirsImaging: { acquisition: $acquisition } } }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  // Generating the acquisition runs the ITC, which is what publishes the derived S/N.
  private def generateAcquisition(oid: Observation.Id): IO[Unit] =
    query(pi, gnirsAcquisitionQueryImaging(oid)).void

  private def gnirsAcquisitionQueryImaging(oid: Observation.Id): String =
    executionConfigQuery(oid, "gnirs", "acquisition", "steps { instrumentConfig { filter } }", None)

  private def check(filter: String, sn: Json): IO[Unit] =
    for
      oid <- imagingObs(filter)
      _   <- generateAcquisition(oid)
      _   <- expect(pi, acquisitionQuery(oid), expected(derived(sn), Json.Null).asRight)
    yield ()

  test("Very Bright classification publishes an acquisition S/N of 30"):
    check("Y", json"30.000")

  test("Bright classification publishes an acquisition S/N of 20"):
    check("J", json"20.000")

  test("Faint classification publishes an acquisition S/N of 10"):
    check("K", json"10.000")

  test("before the ITC has run the acquisition S/N is the Faint fallback"):
    // No classification exists yet, so the value created with the observing mode stands.
    imagingObs("Y").flatMap: oid =>
      expect(pi, acquisitionQuery(oid), expected(derived(json"10.000"), Json.Null).asRight)

  test("an explicit acquisition S/N is never overwritten by the classification"):
    for
      oid <- imagingObs("Y") // would classify Very Bright => 30 if derived
      _   <- setAcquisition(oid, """{ explicitExposureTimeMode: { signalToNoise: { value: 7.0, at: { nanometers: 1250.0 } } } }""")
      _   <- generateAcquisition(oid)
      _   <- expect(pi, acquisitionQuery(oid),
               expected(
                 json"""{ "signalToNoise": { "value": 7.000, "at": { "nanometers": 1250.000 } }, "timeAndCount": null }""",
                 json"""{ "signalToNoise": { "value": 7.000 } }"""
               ).asRight)
    yield ()

  test("clearing an explicit acquisition S/N reverts to the derived value"):
    // Reverting to derived leaves the wavelength alone -- 1250nm here, as the user set it,
    // rather than reverting to the science ETM's 500nm.  Only the S/N follows the
    // classification; the derivation never rewrites `at`.
    for
      oid <- imagingObs("Y")
      _   <- setAcquisition(oid, """{ explicitExposureTimeMode: { signalToNoise: { value: 7.0, at: { nanometers: 1250.0 } } } }""")
      _   <- generateAcquisition(oid)
      _   <- setAcquisition(oid, """{ explicitExposureTimeMode: null }""")
      _   <- generateAcquisition(oid)
      _   <- expect(pi, acquisitionQuery(oid),
               expected(
                 json"""{ "signalToNoise": { "value": 30.000, "at": { "nanometers": 1250.000 } }, "timeAndCount": null }""",
                 Json.Null
               ).asRight)
    yield ()

  test("an explicit acquisition type sets the S/N with no ITC involvement"):
    // BRIGHT => 20, applied when the mutation is applied rather than on the next ITC pass.
    for
      oid <- imagingObs("K") // would classify Faint => 10 if the ITC decided
      _   <- setAcquisition(oid, """{ explicitAcquisitionType: BRIGHT }""")
      _   <- expect(pi, acquisitionQuery(oid), expected(derived(json"20.000"), Json.Null).asRight)
    yield ()

  test("a time-and-count acquisition is explicit and stays put"):
    for
      oid <- imagingObs("Y")
      _   <- setAcquisition(oid, """{ explicitExposureTimeMode: { timeAndCount: { time: { seconds: 4.0 }, count: 2, at: { nanometers: 1250.0 } } } }""")
      _   <- generateAcquisition(oid)
      _   <- expect(pi, acquisitionQuery(oid),
               expected(
                 json"""{ "signalToNoise": null, "timeAndCount": { "count": 2 } }""",
                 json"""{ "signalToNoise": null }"""
               ).asRight)
    yield ()

  test("clearing a time-and-count acquisition converts it back to a derived signal-to-noise"):
    // A derived acquisition is always signal-to-noise, so reverting a time-and-count mode
    // must drop the time and count rather than trip the derived-is-S/N CHECK.
    for
      oid <- imagingObs("Y")
      _   <- setAcquisition(oid, """{ explicitExposureTimeMode: { timeAndCount: { time: { seconds: 4.0 }, count: 2, at: { nanometers: 1250.0 } } } }""")
      _   <- generateAcquisition(oid)
      _   <- setAcquisition(oid, """{ explicitExposureTimeMode: null }""")
      _   <- generateAcquisition(oid)
      _   <- expect(pi, acquisitionQuery(oid),
               expected(
                 json"""{ "signalToNoise": { "value": 30.000, "at": { "nanometers": 1250.000 } }, "timeAndCount": null }""",
                 Json.Null
               ).asRight)
    yield ()
