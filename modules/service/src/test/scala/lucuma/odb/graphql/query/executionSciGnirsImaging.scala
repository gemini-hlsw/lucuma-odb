// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.ImagingInput
import lucuma.itc.client.InstrumentMode
import lucuma.refined.*

class executionSciGnirsImaging extends ExecutionTestSupportForGnirs:

  // 10s (Bright) for J; 25s (Faint) so the derived read mode differs per filter.
  private val TimeJ = IntegrationTime(10.secondTimeSpan, 2.refined)
  private val TimeH = IntegrationTime(25.secondTimeSpan, 3.refined)

  override def fakeItcImagingResultFor(input: ImagingInput): Option[IntegrationTime] =
    input.mode match
      case InstrumentMode.GnirsImaging(filter = f) =>
        f match
          case GnirsFilter.J      => TimeJ.some
          case GnirsFilter.Order4 => TimeH.some
          case _                  => none
      case _                                       => none

  private def sciAtom(filter: GnirsFilter, exposureTime: TimeSpan, p: Int = 0, q: Int = 0): Json =
    json"""
      {
        "description": null,
        "observeClass": "SCIENCE",
        "steps": [
          {
            "instrumentConfig": {
              "exposure":             { "seconds": ${exposureTime.toSeconds} },
              "coadds":               1,
              "centralWavelength":    { "nanometers": ${filter.centralWavelength.toNanometers.value.value} },
              "filter":               ${filter.tag.toScreamingSnakeCase.asJson},
              "decker":               "ACQUISITION",
              "fpuSlit":              null,
              "fpuOther":             "ACQUISITION",
              "fpuIfu":               null,
              "acquisitionMirrorOut": null,
              "camera":               "SHORT_BLUE",
              "focusMotorSteps":      null,
              "readMode":             ${GnirsReadMode.forExposureTime(exposureTime).tag.toScreamingSnakeCase.asJson}
            },
            "stepConfig": { "stepType": "SCIENCE" },
            "telescopeConfig": ${expectedTelescopeConfig(p, q, lucuma.core.enums.StepGuideState.Enabled)},
            "observeClass": "SCIENCE",
            "breakpoint": "DISABLED"
          }
        ]
      }
    """

  private def expectedScience(atoms: List[Json]): Json =
    json"""
      {
        "executionConfig": {
          "gnirs": {
            "science": {
              "nextAtom": ${atoms.head},
              "possibleFuture": ${atoms.tail.asJson},
              "hasMore": false
            }
          }
        }
      }
    """

  test("grouped, no offsets, no sky (INCREASING)"):
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { grouped: { skyCount: 0 } }
          filters: [ { filter: J }, { filter: ORDER4 } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // Increasing wavelength: J (1.25um) before H/Order4 (1.65um).
    val atoms =
      List.fill(2)(sciAtom(GnirsFilter.J, 10.secondTimeSpan)) ++
      List.fill(3)(sciAtom(GnirsFilter.Order4, 25.secondTimeSpan))

    setup.flatMap: oid =>
      expect(pi, gnirsScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("grouped, no offsets, no sky (DECREASING)"):
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          variant: { grouped: { order: DECREASING, skyCount: 0 } }
          filters: [ { filter: J }, { filter: ORDER4 } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    // Decreasing wavelength: H/Order4 before J.
    val atoms =
      List.fill(3)(sciAtom(GnirsFilter.Order4, 25.secondTimeSpan)) ++
      List.fill(2)(sciAtom(GnirsFilter.J, 10.secondTimeSpan))

    setup.flatMap: oid =>
      expect(pi, gnirsScienceQuery(oid, 100.some), expectedScience(atoms).asRight)

  test("acquisition sequence is empty"):
    val mode =
      s"""
        gnirsImaging: {
          camera: SHORT_BLUE
          filters: [ { filter: J } ]
        }
      """

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t), mode)
      yield o

    setup.flatMap: oid =>
      expect(pi, gnirsAcquisitionQuery(oid),
        json"""
          {
            "executionConfig": {
              "gnirs": {
                "acquisition": null
              }
            }
          }
        """.asRight)
