// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.json.all.transport.given

// Match an example in the F2 Long Slit requirements spreadsheet
class executionSciFlamingos2_20x5min extends ExecutionTestSupportForFlamingos2:

  val ExposureTime: TimeSpan = 5.minuteTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, NonNegInt.unsafeFrom(20))

  val abba = flamingos2ExpectedScienceAtom(ExposureTime, (0, 15, Enabled), (0, -15, Enabled), (0, -15, Enabled), (0, 15, Enabled))

  test("simple generation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${flamingos2ScienceQuery(none)}
               }
             }
           """,
        expected =
          Json.obj(
            "observation" -> Json.obj(
              "execution" -> Json.obj(
                "config" -> Json.obj(
                  "flamingos2" -> Json.obj(
                    "science" -> Json.obj(
                      "nextAtom" -> abba,
                      "possibleFuture" -> List(
                        abba,
                        abba,
                        flamingos2ExpectedGcals((0, 15)),
                        abba,
                        abba,
                        flamingos2ExpectedGcals((0, 15))
                      ).asJson,
                      "hasMore" -> false.asJson
                    )
                  )
                )
              )
            )
          ).asRight
      )

  test("stop after first step"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        a <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Science)
        s <- recordStepAs(serviceUser, a, Instrument.Flamingos2, flamingos2Science(ExposureTime), StepConfig.Science, sciTelescopeConfig(15), ObserveClass.Science)
        _ <- addEndStepEvent(s)
        _ <- addSequenceEventAs(serviceUser, v, SequenceCommand.Stop)
      yield o

    setup.flatMap: oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${flamingos2ScienceQuery(none)}
               }
             }
           """,
        expected =
          Json.obj(
            "observation" -> Json.obj(
              "execution" -> Json.obj(
                "config" -> Json.obj(
                  "flamingos2" -> Json.obj(
                    "science" -> Json.obj(
                      "nextAtom" -> flamingos2ExpectedScienceAtom(ExposureTime, (0, -15, Enabled), (0, -15, Enabled), (0, 15, Enabled)),
                      "possibleFuture" -> List(
                        // Gcals to finish up the "stopped" block
                        flamingos2ExpectedGcals((0, 15)),

                        // New block.  In theory the operator or scheduler will
                        // stop after the gcals.  At any rate, only 80 minutes
                        // remaining so no mid-period calibration
                        abba,
                        abba,
                        abba,
                        abba,
                        flamingos2ExpectedGcals((0, 15))
                      ).asJson,
                      "hasMore" -> false.asJson
                    )
                  )
                )
              )
            )
          ).asRight
      )