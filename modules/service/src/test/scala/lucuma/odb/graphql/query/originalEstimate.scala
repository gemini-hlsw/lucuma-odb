// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.traverse.*
import io.circe.ACursor
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SlewStage
import lucuma.core.model.Observation
import lucuma.odb.graphql.mutation.ReplaceGmosNorthSequenceOps

class originalEstimate extends ExecutionTestSupportForGmos with ReplaceGmosNorthSequenceOps:

  // Step estimates for the generated GMOS North long slit sequence, as
  // documented in the `executionDigest` test suite.
  extension (s: String)
    def sec: BigDecimal =
      BigDecimal(s).setScale(6)

  // 4 atoms, each with an arc (67.1) and a flat (57.1)
  val CalibrationTime: BigDecimal =
    ("67.1".sec * 4) + ("57.1".sec * 4)

  // 4 atoms, all of which incur the 1266.1 cost including the science fold
  // move, 3 of them have an additional 2 steps
  val ScienceTime: BigDecimal =
    ("1266.1".sec * 4) + ("1258.19375".sec + "1258.2875".sec) * 3

  val ProgramTime: BigDecimal = CalibrationTime + ScienceTime

  // Two setups at 960 seconds (full setup) each.
  val FullTime: BigDecimal = ProgramTime + 1920

  def originalEstimateQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution {
            originalEstimate {
              setup {
                full { seconds }
                reacquisition { seconds }
              }
              setupCount
              science {
                program { seconds }
                nonCharged { seconds }
                total { seconds }
              }
              fullTimeEstimate {
                program { seconds }
                nonCharged { seconds }
                total { seconds }
              }
            }
          }
        }
      }
    """

  val unrecorded: Json =
    json"""
      {
        "observation": {
          "execution": {
            "originalEstimate": null
          }
        }
      }
    """

  val recorded: Json =
    json"""
      {
        "observation": {
          "execution": {
            "originalEstimate": {
              "setup": {
                "full": { "seconds": 960.000000 },
                "reacquisition": { "seconds": 300.000000 }
              },
              "setupCount": 2,
              "science": {
                "program": { "seconds": ${ProgramTime.asJson} },
                "nonCharged": { "seconds": 0.000000 },
                "total": { "seconds": ${ProgramTime.asJson} }
              },
              "fullTimeEstimate": {
                "program": { "seconds": ${FullTime.asJson} },
                "nonCharged": { "seconds": 0.000000 },
                "total": { "seconds": ${FullTime.asJson} }
              }
            }
          }
        }
      }
    """

  val createObservation: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield o

  test("originalEstimate - null before any visit"):
    createObservation.flatMap: oid =>
      expect(
        user     = pi,
        query    = originalEstimateQuery(oid),
        expected = unrecorded.asRight
      )

  test("originalEstimate - not recorded for a slew visit"):
    val setup: IO[Observation.Id] =
      for
        o <- createObservation
        _ <- addSlewEventAs(serviceUser, o, SlewStage.StartSlew)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = originalEstimateQuery(oid),
        expected = unrecorded.asRight
      )

  test("originalEstimate - recorded with the first observe visit"):
    val setup: IO[Observation.Id] =
      for
        o <- createObservation
        _ <- recordVisitAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = originalEstimateQuery(oid),
        expected = recorded.asRight
      )

  test("originalEstimate - recorded when a slew visit is claimed for observe"):
    val setup: IO[Observation.Id] =
      for
        o <- createObservation
        _ <- addSlewEventAs(serviceUser, o, SlewStage.StartSlew)
        _ <- recordVisitAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = originalEstimateQuery(oid),
        expected = recorded.asRight
      )

  test("originalEstimate - unchanged as the observation executes"):
    val setup: IO[Observation.Id] =
      for
        o  <- createObservation
        v  <- recordVisitAs(serviceUser, o)
        id <- firstScienceAtomStepIds(serviceUser, o)
        _  <- id.traverse(sid => addEndStepEvent(sid, v))
        _  <- recordVisitAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = originalEstimateQuery(oid),
        expected = recorded.asRight
      )

  test("originalEstimate - manual sequence estimate matches its digest"):
    val setup: IO[(lucuma.core.model.Program.Id, Observation.Id)] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- query(
               user  = pi,
               query = s"""
                 mutation {
                   replaceGmosNorthSequence(input: ${input(o, SequenceType.Science, atomInput("Manual", stepInput(GmosNorthFilter.GPrime)))}) {
                     sequence {
                       id
                     }
                   }
                 }
               """
             )
        _ <- recordVisitAs(serviceUser, o)
        _ <- runObscalcUpdate(p, o)
      yield (p, o)

    // Nothing has been executed yet, so the (manual sequence) digest should
    // agree exactly with the recorded original estimate.
    setup.flatMap: (_, oid) =>
      query(
        user  = pi,
        query = s"""
          query {
            observation(observationId: "$oid") {
              execution {
                digest {
                  value {
                    setup {
                      full { microseconds }
                      reacquisition { microseconds }
                    }
                    setupCount
                    science {
                      timeEstimate {
                        program { microseconds }
                        nonCharged { microseconds }
                      }
                    }
                    fullTimeEstimate {
                      program { microseconds }
                      nonCharged { microseconds }
                    }
                  }
                }
                originalEstimate {
                  setup {
                    full { microseconds }
                    reacquisition { microseconds }
                  }
                  setupCount
                  science {
                    program { microseconds }
                    nonCharged { microseconds }
                  }
                  fullTimeEstimate {
                    program { microseconds }
                    nonCharged { microseconds }
                  }
                }
              }
            }
          }
        """
      ).map: js =>
        val exec = js.hcursor.downFields("observation", "execution")
        val dig  = exec.downFields("digest", "value")
        val est  = exec.downField("originalEstimate")

        def micro(c: ACursor, fields: String*): Long =
          fields.foldLeft(c)(_.downField(_)).downField("microseconds").require[Long]

        assertEquals(micro(est, "setup", "full"),          micro(dig, "setup", "full"))
        assertEquals(micro(est, "setup", "reacquisition"), micro(dig, "setup", "reacquisition"))
        assertEquals(est.downField("setupCount").require[Int], dig.downField("setupCount").require[Int])
        assertEquals(micro(est, "science", "program"),     micro(dig, "science", "timeEstimate", "program"))
        assertEquals(micro(est, "science", "nonCharged"),  micro(dig, "science", "timeEstimate", "nonCharged"))
        assertEquals(micro(est, "fullTimeEstimate", "program"),    micro(dig, "fullTimeEstimate", "program"))
        assertEquals(micro(est, "fullTimeEstimate", "nonCharged"), micro(dig, "fullTimeEstimate", "nonCharged"))
