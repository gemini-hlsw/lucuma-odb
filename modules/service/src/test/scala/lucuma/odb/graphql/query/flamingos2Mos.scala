// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

/**
 * Flamingos 2 MOS deliberately produces three partial states, and none of them
 * is a bug: the sequence cannot be generated, the guide environment is
 * unavailable even though lucuma-core declares a guide probe for the mode, and
 * yet the ITC still returns a result so the observation can be planned.
 */
class flamingos2Mos extends OdbSuite with ObservingModeSetupOperations:

  val user: User = TestUsers.service(3)

  override val validUsers: List[User] = List(user)

  private val ObsTime: Timestamp =
    Timestamp.FromString.getOption("2025-02-01T00:00:00Z").get

  private val ObsDuration: TimeSpan =
    1.hourTimeSpan

  private def setup: IO[(Program.Id, Observation.Id, Target.Id)] =
    for
      p <- createProgramAs(user, "Flamingos 2 MOS Testing")
      t <- createTargetWithProfileAs(user, p)
      o <- createFlamingos2MosObservationAs(user, p, List(t))
    yield (p, o, t)

  test("the ITC returns a result"):
    setup.flatMap: (_, oid, tid) =>
      expect(
        user  = user,
        query = s"""
          query {
            observation(observationId: "$oid") {
              itc {
                ... on ItcScienceOnlySpectroscopy {
                  itcType
                  spectroscopyScience {
                    selected {
                      targetId
                      exposureTime { seconds }
                      exposureCount
                    }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "itc": {
                "itcType": "SCIENCE_ONLY_SPECTROSCOPY",
                "spectroscopyScience": {
                  "selected": {
                    "targetId": $tid,
                    "exposureTime": { "seconds": 10.000000 },
                    "exposureCount": 6
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("the mode type is accepted as a query filter"):
    for
      (pid, oid, _) <- setup
      oids          <- observationsWhere(user, s"""program: { id: { EQ: "$pid" } }, observingModeType: { EQ: FLAMINGOS_2_MOS }""")
    yield assertEquals(oids, List(oid))

  test("the sequence cannot be generated"):
    setup.flatMap: (_, oid, _) =>
      expect(
        user     = user,
        query    = s"""
          query {
            executionConfig(observationId: "$oid") {
              instrument
            }
          }
        """,
        expected = List("Flamingos 2 MOS sequence generation is not yet implemented").asLeft
      )

  test("the guide environment is unavailable"):
    setup.flatMap: (_, oid, _) =>
      setObservationTimeAndDuration(user, oid, ObsTime.some, ObsDuration.some) *>
      expect(
        user     = user,
        query    = s"""
          query {
            observation(observationId: "$oid") {
              targetEnvironment {
                guideEnvironment {
                  posAngle { degrees }
                }
              }
            }
          }
        """,
        expected = List("Flamingos 2 MOS sequence generation is not yet implemented").asLeft
      )
