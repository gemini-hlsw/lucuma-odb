// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

class executionState extends ExecutionTestSupportForGmos {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(2)
    )

  def stateQuery(o: Observation.Id): String =
    s"""
       query {
         observation(observationId: "$o") {
           execution {
             executionState
           }
         }
       }
     """

  def stateResult(s: ExecutionState): Json =
    json"""
      {
        "observation": {
          "execution": {
            "executionState": ${s.tag.toScreamingSnakeCase}
          }
        }
      }
    """

  import ExecutionState.*

  test("executionState - NOT_STARTED") {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = stateQuery(oid),
        expected = stateResult(NotStarted).asRight
      )
  }

  test("executionState - ONGOING") {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        ss <- firstScienceAtomStepIds(serviceUser, o)
        _  <- ss.take(3).traverse(sid => addEndStepEvent(sid, v))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = stateQuery(oid),
        expected = stateResult(Ongoing).asRight
      )
  }

  test("executionState - COMPLETED"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))

        ss <- scienceStepIds(serviceUser, o)
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        _  <- ss.traverse(sid => addEndStepEvent(sid, v))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = stateQuery(oid),
        expected = stateResult(Completed).asRight
      )

  // the completion state is computed now from events so there's not a convenient
  // way to determine that the observation is ill-defined vs not started
  test("executionState - NOT_DEFINED".ignore) {
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createIncompleteTargetAs(pi, p) // missing data radial velocity & source profile
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = stateQuery(oid),
        expected = stateResult(NotDefined).asRight
      )
  }

  test("executionState - many"):
    expect(
      user  = pi,
      query = s"""
        query {
          programs(WHERE: { pi: { user: { id: { EQ: "${pi.id}" } } } }) {
            matches {
              observations {
                matches {
                  execution {
                    executionState
                  }
                }
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "programs": {
            "matches": [
              {
                "observations": {
                  "matches": [
                    {
                      "execution": {
                        "executionState": "NOT_STARTED"
                      }
                    }
                  ]
                }
              },
              {
                "observations": {
                  "matches": [
                    {
                      "execution": {
                        "executionState": "ONGOING"
                      }
                    }
                  ]
                }
              },
              {
                "observations": {
                  "matches": [
                    {
                      "execution": {
                        "executionState": "COMPLETED"
                      }
                    }
                  ]
                }
              }
            ]
          }
        }
      """.asRight
    )
}
