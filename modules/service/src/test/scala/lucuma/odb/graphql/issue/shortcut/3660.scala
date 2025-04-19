// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.option.*
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ExecutionTestSupport
import munit.IgnoreSuite

/**
 * In this test case, the query requests both the observation `execution` field
 * and the `itc` field.  Neither query should be successful because there is no
 * observing mode.  We expect two errors, one for `execution` and one for `itc`
 * and often actually get them.  Sometimes though only the `itc` error is
 * returned.
 */
@IgnoreSuite
class ShortCut_3660 extends ExecutionTestSupport {

  test("cannot generate, missing mode + failed itc query") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi, "ShortCut 3660")
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithNoModeAs(pi, p, t)
      } yield o

    setup.flatMap { oid =>
      expectOdbErrors(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 itc {
                   science {
                     selected { signalToNoise }
                   }
                 }
                 execution {
                   config {
                     gmosNorth {
                       static {
                         stageMode
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Set(
          OdbError.SequenceUnavailable(s"Could not generate a sequence for the observation $oid: observation is missing observing mode".some),
          OdbError.InvalidObservation(oid, "ITC cannot be queried until the following parameters are defined: observing mode".some)
        )
      )
    }
  }

}