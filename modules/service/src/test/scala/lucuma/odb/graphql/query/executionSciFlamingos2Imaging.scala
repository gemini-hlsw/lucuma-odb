// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.OdbError

import ObservingModeSetupOperations.*

// Verifies that a Flamingos 2 imaging observation now loads as a sequence
// `ObservingMode` (i.e. `ObservingModeServices.selectObservingMode` returns a
// `flamingos2.imaging.Config` rather than nothing) and flows through
// `GeneratorParamsService`, building an imaging ITC input.  Before this change
// the config never reached the generator and the observation failed with
// "missing observing mode".  Imaging ITC and sequence generation for F2 are
// follow-ups, so generation currently stops at the imaging ITC lookup.
class executionSciFlamingos2Imaging extends ExecutionTestSupportForFlamingos2:

  private def createFlamingos2ImagingObservationAs(
    user: User,
    pid:  Program.Id,
    tids: List[Target.Id]
  ): IO[Observation.Id] =
    query(
      user  = user,
      query = s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson},
            SET: {
              $ConstraintSet,
              targetEnvironment: {
                asterism: ${tids.asJson}
              },
              scienceRequirements: {
                exposureTimeMode: {
                  signalToNoise: {
                    value: 100.0,
                    at: { nanometers: 500 }
                  }
                }
              },
              observingMode: {
                flamingos2Imaging: {
                  filters: [ { filter: Y }, { filter: J } ]
                }
              }
            }
          }) {
            observation { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  test("Flamingos2 imaging config loads; imaging ITC/generation not yet implemented"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2ImagingObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expectOdbError(
        user  = pi,
        query = s"""
          query {
            executionConfig(observationId: "$oid") {
              flamingos2 {
                science {
                  nextAtom { observeClass }
                }
              }
            }
          }
        """,
        expected = {
          case OdbError.InvalidObservation(_, Some(m))
            if m.contains("Imaging ITC lookup is not supported for Flamingos 2 Imaging") => // ok
        }
      )
