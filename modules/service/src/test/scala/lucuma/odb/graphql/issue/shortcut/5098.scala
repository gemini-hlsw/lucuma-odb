// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import lucuma.core.model.Target
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.query.ExecutionTestSupport

class ShortCut_5098 extends ExecutionTestSupport:

  def removeSED(t: Target.Id) = query(pi,
    s"""
      mutation {
        updateTargets(input: {
          SET: {
            sourceProfile: {
              point: {
                bandNormalized: {
                  sed: null
                }
              }
            }
          }
          WHERE: {
            id: { EQ: "$t"}
          }
        }) {
          targets {
            id
          }
        }
      }
    """)

  test("Detect if target has no SED before calling ITC"):
    val setup =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        _ <- removeSED(t)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield (t, o)
    setup.flatMap { case (tid, oid) =>
      expectOdbError(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   digest {
                     science {
                       timeEstimate {
                         total {
                           seconds
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = {
          case OdbError.SequenceUnavailable(
            Some(s"Could not generate a sequence for the observation $oid: GMOS Long Slit acquisition requires a valid target: target $tid is missing SED")
          ) => // expected
        }
      )
    }
