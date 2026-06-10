// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.model.Observation

class cassRotator extends OdbSuite:

  val pi         = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  def cassRotatorQuery(oid: Observation.Id) =
    s"""
      query {
        observation(observationId: "$oid") {
          instrument
          targetEnvironment {
            cassRotator
          }
        }
      }
    """

  test("cassRotator is following for other instruments"):
    for
      p <- createProgramAs(pi)
      t <- createTargetAs(pi, p)
      o <- createGmosNorthImagingObservationAs(pi, p, t)
      _ <- expect(
             pi,
             cassRotatorQuery(o),
             json"""{
              "observation": {
                "instrument": "GMOS_NORTH",
                "targetEnvironment": {
                  "cassRotator": "FOLLOWING"
                  }
                }
              }""".asRight
           )
    yield ()

  test("cassRotator is fixed for Maroon-X"):
    for
      p <- createProgramAs(pi)
      t <- createTargetAs(pi, p)
      o <- createVisitorModeObservationAs(pi, p, VisitorObservingModeType.MaroonX, t)
      _ <- expect(
             pi,
             cassRotatorQuery(o),
             json"""{
              "observation": {
                "instrument": "MAROON_X",
                "targetEnvironment": {
                  "cassRotator": "FIXED"
                  }
                }
              }""".asRight
           )
    yield ()
