// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.model.StandardUser

class ShortCut_8802 extends OdbSuite:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)

  override val validUsers = List(pi)

  test("ObservingMode.mode reports the visitor instrument value"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        createVisitorModeObservationAs(pi, pid, VisitorObservingModeType.AlopekeSpeckle, tid).flatMap: oid =>
          expect(
            user  = pi,
            query = s"""
              query {
                observation(observationId: "$oid") {
                  observingMode {
                    mode
                    visitor {
                      mode
                    }
                  }
                }
              }
            """,
            expected = json"""
              {
                "observation": {
                  "observingMode": {
                    "mode": "ALOPEKE_SPECKLE",
                    "visitor": {
                      "mode": "ALOPEKE_SPECKLE"
                    }
                  }
                }
              }
            """.asRight
          )
