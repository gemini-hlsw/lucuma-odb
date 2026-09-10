// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.User

class updateObservations_GnirsLongSlit extends OdbSuite with UpdateObservationsOps:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  private val dispersionQuery: String = """
    observations {
      observingMode {
        gnirsLongSlit {
          grating
          explicitGrating
          initialGrating
          prism
          explicitPrism
          initialPrism
        }
      }
    }
  """

  private def dispersionExpected(
    grating:         String,
    explicitGrating: Option[String],
    prism:           String,
    explicitPrism:   Option[String]
  ): Either[String, Json] =
    json"""
      {
        "updateObservations": {
          "observations": [
            {
              "observingMode": {
                "gnirsLongSlit": {
                  "grating": $grating,
                  "explicitGrating": $explicitGrating,
                  "initialGrating": "D111",
                  "prism": $prism,
                  "explicitPrism": $explicitPrism,
                  "initialPrism": "MIRROR"
                }
              }
            }
          ]
        }
      }
    """.asRight

  // The default observation is D111 / MIRROR.  Explore edits `grating` and
  // `prism` directly, so they must update the override the same way the
  // `explicit*` fields do.
  test("observing mode: grating and prism edits are persisted"):
    multiUpdateTest(
      pi,
      List(
        (
          "observingMode: { gnirsLongSlit: { prism: LXD } }",
          dispersionQuery,
          dispersionExpected("D111", none, "LXD", "LXD".some)
        ),
        (
          "observingMode: { gnirsLongSlit: { grating: D32 } }",
          dispersionQuery,
          dispersionExpected("D32", "D32".some, "LXD", "LXD".some)
        ),
        (
          "observingMode: { gnirsLongSlit: { grating: null, prism: null } }",
          dispersionQuery,
          dispersionExpected("D111", none, "MIRROR", none)
        ),
        (
          "observingMode: { gnirsLongSlit: { prism: SXD, explicitPrism: LXD } }",
          dispersionQuery,
          dispersionExpected("D111", none, "LXD", "LXD".some)
        )
      ),
      ObservingModeType.GnirsLongSlit.some
    )
