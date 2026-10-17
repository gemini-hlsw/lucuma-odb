// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.Target

class observations_firstScienceTarget extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(2, 31)
  val validUsers = List(pi, staff)

  // Two sibling list fields make grackle split the query into a UNION with a
  // LIMIT per branch.  Staff avoids the access-control filter, which would
  // otherwise produce a different SQL shape.
  private def unionQuery(limit: Int): String =
    s"""
      query {
        observations(WHERE: { calibrationRole: { IS_NULL: true } }, LIMIT: $limit) {
          matches {
            id
            targetEnvironment { firstScienceTarget { id } }
            observingMode {
              gmosNorthImaging { filters { filter } }
              gnirsLongSlit { centralWavelengths { centralWavelength { nanometers } } }
            }
          }
        }
      }
    """

  test("multi-target asterisms don't break a LIMIT applied across a UNION"):
    for
      pid  <- createProgramAs(pi)
      t1   <- createTargetAs(pi, pid, "One")
      t2   <- createTargetAs(pi, pid, "Two")
      o1   <- createGmosNorthImagingObservationAs(pi, pid, t1)
      o2   <- createGmosNorthImagingObservationAs(pi, pid, t1, t2)
      o3   <- createGmosNorthImagingObservationAs(pi, pid, t1, t2)
      o4   <- createGmosNorthImagingObservationAs(pi, pid, t1)
      o5   <- createGmosNorthImagingObservationAs(pi, pid, t1)
      all   = List(o1, o2, o3, o4, o5)
      _    <- (1 to all.size).toList.traverse_ : n =>
                query(staff, unionQuery(n)).map: json =>
                  val matches = json.hcursor.downFields("observations", "matches").values.toList.flatten
                  val rows    = matches.map: m =>
                    (
                      m.hcursor.downField("id").as[Observation.Id],
                      m.hcursor.downFields("targetEnvironment", "firstScienceTarget", "id").as[Target.Id]
                    ).tupled
                  assertEquals(rows, all.take(n).map(oid => (oid, t1).asRight), s"LIMIT $n")
    yield ()

  test("firstScienceTarget skips a deleted first target"):
    for
      pid <- createProgramAs(pi)
      t1  <- createTargetAs(pi, pid, "One")
      t2  <- createTargetAs(pi, pid, "Two")
      oid <- createGmosNorthImagingObservationAs(pi, pid, t1, t2)
      _   <- deleteTargetAs(pi, t1)
      _   <- expect(
               pi,
               s"""
                 query {
                   observation(observationId: "$oid") {
                     targetEnvironment { firstScienceTarget { id } }
                   }
                 }
               """,
               json"""
                 { "observation": { "targetEnvironment": { "firstScienceTarget": { "id": $t2 } } } }
               """.asRight
             )
    yield ()
