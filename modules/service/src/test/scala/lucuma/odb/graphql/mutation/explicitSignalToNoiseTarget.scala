// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.option.*
import cats.syntax.show.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.OdbError

class explicitSignalToNoiseTarget extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  // Query the observation's signal-to-noise target id (or null).
  private def querySnTarget(oid: Observation.Id): IO[Json] =
    query(
      user = pi,
      query = s"""
        query {
          observation(observationId: "${oid.show}") {
            targetEnvironment {
              explicitSignalToNoiseTarget { id }
            }
          }
        }
      """
    ).map:
      _.hcursor
       .downFields("observation", "targetEnvironment", "explicitSignalToNoiseTarget")
       .focus
       .getOrElse(Json.Null)

  private def assertSnTarget(oid: Observation.Id, tid: Option[Target.Id]): IO[Unit] =
    querySnTarget(oid).assertEquals(tid.fold(Json.Null)(t => json"""{ "id": ${t.asJson} }"""))

  // Set (or clear, when tid is None) the signal-to-noise target via updateObservations.
  private def setSnTarget(oid: Observation.Id, tid: Option[Target.Id]): IO[Json] =
    query(
      user = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: { targetEnvironment: { explicitSignalToNoiseTargetId: ${tid.fold("null")(t => s"\"${t.show}\"")} } }
            WHERE: { id: { EQ: "${oid.show}" } }
          }) {
            observations {
              id
              targetEnvironment { explicitSignalToNoiseTarget { id } }
            }
          }
        }
      """
    )

  private def expectSetSnError(oid: Observation.Id, tid: Target.Id): IO[Unit] =
    expectOdbError(
      user  = pi,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: { targetEnvironment: { explicitSignalToNoiseTargetId: "${tid.show}" } }
            WHERE: { id: { EQ: "${oid.show}" } }
          }) {
            observations { id }
          }
        }
      """,
      expected = {
        case OdbError.InvalidArgument(Some(m)) if m.contains("not in the asterism") => ()
      }
    )

  test("defaults to null"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- assertSnTarget(oid, none)
    yield ()

  test("set and query the signal-to-noise target"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- setSnTarget(oid, t1.some)
      _   <- assertSnTarget(oid, t1.some)
    yield ()

  test("selecting a non-member target fails"):
    for
      pid   <- createProgramAs(pi)
      t0    <- createTargetAs(pi, pid, "Larry")
      t1    <- createTargetAs(pi, pid, "Curly")
      alien <- createTargetAs(pi, pid, "Moe")
      oid   <- createObservationAs(pi, pid, t0, t1)
      _     <- expectSetSnError(oid, alien)
      _     <- assertSnTarget(oid, none)
    yield ()

  test("clearing the signal-to-noise target"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- setSnTarget(oid, t1.some)
      _   <- assertSnTarget(oid, t1.some)
      _   <- setSnTarget(oid, none)
      _   <- assertSnTarget(oid, none)
    yield ()

  test("selecting a different target moves the selection"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- setSnTarget(oid, t0.some)
      _   <- assertSnTarget(oid, t0.some)
      _   <- setSnTarget(oid, t1.some)
      _   <- assertSnTarget(oid, t1.some)
    yield ()

  test("replacing the asterism keeps the selection when the target remains"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      t2  <- createTargetAs(pi, pid, "Moe")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- setSnTarget(oid, t1.some)
      // Replace the asterism with a set that still contains t1.
      _   <- query(
               user = pi,
               query = s"""
                 mutation {
                   updateObservations(input: {
                     SET: { targetEnvironment: { asterism: [ "${t1.show}", "${t2.show}" ] } }
                     WHERE: { id: { EQ: "${oid.show}" } }
                   }) { observations { id } }
                 }
               """
             )
      _   <- assertSnTarget(oid, t1.some)
    yield ()

  test("replacing the asterism drops the selection when the target is removed"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      t2  <- createTargetAs(pi, pid, "Moe")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- setSnTarget(oid, t1.some)
      // Replace the asterism with a set that no longer contains t1.
      _   <- query(
               user = pi,
               query = s"""
                 mutation {
                   updateObservations(input: {
                     SET: { targetEnvironment: { asterism: [ "${t0.show}", "${t2.show}" ] } }
                     WHERE: { id: { EQ: "${oid.show}" } }
                   }) { observations { id } }
                 }
               """
             )
      _   <- assertSnTarget(oid, none)
    yield ()

  private def targetIdList(tids: List[Target.Id]): String =
    tids.map(t => s"\"${t.show}\"").mkString("[ ", ", ", " ]")

  // createObservation with an asterism and (optionally) a signal-to-noise target
  // in the same input.
  private def createObsQuery(pid: Program.Id, asterism: List[Target.Id], sn: Option[Target.Id]): String =
    s"""
      mutation {
        createObservation(input: {
          programId: "${pid.show}"
          SET: {
            targetEnvironment: {
              asterism: ${targetIdList(asterism)}
              ${sn.fold("")(t => s"explicitSignalToNoiseTargetId: \"${t.show}\"")}
            }
          }
        }) {
          observation {
            targetEnvironment { explicitSignalToNoiseTarget { id } }
          }
        }
      }
    """

  // updateObservations editing both the asterism and the signal-to-noise target
  // in the same input.
  private def setBothQuery(oid: Observation.Id, asterism: List[Target.Id], sn: Option[Target.Id]): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: {
            targetEnvironment: {
              asterism: ${targetIdList(asterism)}
              ${sn.fold("")(t => s"explicitSignalToNoiseTargetId: \"${t.show}\"")}
            }
          }
          WHERE: { id: { EQ: "${oid.show}" } }
        }) { observations { id } }
      }
    """

  test("create with asterism and signal-to-noise target in one input"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      _   <- expect(
               user = pi,
               query = createObsQuery(pid, List(t0, t1), t1.some),
               expected = Right(json"""
                 {
                   "createObservation": {
                     "observation": {
                       "targetEnvironment": {
                         "explicitSignalToNoiseTarget": { "id": ${t1.asJson} }
                       }
                     }
                   }
                 }
               """)
             )
    yield ()

  test("create with a signal-to-noise target that is not in the asterism fails"):
    for
      pid   <- createProgramAs(pi)
      t0    <- createTargetAs(pi, pid, "Larry")
      t1    <- createTargetAs(pi, pid, "Curly")
      alien <- createTargetAs(pi, pid, "Moe")
      _     <- expectOdbError(
                 user  = pi,
                 query = createObsQuery(pid, List(t0, t1), alien.some),
                 expected = {
                   case OdbError.InvalidArgument(Some(m)) if m.contains("not in the asterism") => ()
                 }
               )
    yield ()

  test("edit asterism and signal-to-noise target in one input"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      t2  <- createTargetAs(pi, pid, "Moe")
      oid <- createObservationAs(pi, pid, t0, t1)
      // Replace the asterism with [t1, t2] and select t2 (a newly-added target)
      // in the same input.
      _   <- query(user = pi, query = setBothQuery(oid, List(t1, t2), t2.some))
      _   <- assertSnTarget(oid, t2.some)
    yield ()

  test("edit selecting a signal-to-noise target absent from the edited asterism fails"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      t2  <- createTargetAs(pi, pid, "Moe")
      oid <- createObservationAs(pi, pid, t0, t1)
      // Set the asterism to [t0, t2] but try to select t1, which is not in it.
      _   <- expectOdbError(
               user  = pi,
               query = setBothQuery(oid, List(t0, t2), t1.some),
               expected = {
                 case OdbError.InvalidArgument(Some(m)) if m.contains("not in the asterism") => ()
               }
             )
      // The failed transaction leaves no selection.
      _   <- assertSnTarget(oid, none)
    yield ()

  // The following exercise the diff-based `updateAsterisms` mutation (ADD/DELETE),
  // which never runs the signal-to-noise validation/re-apply logic: the selection
  // is affected purely by whether the flagged link row survives the edit.

  test("updateAsterisms: deleting the selected target clears the selection"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- setSnTarget(oid, t1.some)
      _   <- updateAsterisms(pi, List(oid), add = Nil, del = List(t1), exp = List((oid, List(t0))))
      _   <- assertSnTarget(oid, none)
    yield ()

  test("updateAsterisms: deleting a different target keeps the selection"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      t2  <- createTargetAs(pi, pid, "Moe")
      oid <- createObservationAs(pi, pid, t0, t1, t2)
      _   <- setSnTarget(oid, t1.some)
      _   <- updateAsterisms(pi, List(oid), add = Nil, del = List(t0), exp = List((oid, List(t1, t2))))
      _   <- assertSnTarget(oid, t1.some)
    yield ()

  test("updateAsterisms: adding a target keeps the selection"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      t2  <- createTargetAs(pi, pid, "Moe")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- setSnTarget(oid, t1.some)
      _   <- updateAsterisms(pi, List(oid), add = List(t2), del = Nil, exp = List((oid, List(t0, t1, t2))))
      _   <- assertSnTarget(oid, t1.some)
    yield ()

  test("updateAsterisms: adding and deleting the selected target in one patch clears it"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      t2  <- createTargetAs(pi, pid, "Moe")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- setSnTarget(oid, t1.some)
      _   <- updateAsterisms(pi, List(oid), add = List(t2), del = List(t1), exp = List((oid, List(t0, t2))))
      _   <- assertSnTarget(oid, none)
    yield ()

  test("updateAsterisms: deleting the selected target clears it independently per observation"):
    for
      pid  <- createProgramAs(pi)
      t0   <- createTargetAs(pi, pid, "Larry")
      t1   <- createTargetAs(pi, pid, "Curly")
      oid1 <- createObservationAs(pi, pid, t0, t1)
      oid2 <- createObservationAs(pi, pid, t0, t1)
      // Select t1 in the first observation only.
      _    <- setSnTarget(oid1, t1.some)
      // Delete t1 from both observations in a single multi-observation edit.
      _    <- updateAsterisms(pi, List(oid1, oid2), add = Nil, del = List(t1),
                              exp = List((oid1, List(t0)), (oid2, List(t0))))
      _    <- assertSnTarget(oid1, none)
      _    <- assertSnTarget(oid2, none)
    yield ()

  // cloneObservation with an inline SET, returning the clone's signal-to-noise
  // target selection.
  private def cloneQuery(oid: Observation.Id, setBody: String): String =
    s"""
      mutation {
        cloneObservation(input: {
          observationId: "${oid.show}"
          SET: { $setBody }
        }) {
          newObservation {
            targetEnvironment { explicitSignalToNoiseTarget { id } }
          }
        }
      }
    """

  test("cloning copies the explicit signal-to-noise target"):
    for
      pid  <- createProgramAs(pi)
      t0   <- createTargetAs(pi, pid, "Larry")
      t1   <- createTargetAs(pi, pid, "Curly")
      oid  <- createObservationAs(pi, pid, t0, t1)
      _    <- setSnTarget(oid, t1.some)
      oid2 <- cloneObservationAs(pi, oid)
      _    <- assertSnTarget(oid2, t1.some)
      _    <- assertSnTarget(oid, t1.some) // original unchanged
    yield ()

  test("cloning with no selection copies nothing"):
    for
      pid  <- createProgramAs(pi)
      t0   <- createTargetAs(pi, pid, "Larry")
      t1   <- createTargetAs(pi, pid, "Curly")
      oid  <- createObservationAs(pi, pid, t0, t1)
      oid2 <- cloneObservationAs(pi, oid)
      _    <- assertSnTarget(oid2, none)
    yield ()

  test("cloning with a new asterism that drops the selected target clears it"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      t2  <- createTargetAs(pi, pid, "Moe")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- setSnTarget(oid, t1.some)
      _   <- expect(
               user = pi,
               query = cloneQuery(oid, s"""targetEnvironment: { asterism: [ "${t0.show}", "${t2.show}" ] }"""),
               expected = Right(json"""
                 {
                   "cloneObservation": {
                     "newObservation": {
                       "targetEnvironment": { "explicitSignalToNoiseTarget": null }
                     }
                   }
                 }
               """)
             )
      _   <- assertSnTarget(oid, t1.some) // original unchanged
    yield ()

  test("cloning applies an explicit signal-to-noise target from the clone input"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      oid <- createObservationAs(pi, pid, t0, t1)
      // Original has no selection; the clone input selects t0.
      _   <- expect(
               user = pi,
               query = cloneQuery(oid, s"""targetEnvironment: { explicitSignalToNoiseTargetId: "${t0.show}" }"""),
               expected = Right(json"""
                 {
                   "cloneObservation": {
                     "newObservation": {
                       "targetEnvironment": { "explicitSignalToNoiseTarget": { "id": ${t0.asJson} } }
                     }
                   }
                 }
               """)
             )
      _   <- assertSnTarget(oid, none) // original unchanged
    yield ()

  test("cloning with a signal-to-noise target not in the asterism fails cleanly and creates no clone"):
    for
      pid   <- createProgramAs(pi)
      t0    <- createTargetAs(pi, pid, "Larry")
      t1    <- createTargetAs(pi, pid, "Curly")
      alien <- createTargetAs(pi, pid, "Moe")
      oid   <- createObservationAs(pi, pid, t0, t1)
      _     <- expectOdbError(
                 user  = pi,
                 query = cloneQuery(oid, s"""targetEnvironment: { explicitSignalToNoiseTargetId: "${alien.show}" }"""),
                 expected = {
                   case OdbError.InvalidArgument(Some(m)) if m.contains("not in the asterism") => ()
                 }
               )
      // The failed clone must not leave an orphan observation behind.
      obs   <- observationsWhere(pi, s"""program: { id: { EQ: "${pid.show}" } }""")
      _     <- IO(assertEquals(obs, List(oid)))
    yield ()

  // Reproduction for the unique-violation on i_asterism_single_sn_target.
  test("switching the SN target from the later target to the earlier one does not raise 23505"):
    for
      pid <- createProgramAs(pi)
      t0  <- createTargetAs(pi, pid, "Larry")
      t1  <- createTargetAs(pi, pid, "Curly")
      oid <- createObservationAs(pi, pid, t0, t1)
      _   <- setSnTarget(oid, t1.some) // flag the later target
      _   <- assertSnTarget(oid, t1.some)
      _   <- setSnTarget(oid, t0.some) // switch to the earlier target: 23505 today
      _   <- assertSnTarget(oid, t0.some)
    yield ()
