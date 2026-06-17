// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.model.ProgramUser

class changePrincipalInvestigator extends OdbSuite:
  val pi1      = TestUsers.Standard.pi(nextId, nextId)
  val pi2      = TestUsers.Standard.pi(nextId, nextId)
  val pi3      = TestUsers.Standard.pi(nextId, nextId)
  val staff    = TestUsers.Standard.staff(nextId, nextId)
  val admin    = TestUsers.Standard.admin(nextId, nextId)
  val service  = TestUsers.service(nextId)

  lazy val validUsers = List(pi1, pi2, pi3, staff, admin, service)

  private def changePiQuery(puid: ProgramUser.Id): String =
    s"""
      mutation {
        changePrincipalInvestigator(
          input: {
            programUserId: "$puid"
          }
        ) {
          programUser {
            id
            role
          }
        }
      }
    """

  private def changePiResult(puid: ProgramUser.Id): Json =
    json"""
      {
        "changePrincipalInvestigator": {
          "programUser": {
            "id": $puid,
            "role": ${ProgramUserRole.Pi}
          }
        }
      }
    """

  /** The current role recorded for `puid` within program `pid`, as JSON. */
  private def roleOf(pid: Program.Id, puid: ProgramUser.Id): IO[Json] =
    query(
      user  = service,
      query = s"""
        query {
          program(programId: "$pid") {
            pi    { id role }
            users { id role }
          }
        }
      """
    ).map: js =>
      val c       = js.hcursor.downField("program")
      val entries = c.downField("pi").focus.toList ::: c.downField("users").values.toList.flatten
      entries
        .find(j => j.hcursor.downField("id").focus.contains(puid.asJson))
        .flatMap(_.hcursor.downField("role").focus)
        .getOrElse(Json.Null)

  test("PI can promote a Coi to PI, demoting the current PI to Coi"):
    createUsers(pi2) >>
    createProgramAs(pi1).flatMap: pid =>
      piProgramUserIdAs(pi1, pid).flatMap: oldPi =>
        addProgramUserAs(pi1, pid, ProgramUserRole.Coi).flatMap: coi =>
          linkUserAs(pi1, coi, pi2.id) >>
          expect(
            user     = pi1,
            query    = changePiQuery(coi),
            expected = changePiResult(coi).asRight
          ) >>
          assertIO(piProgramUserIdAs(pi1, pid), coi) >>      // PI pointer moved to the promoted Coi
          assertIO(roleOf(pid, oldPi), ProgramUserRole.Coi.asJson) >> // old PI demoted
          assertIO(roleOf(pid, coi),   ProgramUserRole.Pi.asJson)     // new PI promoted

  List(staff, admin, service).foreach: u =>
    test(s"${u.role.access} can transfer the PI role"):
      createProgramAs(pi1).flatMap: pid =>
        addProgramUserAs(pi1, pid, ProgramUserRole.Coi).flatMap: coi =>
          expect(
            user     = u,
            query    = changePiQuery(coi),
            expected = changePiResult(coi).asRight
          )

  test("A Coi cannot transfer the PI role"):
    createUsers(pi2) >>
    createProgramAs(pi1).flatMap: pid =>
      addProgramUserAs(pi1, pid, ProgramUserRole.Coi).flatMap: coi =>
        linkUserAs(pi1, coi, pi2.id) >>
        expect(
          user     = pi2,
          query    = changePiQuery(coi),
          expected = List(s"User ${pi2.id} is not authorized to perform this operation.").asLeft
        )

  test("A user from another program cannot transfer the PI role"):
    createUsers(pi3) >>
    createProgramAs(pi1).flatMap: pid =>
      addProgramUserAs(pi1, pid, ProgramUserRole.Coi).flatMap: coi =>
        expect(
          user     = pi3,
          query    = changePiQuery(coi),
          expected = List(s"User ${pi3.id} is not authorized to perform this operation.").asLeft
        )

  List(
    ProgramUserRole.CoiRO,
    ProgramUserRole.SupportPrimary,
    ProgramUserRole.SupportSecondary
  ).foreach: role =>
    test(s"Cannot promote a $role to PI"):
      createProgramAs(pi1).flatMap: pid =>
        addProgramUserAs(staff, pid, role).flatMap: pu =>
          expect(
            user     = pi1,
            query    = changePiQuery(pu),
            expected = List("The specified program user must be a coinvestigator.").asLeft
          )
