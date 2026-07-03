// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.chown

import cats.effect.*
import cats.implicits.*
import io.circe.Json
import lucuma.core.model.GuestUser
import lucuma.core.model.Program
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.sso.client.codec.user.EncoderUser
import org.http4s.*
import org.http4s.Method.POST
import org.http4s.Response
import org.http4s.client.dsl.io.*

import Status.{ Ok, Forbidden }

class ChownSuite extends OdbSuite:

  val guest = TestUsers.guest(nextId)
  val pi1, pi2, pi3 = TestUsers.Standard.pi(nextId, nextId)
  val service = TestUsers.service(nextId)
  val validUsers = List(guest, pi1, pi2, pi3, service)  

  def selectVisibleProgramIdsAs(who: User): IO[List[Program.Id]] =
    query(
      user = who,
      query = s"""
        query {
          programs() {
            matches {
              id
            }
          }
        }
      """
    ).map: json =>
      json
        .hcursor
        .downFields("programs", "matches")
        .as[List[Json]]
        .flatMap: list =>
          list.traverse: js =>
            js.hcursor.downField("id").as[Program.Id]
        .toOption
        .get
        .sorted

  def runChownRequestAs(who: User, from: GuestUser, to: StandardUser): Resource[IO, Response[IO]] =
    runHttpRequestAs(who): rootUri =>
      POST(rootUri / "chown")
        .withEntity:
          Json.obj(
            "from" -> EncoderUser(from), 
            "to" -> EncoderUser(to)
          )

  def chownAs(who: User, from: GuestUser, to: StandardUser): IO[(Status, String)] =
    runChownRequestAs(who, from, to).use: res =>
      res.as[String].tupleLeft(res.status)

  test("Guest user should *not* be able to chown guest to standard."):
    assertIO(
      chownAs(guest, guest, pi1),
      (Forbidden, "")
    )

  test("Standard user should *not* be able to chown guest to standard."):
    assertIO(
      chownAs(pi1, guest, pi1),
      (Forbidden, "")
    )

  test("Chown with no programs should work."):
    assertIO(
      chownAs(service, guest, pi1),
      (Ok, s"User ${guest.id} owns no programs, so nothing was done.")
    ) >>
    assertIO(
      selectVisibleProgramIdsAs(pi1),
      Nil
    )

  test("Chown with one program should work."):
    createProgramAs(guest).flatMap: pid =>
      assertIO(
        chownAs(service, guest, pi2),
        (Ok, s"Changed ownership from ${guest.id} to ${pi2.id} for ${pid}.")
      ) >>
      assertIO(
        selectVisibleProgramIdsAs(pi2),
        List(pid)
      )

  test("Chown with many programs should work."):
    createProgramAs(guest).replicateA(3).flatMap: pids =>
      assertIO(
        chownAs(service, guest, pi3),
        (Ok, s"Changed ownership from ${guest.id} to ${pi3.id} for ${pids.mkString(", ")}.")
      ) >>
      assertIO(
        selectVisibleProgramIdsAs(pi3),
        pids
      )
