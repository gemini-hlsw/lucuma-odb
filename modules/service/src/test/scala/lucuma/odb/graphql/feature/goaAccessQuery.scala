// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.Semester
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.core.util.Gid

class goaAccessQuery extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 30)
  val pi2   = TestUsers.Standard.pi(2, 32)
  val pi3   = TestUsers.Standard.pi(3, 33)
  val pi4   = TestUsers.Standard.pi(4, 34)
  val staff = TestUsers.Standard.staff(10, 100)

  val goa1  = TestUsers.Standard(
    20,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(200).get),
    email = "charles@guiteau.com".some
  )

  val goa2  = TestUsers.Standard(
    21,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(201).get),
    email = "leon@czolgosz.edu".some
  )

  override protected def goaUsers: Set[User.Id] =
    Set(goa1.id, goa2.id)

  val validUsers = List(pi, pi2, pi3, pi4, staff, goa1, goa2).toList

  val sem2025B   = Semester.unsafeFromString("2025B")

  def queryString(u: User): String =
    s"""
      query {
        goaDataDownloadAccess(orcidId: "${TestUsers.orcidId(u.id.value.value).value}")
      }
    """

  def addUser(pid: Program.Id, user: User, role: ProgramUserRole): IO[Unit] =
    for
      _ <- createUsers(user)
      m <- addProgramUserAs(pi, pid, role)
      _ <- linkUserAs(pi, m, user.id)
    yield ()

  def setup(link: Option[(User, ProgramUserRole)]): IO[ProgramReference] =
    for
      cid <- createCallForProposalsAs(staff, semester = sem2025B)
      pid <- createProgramWithUsPi(pi)
      _   <- addQueueProposal(pi, pid, cid)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- submitProposal(pi, pid)
      _   <- link.fold(IO.unit)((u, r) => addUser(pid, u, r))
      _   <- submitProposal(pi, pid)
      ref <- acceptProposal(staff, pid)
    yield ref.get

  test("GOA data access: PI"):
    for
      ref <- setup(none)
      _   <- expect(
        user     = goa1,
        query    = queryString(pi),
        expected =
          json"""
            {
              "goaDataDownloadAccess": [
                ${ref.asJson}
              ]
            }
          """.asRight
      )
    yield ()

  test("non-GOA, non-staff users may not perform this query"):
    for
      _ <- setup(none)
      _ <- expect(
        user     = pi,
        query    = queryString(pi),
        expected = List("Only the GOA user may access this field.").asLeft
      )
    yield ()

  test("GOA data access: EXTERNAL"):
    for
      ref <- setup((pi2, ProgramUserRole.External).some)
      _   <- expect(
        user     = goa2,
        query    = queryString(pi2),
        expected =
          json"""
            {
              "goaDataDownloadAccess": [
                ${ref.asJson}
              ]
            }
          """.asRight
      )
    yield ()

  test("GOA data access: COI"):
    for
      ref <- setup((pi3, ProgramUserRole.Coi).some)
      _   <- expect(
        user     = goa1,
        query    = queryString(pi3),
        expected =
          json"""
            {
              "goaDataDownloadAccess": [
                ${ref.asJson}
              ]
            }
          """.asRight
      )
    yield ()

  test("GOA data access: COI unfriended"):
    for
      ref <- setup((pi4, ProgramUserRole.Coi).some)
      _   <- query(
        user  = pi,
        query = s"""
          mutation {
            updateProgramUsers(
              input: {
                SET: {
                  hasDataAccess: false
                }
                WHERE: {
                  user: {
                    id: { EQ: "${pi4.id}" }
                  }
                  program: {
                    reference: { label: { EQ: "${ref.label}" } }
                  }
                }
              }
            ) {
              programUsers {
                hasDataAccess
              }
            }
          }
        """
      ).void
      _   <- expect(
        user     = goa2,
        query    = queryString(pi4),
        expected =
          json"""
            {
              "goaDataDownloadAccess": [
              ]
            }
          """.asRight
      )
    yield ()

  private def label(idx: Int): String =
    ProgramReference.fromString.unsafeGet(s"G-2025B-000$idx-Q").label

  test("GOA data access: multiple"):
    expect(
      user     = goa1,
      query    = queryString(pi),
      expected = json"""
        {
          "goaDataDownloadAccess": ${(1 to 5).map(label).asJson}
        }
      """.asRight
    )

  test("GOA data access: by staff"):
    expect(
      user     = staff,
      query    = queryString(pi),
      expected = json"""
        {
          "goaDataDownloadAccess": ${(1 to 5).map(label).asJson}
        }
      """.asRight
    )
