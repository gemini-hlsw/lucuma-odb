// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.Gender
import lucuma.core.enums.Partner
import lucuma.core.enums.Partner.US
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.core.util.Gid

class updateProgramUsers extends OdbSuite {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val guest1  = TestUsers.guest(3)
  val guest2  = TestUsers.guest(4)
  val staff   = TestUsers.Standard.staff(5, 34)
  val pi3     = TestUsers.Standard.pi(6, 35)

  val piCharles = TestUsers.Standard(
    7,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(7).get),
    primaryEmail = "charles@guiteau.com".some
  )

  val piLeon    = TestUsers.Standard(
    8,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(8).get),
    primaryEmail = "leon@czolgosz.edu".some
  )

  val service = TestUsers.service(10)

  val validUsers = List(pi, pi2, pi3, guest1, guest2, staff, piCharles, piLeon, service).toList

  def updateUserMutation(u: User, pl: PartnerLink): String =
    s"""
      mutation {
        updateProgramUsers(
          input: {
            SET: {
              partnerLink: {
                ${pl.fold("linkType: HAS_UNSPECIFIED_PARTNER", "linkType: HAS_NON_PARTNER", p => s"partner: ${p.tag.toScreamingSnakeCase}")}
              }
            }
            WHERE: {
              user: {
                id: { EQ: "${u.id}" }
              }
            }
          }
        ) {
          hasMore
          programUsers {
            program { id }
            user { id }
            partnerLink {
              linkType
              ... on HasPartner {
                partner
              }
            }
          }
        }
      }
    """

  def updateUserEducationalStatus(p: Program.Id, u: User, es: Option[EducationalStatus]): String =
    s"""
      mutation {
        updateProgramUsers(
          input: {
            SET: {
              educationalStatus: ${es.map(_.tag.toScreamingSnakeCase).getOrElse("null")}
            }
            WHERE: {
              user: {
                id: { EQ: "${u.id}" }
              },
              program: {
                id: { EQ: "${p.show}" }
              }
            }
          }
        ) {
          programUsers {
            program { id }
            user { id }
            educationalStatus
          }
        }
      }
    """

  def updateUserThesisFlag(p: Program.Id, u: User, th: Boolean): String =
    s"""
      mutation {
        updateProgramUsers(
          input: {
            SET: {
              thesis: $th
            }
            WHERE: {
              user: {
                id: { EQ: "${u.id}" }
              },
              program: {
                id: { EQ: "${p.show}" }
              }
            }
          }
        ) {
          programUsers {
            program { id }
            user { id }
            thesis
          }
        }
      }
    """

  def updateUserGender(p: Program.Id, u: User, g: Option[Gender]): String =
    s"""
      mutation {
        updateProgramUsers(
          input: {
            SET: {
              gender: ${g.map(_.tag.toScreamingSnakeCase).getOrElse("null")}
            }
            WHERE: {
              user: {
                id: { EQ: "${u.id}" }
              },
              program: {
                id: { EQ: "${p.show}" }
              }
            }
          }
        ) {
          programUsers {
            program { id }
            user { id }
            gender
          }
        }
      }
    """

  def expected(ts: (Program.Id, User, PartnerLink)*): Json =
    Json.obj(
      "updateProgramUsers" -> Json.obj(
        "hasMore" -> Json.False,
        "programUsers" -> ts.toList.map { case (pid, user, link) =>
          Json.obj(
            "program" -> Json.obj("id" -> pid.asJson),
            "user"    -> Json.obj("id" -> user.id.asJson),
            "partnerLink" -> Json.fromFields(
              ("linkType" -> link.linkType.tag.toScreamingSnakeCase.asJson) :: link.partnerOption.toList.map { p =>
                "partner" -> p.tag.toScreamingSnakeCase.asJson
              }
            )
          )
        }.asJson
      )
    )

  def expectedES(ts: (Program.Id, User, Option[EducationalStatus])*): Json =
    Json.obj(
      "updateProgramUsers" -> Json.obj(
        "programUsers" -> ts.toList.map { case (pid, user, es) =>
          Json.obj(
            "program"           -> Json.obj("id" -> pid.asJson),
            "user"              -> Json.obj("id" -> user.id.asJson),
            "educationalStatus" -> es.map(_.tag.toScreamingSnakeCase.asJson).getOrElse(Json.Null)
          )
        }.asJson
      )
    )

  def expectedThesis(ts: (Program.Id, User, Boolean)*): Json =
    Json.obj(
      "updateProgramUsers" -> Json.obj(
        "programUsers" -> ts.toList.map { case (pid, user, th) =>
          Json.obj(
            "program" -> Json.obj("id" -> pid.asJson),
            "user"    -> Json.obj("id" -> user.id.asJson),
            "thesis"  -> th.asJson
          )
        }.asJson
      )
    )

  def expectedGender(ts: (Program.Id, User, Option[Gender])*): Json =
    Json.obj(
      "updateProgramUsers" -> Json.obj(
        "programUsers" -> ts.toList.map { case (pid, user, g) =>
          Json.obj(
            "program" -> Json.obj("id" -> pid.asJson),
            "user"    -> Json.obj("id" -> user.id.asJson),
            "gender"  -> g.map(_.tag.toScreamingSnakeCase.asJson).getOrElse(Json.Null)
          )
        }.asJson
      )
    )

  test("update pi partner") {
    createProgramAs(pi2) >> createProgramAs(pi).flatMap { pid =>
      expect(
        user     = pi,
        query    = updateUserMutation(pi, PartnerLink.HasPartner(US)),
        expected = expected((pid, pi, PartnerLink.HasPartner(US))).asRight
      )
    }
  }

  test("update coi partner") {
    createProgramAs(pi).flatMap { pid =>
      linkAs(pi, pi2.id, pid, ProgramUserRole.Coi, PartnerLink.HasUnspecifiedPartner) >>
        expect(
          user     = pi,
          query    = updateUserMutation(pi2, PartnerLink.HasNonPartner),
          expected = expected((pid, pi2, PartnerLink.HasNonPartner)).asRight
        )
    }
  }

  test("update pi educational status") {
    createProgramAs(pi).flatMap { pid =>
      linkAs(pi, pi2.id, pid, ProgramUserRole.Coi, PartnerLink.HasUnspecifiedPartner) >>
        expect(
          user     = pi,
          query    = updateUserEducationalStatus(pid, pi2, Some(EducationalStatus.UndergradStudent)),
          expected = expectedES((pid, pi2, Some(EducationalStatus.UndergradStudent))).asRight
        )
    }
  }

  test("unset pi educational status") {
    createProgramAs(pi2) >> createProgramAs(pi).flatMap { pid =>
      linkAs(pi, pi2.id, pid, ProgramUserRole.Coi, PartnerLink.HasUnspecifiedPartner) >>
        expect(
          user     = pi,
          query    = updateUserEducationalStatus(pid, pi2, None),
          expected = expectedES((pid, pi2, None)).asRight
        )
    }
  }

  test("update pi gender") {
    createProgramAs(pi).flatMap { pid =>
      linkAs(pi, pi2.id, pid, ProgramUserRole.Coi, PartnerLink.HasUnspecifiedPartner) >>
        expect(
          user     = pi,
          query    = updateUserGender(pid, pi2, Some(Gender.Other)),
          expected = expectedGender((pid, pi2, Some(Gender.Other))).asRight
        )
    }
  }

  test("unset pi gender") {
    createProgramAs(pi2) >> createProgramAs(pi).flatMap { pid =>
      linkAs(pi, pi2.id, pid, ProgramUserRole.Coi, PartnerLink.HasUnspecifiedPartner) >>
        expect(
          user     = pi,
          query    = updateUserGender(pid, pi2, None),
          expected = expectedGender((pid, pi2, None)).asRight
        )
    }
  }

  test("update pi thesis flag") {
    createProgramAs(pi3) >> createProgramAs(pi).flatMap { pid =>
      linkAs(pi, pi3.id, pid, ProgramUserRole.Coi, PartnerLink.HasUnspecifiedPartner) >>
        expect(
          user     = pi,
          query    = updateUserThesisFlag(pid, pi3, true),
          expected = expectedThesis((pid, pi3, true)).asRight
        )
    }
  }

  test("cannot update another pi's partner as a PI") {
    createProgramAs(piCharles).flatMap { pid =>
      expect(
        user     = piCharles,
        query    = updateUserMutation(pi, PartnerLink.HasPartner(US)),
        expected = expected().asRight
      )
    }
  }

  test("cannot update another pi's partner") {
    createProgramAs(piLeon).flatMap { pid =>
      linkAs(piLeon, piCharles.id, pid, ProgramUserRole.CoiRO, PartnerLink.HasUnspecifiedPartner) >>
      expect(
        user     = piCharles,
        query    = s"""
          mutation {
            updateProgramUsers(
              input: {
                SET: {
                  partnerLink: { linkType: HAS_NON_PARTNER }
                }
                WHERE: {
                  program: {
                    id: { EQ: "$pid" }
                  }
                }
              }
            ) {
              programUsers {
                user { id }
              }
            }
          }
        """,
        expected = json"""
          {
            "updateProgramUsers": {
              "programUsers": []
            }
          }
        """.asRight
      )
    }
  }

  test("cannot update another pi's educational status") {
    createProgramAs(piLeon).flatMap { pid =>
      linkAs(piLeon, piCharles.id, pid, ProgramUserRole.CoiRO, PartnerLink.HasUnspecifiedPartner) >>
      expect(
        user     = piCharles,
        query    = s"""
          mutation {
            updateProgramUsers(
              input: {
                SET: {
                  educationalStatus: PHD
                }
                WHERE: {
                  program: {
                    id: { EQ: "$pid" }
                  }
                }
              }
            ) {
              programUsers {
                user { id }
              }
            }
          }
        """,
        expected = json"""
          {
            "updateProgramUsers": {
              "programUsers": []
            }
          }
        """.asRight
      )
    }
  }

}
