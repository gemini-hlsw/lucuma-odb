// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.Gender
import lucuma.core.enums.Partner.US
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.core.model.UserProfile
import lucuma.core.syntax.string.*
import lucuma.core.util.Gid
import lucuma.refined.*

class updateProgramUsers extends OdbSuite:

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val guest1  = TestUsers.guest(3)
  val guest2  = TestUsers.guest(4)
  val staff   = TestUsers.Standard.staff(5, 34)
  val pi3     = TestUsers.Standard.pi(6, 35)

  val piCharles = TestUsers.Standard(
    7,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(7).get),
    email = "charles@guiteau.com".some
  )

  val piLeon    = TestUsers.Standard(
    8,
    StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(8).get),
    email = "leon@czolgosz.edu".some
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

  def updateUserDataAccess(p: Program.Id, u: User, hasDataAccess: Boolean): String =
    s"""
      mutation {
        updateProgramUsers(
          input: {
            SET: {
              hasDataAccess: $hasDataAccess
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
            hasDataAccess
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

  def updateUserAffiliation(p: Program.Id, u: User, affiliation: Option[NonEmptyString]): String =
    s"""
      mutation {
        updateProgramUsers(
          input: {
            SET: {
              affiliation: ${quotedOption(affiliation.map(_.value))}
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
            affiliation
          }
        }
      }
    """

  def quotedOption(o: Option[String]): String =
    o.fold("null")(s => s"\"$s\"")

  def profileInput(o: Option[UserProfile]): String =
    o.fold("null"): up =>
      s"""
        {
          givenName: ${quotedOption(up.givenName)}
          familyName: ${quotedOption(up.familyName)}
          creditName: ${quotedOption(up.creditName)}
          email: ${quotedOption(up.email)}
        }
      """

  def updatePreferred(p: Program.Id, u: User, profile: Option[UserProfile]): String =
    s"""
      mutation {
        updateProgramUsers(
          input: {
            SET: {
              preferredProfile: ${profileInput(profile)}
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
            preferredProfile {
              givenName
              familyName
              creditName
              email
            }
          }
        }
      }
    """

  def updatePreferredEmail(p: Program.Id, u: User, email: Option[String]): String =
    s"""
      mutation {
        updateProgramUsers(
          input: {
            SET: {
              preferredProfile: {
                email: ${quotedOption(email)}
              }
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
            preferredProfile {
              email
            }
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

  def expectedAffiliation(ts: (Program.Id, User, Option[NonEmptyString])*): Json =
    Json.obj(
      "updateProgramUsers" -> Json.obj(
        "programUsers" -> ts.toList.map { case (pid, user, affiliation) =>
          Json.obj(
            "program"     -> Json.obj("id" -> pid.asJson),
            "user"        -> Json.obj("id" -> user.id.asJson),
            "affiliation" -> affiliation.map(_.value).asJson
          )
        }.asJson
      )
    )

  def expectedDataAccess(ts: (Program.Id, User, Boolean)*): Json =
    Json.obj(
      "updateProgramUsers" -> Json.obj(
        "programUsers" -> ts.toList.map { case (pid, user, th) =>
          Json.obj(
            "program"       -> Json.obj("id" -> pid.asJson),
            "user"          -> Json.obj("id" -> user.id.asJson),
            "hasDataAccess" -> th.asJson
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

  def expectedPreferred(ts: (Program.Id, User, Option[UserProfile])*): Json =
    Json.obj(
      "updateProgramUsers" -> Json.obj(
        "programUsers" -> ts.toList.map { case (pid, user, prof) =>
           val p = prof.getOrElse(UserProfile.Empty)
           Json.obj(
             "program" -> Json.obj("id" -> pid.asJson),
             "user"    -> Json.obj("id" -> user.id.asJson),
             "preferredProfile" -> Json.obj(
               "givenName"  -> p.givenName.fold(Json.Null)(_.asJson),
               "familyName" -> p.familyName.fold(Json.Null)(_.asJson),
               "creditName" -> p.creditName.fold(Json.Null)(_.asJson),
               "email"      -> p.email.fold(Json.Null)(_.asJson)
             )
           )
         }.asJson
      )
    )

  def expectedPreferredEmail(ts: (Program.Id, User, Option[String])*): Json =
    Json.obj(
      "updateProgramUsers" -> Json.obj(
        "programUsers" -> ts.toList.map { case (pid, user, email) =>
           Json.obj(
             "program" -> Json.obj("id" -> pid.asJson),
             "user"    -> Json.obj("id" -> user.id.asJson),
             "preferredProfile" -> Json.obj(
               "email" -> email.fold(Json.Null)(_.asJson)
             )
           )
         }.asJson
      )
    )

  test("update pi partner"):
    createProgramAs(pi2) >>
    createProgramAs(pi).flatMap: pid =>
      expect(
        user     = pi,
        query    = updateUserMutation(pi, PartnerLink.HasPartner(US)),
        expected = expected((pid, pi, PartnerLink.HasPartner(US))).asRight
      )

  test("update coi partner"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        expect(
          user     = pi,
          query    = updateUserMutation(pi2, PartnerLink.HasNonPartner),
          expected = expected((pid, pi2, PartnerLink.HasNonPartner)).asRight
        )

  test("update coi educational status"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        expect(
          user     = pi,
          query    = updateUserEducationalStatus(pid, pi2, Some(EducationalStatus.UndergradStudent)),
          expected = expectedES((pid, pi2, Some(EducationalStatus.UndergradStudent))).asRight
        )

  test("unset coi educational status"):
    createProgramAs(pi2) >> createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        expect(
          user     = pi,
          query    = updateUserEducationalStatus(pid, pi2, None),
          expected = expectedES((pid, pi2, None)).asRight
        )

  test("update coi gender"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        expect(
          user     = pi,
          query    = updateUserGender(pid, pi2, Some(Gender.Other)),
          expected = expectedGender((pid, pi2, Some(Gender.Other))).asRight
        )

  test("unset coi gender"):
    createProgramAs(pi2) >> createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        expect(
          user     = pi,
          query    = updateUserGender(pid, pi2, None),
          expected = expectedGender((pid, pi2, None)).asRight
        )

  test("update coi thesis flag"):
    createProgramAs(pi3) >> createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(pi, mid, pi3.id) >>
        expect(
          user     = pi,
          query    = updateUserThesisFlag(pid, pi3, true),
          expected = expectedThesis((pid, pi3, true)).asRight
        )

  test("update coi affiliation"):
    createProgramAs(pi3) >> createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(pi, mid, pi3.id) >>
        expect(
          user     = pi,
          query    = updateUserAffiliation(pid, pi3, Some("UTFSM".refined)),
          expected = expectedAffiliation((pid, pi3, Some("UTFSM".refined))).asRight
        )

  test("update coi affiliation to null"):
    createProgramAs(pi3) >> createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(pi, mid, pi3.id) >>
        expect(
          user     = pi,
          query    = updateUserAffiliation(pid, pi3, None),
          expected = expectedAffiliation((pid, pi3, None)).asRight
        )

  test("update coi hasDataAccess flag"):
    createProgramAs(pi3) >> createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(pi, mid, pi3.id) >>
        expect(
          user     = pi,
          query    = updateUserDataAccess(pid, pi3, false),
          expected = expectedDataAccess((pid, pi3, false)).asRight
        )

  test("coi cannot update hasDataAccess flag"):
    createProgramAs(pi3) >> createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, partnerLink = PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(pi, mid, pi3.id) >>
        expect(
          user     = pi3,
          query    = updateUserDataAccess(pid, pi3, false),
          expected =
            json"""
              {
                "updateProgramUsers": {
                  "programUsers": []
                }
              }
            """.asRight
        )

  val GavriloPrincip: UserProfile =
    UserProfile(
      "Gavrilo".some,
      "Princip".some,
      "Гаврило Принцип".some,
      "gprincip@mladabosna.org".some
    )

  test("update preferred"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        expect(
          user     = pi,
          query    = updatePreferred(pid, pi2, GavriloPrincip.some),
          expected = expectedPreferred((pid, pi2, GavriloPrincip.some)).asRight
        )

  test("unset preferred"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        query(pi, updatePreferred(pid, pi2, GavriloPrincip.some)) >>
          expect(
            user     = pi,
            query    = updatePreferred(pid, pi2, none),
            expected = expectedPreferred((pid, pi2, none)).asRight
          )

  test("update preferred email"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        expect(
          user     = pi,
          query    = updatePreferredEmail(pid, pi2, GavriloPrincip.email),
          expected = expectedPreferredEmail((pid, pi2, GavriloPrincip.email)).asRight
        )

  test("unset preferred email"):
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        query(pi, updatePreferredEmail(pid, pi2, GavriloPrincip.email)) >>
          expect(
            user     = pi,
            query    = updatePreferredEmail(pid, pi2, none),
            expected = expectedPreferredEmail((pid, pi2, none)).asRight
          )

  test("cannot update another pi's partner as a PI"):
    createProgramAs(piCharles).flatMap: _ =>
      expect(
        user     = piCharles,
        query    = updateUserMutation(pi, PartnerLink.HasPartner(US)),
        expected = expected().asRight
      )

  test("Read-only COI cannot update pi's partner, but can update its own"):
    createProgramAs(piLeon).flatMap: pid =>
      addProgramUserAs(piLeon, pid, ProgramUserRole.CoiRO, PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(piLeon, mid, piCharles.id) >>
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
                "programUsers": [
                  {
                    "user": {
                      "id": ${piCharles.id.asJson}
                    }
                  }
                ]
              }
            }
          """.asRight
        )

  test("Read-only COI cannot update pi's educational status, but can update its own"):
    createProgramAs(piLeon).flatMap: pid =>
      addProgramUserAs(piLeon, pid, ProgramUserRole.CoiRO, PartnerLink.HasUnspecifiedPartner).flatMap: mid =>
        linkUserAs(piLeon, mid, piCharles.id) >>
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
                "programUsers": [
                  {
                    "user": {
                      "id": ${piCharles.id.asJson}
                    }
                  }
                ]
              }
            }
          """.asRight
        )

  test("Read-only COI can update its own profile"):
    createProgramAs(pi2) >>
    createProgramAs(pi).flatMap: pid =>
      addProgramUserAs(pi, pid, ProgramUserRole.CoiRO).flatMap: mid =>
        linkUserAs(pi, mid, pi2.id) >>
        expect(
          user     = pi2,
          query    = updatePreferredEmail(pid, pi2, GavriloPrincip.email),
          expected = expectedPreferredEmail((pid, pi2, GavriloPrincip.email)).asRight
        )
