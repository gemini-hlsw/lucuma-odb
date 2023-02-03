// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.Tag
import org.checkerframework.checker.units.qual.s

trait DatabaseOperations { this: OdbSuite =>

  def createProgramAs(user: User, name: String = null): IO[Program.Id] =
    query(user, s"mutation { createProgram(input: { SET: { name: ${Option(name).asJson} } }) { program { id } } }").flatMap { js =>
      js.hcursor
        .downField("createProgram")
        .downField("program")
        .downField("id")
        .as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  def createObservationAs(user: User, pid: Program.Id, tids: Target.Id*): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
            programId: ${pid.asJson},
              SET: {
                targetEnvironment: {
                  asterism: ${tids.asJson}
                }
              }
            }) {
              observation {
                id
              }
            }
          }
        """
    ).map { json => 
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  def createTargetAs(
    user: User,
    pid:  Program.Id,
    name: String = "No Name",
    sourceProfile: String =
      """
        sourceProfile: {
          point: {
            bandNormalized: {
              sed: {
                stellarLibrary: B5_III
              }
              brightnesses: []
            }
          }
        }
      """
  ): IO[Target.Id] =
    query(
      user,
      s"""
        mutation {
          createTarget(
            input: {
              programId: ${pid.asJson}
              SET: {
                name: "$name"
                sidereal: {
                  ra: { hours: "0.0" }
                  dec: { degrees: "0.0" }
                  epoch: "J2000.000"
                }
                $sourceProfile
              }
            }
          ) {
            target { id }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downField("createTarget")
        .downField("target")
        .downField("id")
        .as[Target.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  def setAllocationAs(
    user: User,
    pid: Program.Id,
    partner: Tag,
    duration: TimeSpan,
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          setAllocation(input: {
            programId: ${pid.asJson}
            partner:   ${partner.value.toUpperCase}
            duration:  {
              hours: "${duration.toHours}"
            }
          }) {
            allocation {
              partner
              duration {
                microseconds
                milliseconds
                seconds
                minutes
                hours
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "setAllocation" : {
            "allocation" : {
              "partner":  ${partner.asJson},
              "duration": {
                "microseconds": ${duration.toMicroseconds},
                "milliseconds": ${duration.toMilliseconds},
                "seconds": ${duration.toSeconds},
                "minutes": ${duration.toMinutes},
                "hours": ${duration.toHours}
              }
            }
          }
        }
      """.asRight
    )

  def linkAs(
    user: User,
    uid: User.Id,
    pid: Program.Id,
    role: ProgramUserRole,
    supportType: Option[ProgramUserSupportType] = None,
    partner: Option[Partner] = None,
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          linkUser(input: {
            programId: ${pid.asJson}
            userId: ${uid.asJson}
            role: ${role.tag.toUpperCase}
            supportType: ${supportType.fold("null")(_.tag.toUpperCase)}
            supportPartner: ${partner.fold("null")(_.tag.toUpperCase)}
          }) {
            user {
              role
              userId
            }
          }
        }
      """,
      expected = json"""
        {
          "linkUser" : {
            "user": {
              "role" : $role,
              "userId" : $uid
            }
          }
        }
      """.asRight
    )

  def linkCoiAs(user: User, uid: User.Id, pid: Program.Id): IO[Unit] =
    linkAs(user, uid, pid, ProgramUserRole.Coi)

  def linkCoiAs(user: User, arrow: (User.Id, Program.Id)): IO[Unit] =
    linkCoiAs(user, arrow._1, arrow._2)

  def linkObserverAs(user: User, uid: User.Id, pid: Program.Id): IO[Unit] =
    linkAs(user, uid, pid, ProgramUserRole.Observer)

  def linkObserverAs(user: User, arrow: (User.Id, Program.Id)): IO[Unit] =
    linkObserverAs(user, arrow._1, arrow._2)

  def linkStaffSupportAs(user: User, uid: User.Id, pid: Program.Id): IO[Unit] =
    linkAs(user, uid, pid, ProgramUserRole.Support, Some(ProgramUserSupportType.Staff))

  def linkStaffSupportAs(user: User, arrow: (User.Id, Program.Id)): IO[Unit] =
    linkStaffSupportAs(user, arrow._1, arrow._2)

  def linkNgoSupportAs(user: User, uid: User.Id, pid: Program.Id, partner: Partner): IO[Unit] =
    linkAs(user, uid, pid, ProgramUserRole.Support, Some(ProgramUserSupportType.Partner), Some(partner))

  def linkNgoSupportAs(user: User, arrow: (User.Id, Program.Id), partner: Partner): IO[Unit] =
    linkNgoSupportAs(user, arrow._1, arrow._2, partner)

  def createUsers(users: User*): IO[Unit] =
    users.toList.traverse_(createProgramAs(_)) // TODO: something cheaper

  def updateAsterisms( 
    user: User,
    pid:  Program.Id,
    oids: List[Observation.Id],
    add:  List[Target.Id],
    del:  List[Target.Id],
    exp:  List[(Observation.Id, List[Target.Id])]
  ): IO[Unit] =
    expect(
      user = user,
      query =
        s"""
        mutation {
          updateAsterisms(input: {
            programId: ${pid.asJson}
            SET: {
              ${
                 add match {
                   case Nil => ""
                   case ts  => s"ADD: [ ${ts.map(_.show).mkString("\"", "\",\"", "\"")} ]"
                 }
              }
              ${
                del match {
                  case Nil => ""
                  case ts  => s"DELETE: [ ${ts.map(_.show).mkString("\"", "\",\"", "\"")} ]"
                }
              }
            }
            WHERE: {
              id: { IN: [ ${oids.map(_.show).mkString("\"", "\",\"", "\"")} ] }
            }
          }) {
            observations {
              id
              targetEnvironment {
                asterism {
                  id
                }
              }
            }
          }
        }
      """,
      expected =
        json"""
        {
          "updateAsterisms": {
            "observations": 
              ${exp.map { case (oid, ts) =>
                json"""
                  {
                    "id": ${oid.asJson},
                    "targetEnvironment": {
                      "asterism":
                        ${ts.map { tid =>
                          json"""
                            {
                              "id": ${tid.asJson}
                            }
                          """
                        }}
                    }
                  }
                """
            }}
          }
        }
      """.asRight
    )

}