// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.all._
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.*
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED.*
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.util.TimeSpan
import lucuma.odb.FMain
import lucuma.odb.data.Existence
import lucuma.odb.data.ObservingModeType
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.Tag
import lucuma.odb.data.TargetRole
import lucuma.odb.json.angle.query.given
import lucuma.odb.json.offset.transport.given
import lucuma.odb.json.sequence.given
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.json.stepconfig.given
import lucuma.odb.json.wavelength.query.given
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import natchez.Trace.Implicits.noop
import skunk.*
import skunk.circe.codec.json.{json => jsonCodec}
import skunk.syntax.all.*

import scala.collection.immutable.SortedMap

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
    createObservationAs(user, pid, None, tids: _*)

  private def scienceRequirementsObject(observingMode: ObservingModeType): String =
    observingMode match
      case ObservingModeType.GmosNorthLongSlit |
           ObservingModeType.GmosSouthLongSlit =>
        """{
        mode: SPECTROSCOPY
        spectroscopy: {
          wavelength: { nanometers: 500 }
          resolution: 100
          signalToNoise: 100.0
          wavelengthCoverage: { nanometers: 20 }
          focalPlane: SINGLE_SLIT
          focalPlaneAngle: { microarcseconds: 0 }
        }
      }"""

  private def observingModeObject(observingMode: ObservingModeType): String =
    observingMode match
      case ObservingModeType.GmosNorthLongSlit =>
        """{
          gmosNorthLongSlit: {
            grating: R831_G5302
            filter: R_PRIME
            fpu: LONG_SLIT_0_50
            centralWavelength: { nanometers: 500 }
          }
        }"""
      case ObservingModeType.GmosSouthLongSlit =>
        """{
          gmosSouthLongSlit: {
            grating: B1200_G5321
            filter: R_PRIME
            fpu: LONG_SLIT_0_50
            centralWavelength: { nanometers: 500 }
          }
        }"""

  def createObservationAs(user: User, pid: Program.Id, observingMode: Option[ObservingModeType] = None, tids: Target.Id*): IO[Observation.Id] =
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
                ${observingMode.foldMap { m =>
                  s"""
                    scienceRequirements: ${scienceRequirementsObject(m)}
                    observingMode: ${observingModeObject(m)}
                  """
                }}
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

  def createObservationInGroupAs(user: User, pid: Program.Id, groupId: Option[Group.Id] = None, groupIndex: Option[NonNegShort] = None): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
            programId: ${pid.asJson},
              SET: {
                groupId: ${groupId.asJson}
                groupIndex: ${groupIndex.map(_.value).asJson}
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

  def cloneObservationAs(user: User, oid: Observation.Id): IO[Observation.Id] =
    query(
      user = user,
      query = s"""
        mutation {
          cloneObservation(input: {
            observationId: "$oid"
          }) {
            newObservation { id }
          }
        }
      """
    ).map(_.hcursor.downFields("cloneObservation", "newObservation", "id").require[Observation.Id])

  def updateTargetExistencetAs(user: User, tid: Target.Id, existence: Existence): IO[Unit] =
    query(
      user = user,
      query = s"""
        mutation {
          updateTargets(input: {
            SET: {
              existence: ${existence.tag.toUpperCase()}
            }
            WHERE: {
              id: { EQ: "$tid"}
            }
          }) {
            targets {
              id
              name
            }
          }
        }
      """
    ).void

  def deleteTargetAs(user: User, tid: Target.Id): IO[Unit] =
    updateTargetExistencetAs(user, tid, Existence.Deleted)

  def undeleteTargetAs(user: User, tid: Target.Id): IO[Unit] =
    updateTargetExistencetAs(user, tid, Existence.Present)

  def createGroupAs(user: User, pid: Program.Id, parentGroupId: Option[Group.Id] = None, parentIndex: Option[NonNegShort] = None, minRequired: Option[NonNegShort] = None): IO[Group.Id] =
    query(
      user = user,
      query = s"""
        mutation {
          createGroup(
            input: {
              programId: "$pid"
              SET: {
                parentGroup: ${parentGroupId.asJson.spaces2}
                parentGroupIndex: ${parentIndex.map(_.value).asJson.spaces2}
                minimumRequired: ${minRequired.map(_.value).asJson.spaces2}
              }
            }
          ) {
            group {
              id
            }
          }
        }
        """
    ).map { json =>
      json.hcursor.downFields("createGroup", "group", "id").require[Group.Id]
    }

  def groupElementsAs(user: User, pid: Program.Id, gid: Option[Group.Id]): IO[List[Either[Group.Id, Observation.Id]]] =
    query(user, s"""query { program(programId: "$pid") { allGroupElements { parentGroupId group { id } observation { id } } } }""")
      .map(_
        .hcursor
        .downFields("program", "allGroupElements")
        .require[List[Json]]
        .flatMap { json =>
          val parentId = json.hcursor.downField("parentGroupId").require[Option[Group.Id]]
          if (parentId === gid) then
            val id = json.hcursor.downFields("group", "id").as[Group.Id].toOption.toLeft(json.hcursor.downFields("observation", "id").require[Observation.Id])
            List(id)
          else Nil
        }
      )

  protected def staticConfig(instrument: Instrument): String =
    instrument match {
      case Instrument.GmosNorth =>
        """
          {
            stageMode: FOLLOW_XY
          }
        """

      case Instrument.GmosSouth =>
        """
          {
            stageMode: FOLLOW_XYZ
          }
        """

      case _ => "{}"

    }

  protected def dynamicConfig(instrument: Instrument): String =
    s"""
      instrument: {
        exposure: {
          seconds: 1200
        },
        readout: {
          xBin: ONE,
          yBin: ONE,
          ampCount: TWELVE,
          ampGain: LOW,
          ampReadMode: SLOW
        },
        dtax: TWO,
        roi: FULL_FRAME,
        gratingConfig: {
          grating: ${instrument match {
            case Instrument.GmosNorth => "B1200_G5301"
            case Instrument.GmosSouth => "B1200_G5321"
            case _                    => "EXPECTING_GMOS"
          }},
          order: ONE,
          wavelength: {
            nanometers: 600
          }
        },
        fpu: {
          builtin: LONG_SLIT_0_50
        }
      }
    """

  val stepConfigScience: String =
    """
      stepConfig: {
        science: {
          offset: {
             p: {
               arcseconds: 0
             },
             q: {
               arcseconds: 10
             }
          },
          guiding: ENABLED
        }
      }
    """


  def recordVisitAs(user: User, instrument: Instrument, oid: Observation.Id): IO[Visit.Id] = {
    val name = s"record${instrument.tag}Visit"

    query(
      user = user,
      query =
        s"""
          mutation {
            $name(input: {
              observationId: ${oid.asJson},
              static: ${staticConfig(instrument)}
            }) {
              visit {
                id
              }
            }
          }
        """
    ).map { json =>
      json.hcursor.downFields(name, "visit", "id").require[Visit.Id]
    }
  }

  def recordAtomAs(user: User, instrument: Instrument, vid: Visit.Id, sequenceType: SequenceType = SequenceType.Science, stepCount: Int = 1): IO[Atom.Id] = {
    val name = s"record${instrument.tag}Atom"

    query(
      user = user,
      query =
        s"""
          mutation {
            $name(input: {
              visitId: ${vid.asJson},
              sequenceType: ${sequenceType.tag.toUpperCase},
              stepCount: ${stepCount.asJson}
            }) {
              atomRecord {
                id
              }
            }
          }
        """
    ).map { json =>
      json.hcursor.downFields(name, "atomRecord", "id").require[Atom.Id]
    }
  }

  def recordStepAs(user: User, instrument: Instrument, aid: Atom.Id): IO[Step.Id] =
    recordStepAs(user, aid, instrument, dynamicConfig(instrument), stepConfigScience)

  def recordStepAs(
    user:            User,
    aid:             Atom.Id,
    instrument:      Instrument,
    instrumentInput: String,
    stepConfigInput: String
  ): IO[Step.Id] = {

    val name = s"record${instrument.tag}Step"

    val q = s"""
      mutation {
        $name(input: {
          atomId: ${aid.asJson},
          $instrumentInput,
          $stepConfigInput
        }) {
          stepRecord {
            id
          }
        }
      }
    """

    query(
      user  = user,
      query = q,
    ).map { json =>
      json.hcursor.downFields(name, "stepRecord", "id").require[Step.Id]
    }

  }

  def recordStepAs[D: io.circe.Encoder](
    user:            User,
    aid:             Atom.Id,
    instrument:      Instrument,
    instrumentInput: D,
    stepConfig:      StepConfig
  ): IO[Step.Id] = {

    val name = s"record${instrument.tag}Step"

    def step = stepConfig.asJson.mapObject(_.remove("stepType"))

    val vars = Json.obj(
      "input" -> Json.obj(
        "atomId" -> aid.asJson,
        "instrument" -> instrumentInput.asJson,
        "stepConfig" -> (stepConfig match {
          case StepConfig.Bias          => Json.obj("bias" -> "true".asJson)
          case StepConfig.Dark          => Json.obj("dark" -> "true".asJson)
          case StepConfig.Gcal(_,_,_,_) => Json.obj("gcal" -> step)
          case StepConfig.Science(_,_)  => Json.obj("science" -> step)
          case StepConfig.SmartGcal(_)  => Json.obj("smartGcal" -> step)
        })
      )
    )

    val q = s"""
      mutation RecordStep($$input: ${name.capitalize}Input!) {
        $name(input: $$input) {
          stepRecord {
            id
          }
        }
      }
    """

    query(
      user      = user,
      query     = q,
      variables = vars.asObject
    ).map { json =>
      json.hcursor.downFields(name, "stepRecord", "id").require[Step.Id]
    }
  }

  def recordDatasetAs(
    user:     User,
    sid:      Step.Id,
    filename: String
  ): IO[Dataset.Id] = {
    val q = s"""
      mutation {
        recordDataset(input: {
          stepId: ${sid.asJson},
          filename: "$filename"
        }) {
          dataset {
            id {
              stepId
              index
            }
          }
        }
      }
    """

    query(user = user, query = q).map { json =>
      json.hcursor.downFields("recordDataset", "dataset", "id").require[Dataset.Id]
    }
  }

  def getTargetRoleFromDb(tid: Target.Id): IO[TargetRole] = {
    val query = sql"select c_role from t_target where c_target_id = $target_id".query(target_role)
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(query).use(_.unique(tid)))
  }

  def createGuideTargetIn(
    pid:  Program.Id,
    name: NonEmptyString = "Unnamed Guide".refined,
    sourceProfile: SourceProfile =
      SourceProfile.Point(
        BandNormalized(
          StellarLibrary(StellarLibrarySpectrum.B5III).some,
          SortedMap.empty
        )
      )
  ): IO[Target.Id] = {
    val af = sql"""
      insert into t_target (
        c_program_id,
        c_name,
        c_type,
        c_sid_ra,
        c_sid_dec,
        c_sid_epoch,
        c_source_profile,
        c_role
      )
      select
        $program_id,
        $text_nonempty,
        'sidereal',
        ${right_ascension},
        ${declination},
        ${epoch},
        $jsonCodec,
        $target_role
      returning c_target_id
    """.apply(
      pid,
      name,
      RightAscension.Zero,
      Declination.Zero,
      Epoch.J2000,
      sourceProfile.asJson,
      TargetRole.Guide
    )
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(af.fragment.query(target_id)).use(_.unique(af.argument)))
  }
}
