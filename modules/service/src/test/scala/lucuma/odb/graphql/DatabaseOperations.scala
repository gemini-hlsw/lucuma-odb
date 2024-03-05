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
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.enums.StepStage
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.ExecutionEvent.DatasetEvent
import lucuma.core.model.ExecutionEvent.SequenceEvent
import lucuma.core.model.ExecutionEvent.StepEvent
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
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
import lucuma.core.syntax.string.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.FMain
import lucuma.odb.data.Existence
import lucuma.odb.data.ObservingModeType
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.Tag
import lucuma.odb.data.TargetRole
import lucuma.odb.data.UserInvitation
import lucuma.odb.graphql.input.TimeChargeCorrectionInput
import lucuma.odb.json.angle.query.given
import lucuma.odb.json.offset.transport.given
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.json.stepconfig.given
import lucuma.odb.json.wavelength.query.given
import lucuma.odb.syntax.instrument.*
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

  def fetchPid(user: User, pro: ProposalReference): IO[Program.Id] =
    query(user, s"""
      query { program(proposalReference: "${pro.label}") { id } }
    """).flatMap { js =>
      js.hcursor
        .downFields("program", "id")
        .as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  def fetchPid(user: User, prg: ProgramReference): IO[Program.Id] =
    query(user, s"""
      query { program(programReference: "${prg.label}") { id } }
    """).flatMap { js =>
      js.hcursor
        .downFields("program", "id")
        .as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  def fetchProposalReference(user: User, pid: Program.Id): IO[Option[ProposalReference]] =
    query(user, s"""
      query { program(programId: "$pid") { proposalReference } }
    """).flatMap { js =>
      js.hcursor
        .downFields("program", "proposalReference")
        .as[Option[ProposalReference]]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  def fetchProgramReference(user: User, pid: Program.Id): IO[Option[ProgramReference]] =
    query(user, s"""
      query { program(programId: "$pid") { reference { label } } }
    """).flatMap { js =>
      js.hcursor
        .downFields("program", "reference", "label")
        .success
        .traverse(_.as[ProgramReference])
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  // For proposal tests where it doesn't matter what the proposal is, just that
  // there is one.
  def addProposal(user: User, pid: Program.Id): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updatePrograms(
            input: {
              SET: {
                proposal: {
                  proposalClass: {
                    queue: {
                      minPercentTime: 50
                    }
                  }
                  category: COSMOLOGY
                  toOActivation: NONE
                  partnerSplits: [
                    {
                      partner: US
                      percent: 100
                    }
                  ]
                }
              }
              WHERE: {
                id: {
                  EQ: "$pid"
                }
              }
            }
          ) {
            programs {
              id
            }
          }
        }
      """,
      expected =
        Right(json"""
          {
            "updatePrograms" : {
              "programs": [
                {
                  "id" : $pid
                }
              ]
            }
          }
        """)
    )

  def submitProposal(user: User, pid: Program.Id, s: Option[Semester]): IO[ProposalReference] =
    query(user, s"""
        mutation {
          updatePrograms(
            input: {
              SET: {
                proposalStatus: SUBMITTED
                ${s.map(semster => s""",\nsemester: "${semster.format}"""").getOrElse("")}
              }
              WHERE: {
                id: {
                  EQ: "$pid"
                }
              }
            }
          ) {
            programs {
              proposal { reference { label } }
            }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downField("updatePrograms")
        .downField("programs")
        .downArray
        .downFields("proposal", "reference", "label")
        .as[ProposalReference]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  def acceptProposal(user: User, pid: Program.Id): IO[Option[ProgramReference]] =
    query(user, s"""
        mutation {
          updatePrograms(
            input: {
              SET:   { proposalStatus: ACCEPTED }
              WHERE: { id: { EQ: "$pid" } }
            }
          ) {
            programs { reference { label } }
          }
        }
      """
    ).flatMap { js =>
      js.hcursor
        .downField("updatePrograms")
        .downField("programs")
        .downArray
        .downFields("reference", "label")
        .success
        .traverse(_.as[ProgramReference])
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  def createObservationAs(user: User, pid: Program.Id, tids: Target.Id*): IO[Observation.Id] =
    createObservationAs(user, pid, None, tids*)

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
      ${instrument.fieldName}: {
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

  def addTimeChargeCorrection(user: User, vid: Visit.Id, correction: TimeChargeCorrectionInput): IO[Unit] =
    query(
      user  = user,
      query =
        s"""
          mutation {
            addTimeChargeCorrection(input: {
              visitId: "$vid",
              correction: {
                chargeClass: ${correction.chargeClass.tag.toScreamingSnakeCase},
                op: ${correction.op.tag.toScreamingSnakeCase},
                amount: {
                  seconds: "${correction.amount.toSeconds}"
                }
                ${correction.comment.fold("")(c => s", comment: \"${c.value}\"")}
              }
            }) {
              timeChargeInvoice {
                corrections {
                  created
                }
              }
            }
          }
        """
      ).void

  def recordVisitAs(user: User, instrument: Instrument, oid: Observation.Id): IO[Visit.Id] = {
    val name = s"record${instrument.tag}Visit"

    query(
      user = user,
      query =
        s"""
          mutation {
            $name(input: {
              observationId: ${oid.asJson},
              ${instrument.fieldName}: ${staticConfig(instrument)}
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

  def addSequenceEventAs(
    user: User,
    vid:  Visit.Id,
    cmd:  SequenceCommand
  ): IO[SequenceEvent] = {
    val q = s"""
      mutation {
        addSequenceEvent(input: {
          visitId: "$vid",
          command: ${cmd.tag.toUpperCase}
        }) {
          event {
            id
            received
            observation { id }
          }
        }
      }
    """

    query(user = user, query = q).map { json =>
      val c = json.hcursor.downFields("addSequenceEvent", "event")
      val e = for {
        i <- c.downField("id").as[ExecutionEvent.Id]
        r <- c.downField("received").as[Timestamp]
        o <- c.downFields("observation", "id").as[Observation.Id]
      } yield SequenceEvent(i, r, o, vid, cmd)
      e.fold(f => throw new RuntimeException(f.message), identity)
    }
  }


  def recordAtomAs(user: User, instrument: Instrument, vid: Visit.Id, sequenceType: SequenceType = SequenceType.Science, stepCount: Int = 1): IO[Atom.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            recordAtom(input: {
              visitId: ${vid.asJson},
              instrument: ${instrument.tag.toScreamingSnakeCase},
              sequenceType: ${sequenceType.tag.toScreamingSnakeCase},
              stepCount: ${stepCount.asJson}
            }) {
              atomRecord {
                id
              }
            }
          }
        """
    ).map { json =>
      json.hcursor.downFields("recordAtom", "atomRecord", "id").require[Atom.Id]
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
          $stepConfigInput,
          observeClass: ${ObserveClass.Science.tag.toScreamingSnakeCase}
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
    stepConfig:      StepConfig,
    observeClass:    ObserveClass = ObserveClass.Science
  ): IO[Step.Id] = {

    val name = s"record${instrument.tag}Step"

    def step = stepConfig.asJson.mapObject(_.remove("stepType"))

    val vars = Json.obj(
      "input" -> Json.obj(
        "atomId" -> aid.asJson,
        instrument.fieldName -> instrumentInput.asJson,
        "stepConfig" -> (stepConfig match {
          case StepConfig.Bias          => Json.obj("bias" -> "true".asJson)
          case StepConfig.Dark          => Json.obj("dark" -> "true".asJson)
          case StepConfig.Gcal(_,_,_,_) => Json.obj("gcal" -> step)
          case StepConfig.Science(_,_)  => Json.obj("science" -> step)
          case StepConfig.SmartGcal(_)  => Json.obj("smartGcal" -> step)
        }),
        "observeClass" -> observeClass.asJson
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

  def addStepEventAs(
    user:  User,
    sid:   Step.Id,
    stage: StepStage
  ): IO[StepEvent] = {
    val q = s"""
      mutation {
        addStepEvent(input: {
          stepId:    "$sid",
          stepStage: ${stage.tag.toUpperCase}
        }) {
          event {
            id
            received
            observation { id }
            visit { id }
          }
        }
      }
    """

    query(user = user, query = q).map { json =>
      val c = json.hcursor.downFields("addStepEvent", "event")
      val e = for {
        i <- c.downField("id").as[ExecutionEvent.Id]
        r <- c.downField("received").as[Timestamp]
        o <- c.downFields("observation", "id").as[Observation.Id]
        v <- c.downFields("visit", "id").as[Visit.Id]
      } yield StepEvent(i, r, o, v, sid, stage)
      e.fold(f => throw new RuntimeException(f.message), identity)
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
            id
          }
        }
      }
    """

    query(user = user, query = q).map { json =>
      json.hcursor.downFields("recordDataset", "dataset", "id").require[Dataset.Id]
    }
  }

  def addDatasetEventAs(
    user:  User,
    did:   Dataset.Id,
    stage: DatasetStage
  ): IO[DatasetEvent] = {
    val q = s"""
      mutation {
        addDatasetEvent(input: {
          datasetId: "$did",
          datasetStage: ${stage.tag.toUpperCase}
        }) {
          event {
            id
            received
            observation { id }
            visit { id }
            step { id }
          }
        }
      }
    """

    query(user = user, query = q).map { json =>
      val c = json.hcursor.downFields("addDatasetEvent", "event")
      val e = for {
        i <- c.downField("id").as[ExecutionEvent.Id]
        r <- c.downField("received").as[Timestamp]
        o <- c.downFields("observation", "id").as[Observation.Id]
        v <- c.downFields("visit", "id").as[Visit.Id]
        s <- c.downFields("step", "id").as[Step.Id]
      } yield DatasetEvent(i, r, o, v, s, did, stage)
      e.fold(f => throw new RuntimeException(f.message), identity)
    }
  }

  def updateDatasets(
    user: User,
    qa:   DatasetQaState,
    dids: List[Dataset.Id]
  ): IO[Unit] = {
    val q = s"""
      mutation {
        updateDatasets(input: {
          SET: {
            qaState: ${qa.tag.toScreamingSnakeCase}
          },
          WHERE: {
            id: { IN: [ ${dids.map(_.show).mkString("\"", "\",\"", "\"")} ] }
          }
        }) {
          datasets {
            id
          }
        }
      }
    """

    query(user = user, query = q).void
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

  def moveObservationAs(user: User, oid: Observation.Id, gid: Option[Group.Id]): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              groupId: ${gid.asJson}
            }
            WHERE: {
              id: {
                EQ: ${oid.asJson}
              }
            }
          }) {
            observations {
              id
              groupId
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "updateObservations": {
              "observations": [
                {
                  "id": $oid,
                  "groupId": $gid
                }
              ]
            }
          }
        """
      )
    )

  def createUserInvitationAs(
    user: User, 
    pid: Program.Id, 
    role: ProgramUserRole = ProgramUserRole.Coi,
    supportType: Option[ProgramUserSupportType] = None,
    supportPartner: Option[Tag] = None
  ): IO[UserInvitation] =
    query(
      user = user,
      query = s"""
      mutation {
        createUserInvitation(
          input: {
            programId: "$pid"
            role: ${role.tag.toUpperCase}
            ${supportType.map(_.tag.toUpperCase).foldMap(s => s"supportType: $s")}
            ${supportPartner.map(_.value.toUpperCase).foldMap(s => s"supportPartner: $s")}
          }
        ) {
          key
        }
      }
      """
    ).map { js =>
      js.hcursor
        .downFields("createUserInvitation", "key")
        .require[UserInvitation]
    }

  def redeemUserInvitationAs(u: User, inv: UserInvitation, accept: Boolean = true): IO[UserInvitation.Id] =
    query(
      user = u,
      query = s"""
        mutation {
          redeemUserInvitation(input: { 
            key: "${UserInvitation.fromString.reverseGet(inv)}"
            accept: $accept
          }) {
            invitation {
              id
              status
              issuer {
                id
              }
              redeemer {
                id
              }
            }
          }
        }
      """     
    ).map { j =>
      j.hcursor.downFields("redeemUserInvitation", "invitation", "id").require[UserInvitation.Id]
    }

  def revokeUserInvitationAs(u: User, id: UserInvitation.Id): IO[UserInvitation.Id] =
    query(
      user = u,
      query = s"""
        mutation {
          revokeUserInvitation(input: { 
            id: "${UserInvitation.Id.fromString.reverseGet(id)}"
          }) {
            invitation {
              id
              status
              issuer {
                id
              }
              redeemer {
                id
              }
            }
          }
        }
      """     
    ).map { j =>
      j.hcursor.downFields("revokeUserInvitation", "invitation", "id").require[UserInvitation.Id]
    }

}
