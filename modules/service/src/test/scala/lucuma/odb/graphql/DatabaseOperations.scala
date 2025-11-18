// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.Ior
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.AtomStage
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.EmailStatus
import lucuma.core.enums.Gender
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SlewStage
import lucuma.core.enums.StepStage
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.CallForProposals
import lucuma.core.model.Client
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.ExecutionEvent.AtomEvent
import lucuma.core.model.ExecutionEvent.DatasetEvent
import lucuma.core.model.ExecutionEvent.SequenceEvent
import lucuma.core.model.ExecutionEvent.SlewEvent
import lucuma.core.model.ExecutionEvent.StepEvent
import lucuma.core.model.Group
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.ProgramNote
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProgramUser
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.ServiceUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.UserInvitation
import lucuma.core.model.UserProfile
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.syntax.string.*
import lucuma.core.util.CalculationState
import lucuma.core.util.DateInterval
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.FMain
import lucuma.odb.data.EmailId
import lucuma.odb.data.Existence
import lucuma.odb.data.Obscalc
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.input.AllocationInput
import lucuma.odb.graphql.input.TimeChargeCorrectionInput
import lucuma.odb.json.offset.transport.given
import lucuma.odb.json.stepconfig.given
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.EmailService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.syntax.instrument.*
import lucuma.odb.util.Codecs.*
import lucuma.refined.*
import natchez.Trace.Implicits.noop
import skunk.*
import skunk.codec.boolean.*
import skunk.syntax.all.*

import java.time.LocalDate
import java.time.format.DateTimeFormatter

trait DatabaseOperations { this: OdbSuite =>

  // Executes the obscalc update for an observation on demand.  In production,
  // this is handled by a background worker but for testing it is useful to
  // trigger an update on demand.
  def runObscalcUpdateAs(user: ServiceUser, pid: Program.Id, oid: Observation.Id): IO[Unit] =
    withServicesForObscalc(user): services =>
      TimeEstimateCalculatorImplementation
        .fromSession(services.session, services.enums)
        .flatMap: tec =>
          given Services[IO] = services
          requireServiceAccessOrThrow:
            val srv  = obscalcService(CommitHash.Zero, tec)
            val when =
              services.transactionally:
                srv
                  .selectOne(oid)
                  .map(_.map(_.meta.lastInvalidation).getOrElse(Timestamp.Min))

            when.flatMap: t =>
              srv.calculateAndUpdate(Obscalc.PendingCalc(pid, oid, t))
      .void

  def selectCalculationStates: IO[Map[Observation.Id, CalculationState]] =
    withSession: session =>
      val states: Query[Void, (Observation.Id, CalculationState)] = sql"""
        SELECT
          c_observation_id,
          c_obscalc_state
        FROM
          t_obscalc
      """.query(lucuma.odb.util.Codecs.observation_id *: lucuma.odb.util.Codecs.calculation_state)

      session.execute(states).map(_.toMap)

  def createCallForProposalsAs(
     user:        User,
     callType:    CallForProposalsType = CallForProposalsType.RegularSemester,
     semester:    Semester             = Semester.unsafeFromString("2025A"),
     activeStart: LocalDate            = LocalDate.parse("2025-02-01"),
     activeEnd:   LocalDate            = LocalDate.parse("2025-07-31"),
     deadline:    Option[Timestamp]    = Timestamp.Max.some,
     partners:    List[(Partner, Option[Timestamp])] = List((Partner.US, none)),
     other:       Option[String]       = None
  ): IO[CallForProposals.Id] =
    val deadlineStr = deadline.foldMap(ts => s"submissionDeadlineDefault: \"${ts.isoFormat}\"")
    val partnerList =
      partners.map((p, d) =>
        s"""
          {
            partner: ${p.tag.toScreamingSnakeCase}
            ${d.foldMap(ts => s"submissionDeadlineOverride: \"${ts.isoFormat}\"")}
          }
        """
      ).mkString("[", ", ", "]")
    query(user, s"""
      mutation {
        createCallForProposals(
          input: {
            SET: {
              type:        ${callType.tag.toScreamingSnakeCase}
              semester:    "${semester.format}"
              activeStart: "${activeStart.format(DateTimeFormatter.ISO_DATE)}"
              activeEnd:   "${activeEnd.format(DateTimeFormatter.ISO_DATE)}"
              $deadlineStr
              partners: $partnerList
              ${other.getOrElse("")}
            }
          }
        ) {
          callForProposals {
            id
          }
        }
      }
    """
    ).flatMap { js =>
      js.hcursor
        .downFields("createCallForProposals", "callForProposals", "id")
        .as[CallForProposals.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  def updateCallForProposalsAs(
    user: User,
    id: CallForProposals.Id,
    set: String
  ): IO[Unit] =
    query(user, s"""
      mutation {
        updateCallsForProposals(
          input: {
            WHERE: {
              id: { EQ: "$id" }
            }
            SET: $set
          }
        ) {
          callsForProposals {
            id
          }
        }
      }
    """).void

  def createProgramAs(user: User, name: String = null, clientOption: ClientOption = ClientOption.Default): IO[Program.Id] =
    query(user, s"mutation { createProgram(input: { SET: { name: ${Option(name).asJson} } }) { program { id } } }", client = clientOption).flatMap { js =>
      js.hcursor
        .downFields("createProgram", "program", "id")
        .as[Program.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  def createProgramWithPiAffiliation(
    pi: User,
    piPartnerLink: PartnerLink,
    programName: String = null
  ): IO[Program.Id] =
    for
      pid    <- createProgramAs(pi, programName)
      piPuid <- piProgramUserIdAs(pi, pid)
      _      <- updateProgramUserAs(pi, piPuid, piPartnerLink)
    yield pid

  def createProgramWithNonPartnerPi(pi: User, programName: String = null): IO[Program.Id] =
    createProgramWithPiAffiliation(pi, PartnerLink.HasNonPartner, programName)

  def createProgramWithUsPi(pi: User, programName: String = null): IO[Program.Id] =
    createProgramWithPiAffiliation(pi, PartnerLink.HasPartner(Partner.US), programName)

  def createProgramWithCaPi(pi: User, programName: String = null): IO[Program.Id] =
    createProgramWithPiAffiliation(pi, PartnerLink.HasPartner(Partner.CA), programName)

  def createProgramNoteAs(
    user:      User,
    pid:       Program.Id,
    title:     String,
    text:      Option[String]    = none,
    isPrivate: Option[Boolean]   = none,
    existence: Option[Existence] = none
  ): IO[ProgramNote.Id] =
    val props = List(
      s"""title: "$title"""".some,
      text.map(t => s"""text: "$t""""),
      isPrivate.map(b => s"isPrivate: $b"),
      existence.map(e => s"""existence: ${e.tag.toUpperCase}""")
    ).flatten.mkString("{\n", "\n", "}\n")

    query(
      user  = user,
      query = s"""
        mutation {
          createProgramNote(
            input: {
              programId: "$pid"
              SET: $props
            }
          ) {
            programNote {
              id
              title
              text
              isPrivate
              existence
            }
          }
        }
      """
    ).flatMap: json =>
      val c = json.hcursor.downFields("createProgramNote", "programNote")
      (
        for
          i <- c.downField("id").as[ProgramNote.Id]
          t <- c.downField("title").as[String]
          x <- c.downField("text").as[String]
          p <- c.downField("isPrivate").as[Boolean]
          e <- c.downField("existence").as[Existence]
        yield (i, (t, x, p, e))
      )
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
        .flatTap: (_, res) =>
          assertEquals(
            res,
            (title, text.orNull, isPrivate.getOrElse(false), existence.getOrElse(Existence.Present))
          ).pure
        .map(_._1)



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
      query {
        program(programId: "$pid") {
          proposal {
            reference { label }
          }
        }
      }
    """).flatMap { js =>
      js.hcursor
        .downFields("program", "proposal", "reference", "label")
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

  def fetchOid(user: User, obs: ObservationReference): IO[Observation.Id] =
    query(user, s"""
      query { observation(observationReference: "${obs.label}") { id } }
    """).flatMap { js =>
      js.hcursor
        .downFields("observation", "id")
        .as[Observation.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]
    }

  def setProgramReference(user: User, pid: Program.Id, set: String): IO[Option[ProgramReference]] =
    query(
      user,
      s"""
        mutation {
          setProgramReference(input: {
            programId: "$pid"
            SET: { $set }
          }) {
            reference { label }
          }
        }
      """
    ).flatMap {
      _.hcursor
       .downFields("setProgramReference", "reference", "label")
       .success
       .traverse(_.as[ProgramReference])
       .leftMap(f => new RuntimeException(f.message))
       .liftTo[IO]
    }

  def addQueueProposal(user: User, pid: Program.Id, cid: CallForProposals.Id): IO[Unit] =
    addProposal(
      user,
      pid,
      cid.some,
      s"""
        queue: {
          toOActivation: NONE
          minPercentTime: 0
        }
      """.some
    )

  def addDemoScienceProposal(user: User, pid: Program.Id, cid: CallForProposals.Id): IO[Unit] =
    addProposal(
      user,
      pid,
      cid.some,
      s"""
        demoScience: {
          toOActivation: NONE
          minPercentTime: 0
        }
      """.some
    )

  def addProposal(
    user: User,
    pid: Program.Id,
    callId: Option[CallForProposals.Id] = None,
    callProps: Option[String] = None
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          createProposal(
            input: {
              programId: "$pid"
              SET: {
                category: GALACTIC_OTHER
                ${callId.fold("")(c => s"callId: \"$c\"")}
                ${callProps.fold("") { c =>
                  s"""
                    type: {
                      $c
                    }
                  """
                }}
              }
            }
          ) {
            proposal {
              category
            }
          }
        }
      """,
      expected =
        Json.obj(
          "createProposal" -> Json.obj(
            "proposal" -> Json.obj(
              "category" -> "GALACTIC_OTHER".asJson
            )
          )
        ).asRight
    )

  def setCallId(user: User, pid: Program.Id, cid: CallForProposals.Id): IO[Unit] =
    query(
      user,
      s"""
        mutation {
          updateProposal(
            input: {
              programId: "$pid",
              SET: {
                callId: "$cid"
              }
            }
          ) {
            proposal { category }
          }
        }
      """
    ).void

  def getProprietaryMonths(
    user: User,
    pid:  Program.Id
  ): IO[Option[NonNegInt]] =
    query(user, s"""
      query {
        program(programId: "$pid") {
          goa { proprietaryMonths }
        }
      }
    """).flatMap: js =>
      js.hcursor
        .downFields("program", "goa", "proprietaryMonths")
        .success
        .traverse(_.as[Int].map(NonNegInt.unsafeFrom))
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  def getActivePeriod(
    user: User,
    pid:  Program.Id
  ): IO[DateInterval] =
    query(
      user  = user,
      query = s"""
        query {
          program(programId: "$pid") {
            active {
              start
              end
            }
          }
        }
      """
    ).flatMap: js =>
      val interval = js.hcursor.downFields("program", "active")
      (for
        s <- interval.downField("start").as[LocalDate]
        e <- interval.downField("end").as[LocalDate]
      yield DateInterval.between(s, e)).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

  def addPartnerSplits(
    user: User,
    pid: Program.Id,
    proposalType: String = "queue",
    partnerSplits: List[(Partner, Int)] = List((Partner.US, 70), (Partner.CA, 30))
  ): IO[Unit] =
    val splitsList = partnerSplits.map((p, v) =>
      s"""
        {
          partner: ${p.tag.toScreamingSnakeCase}
          percent: $v
        }
      """
    ).mkString("[", ",", "]")
    query(user, s"""
      mutation {
        updateProposal(
          input: {
            programId: "$pid"
            SET: {
              type: {
                $proposalType: {
                  partnerSplits: $splitsList
                }
              }
            }
          }
        ) {
          proposal { category }
        }
      }
    """).void

  def setProposalStatus(user: User, pid: Program.Id, status: String): IO[(Option[ProgramReference], Option[ProposalReference])] =
    queryIor(user,  s"""
        mutation {
          setProposalStatus(
            input: {
              programId: "$pid"
              status: $status
            }
          ) {
            program {
              reference { label }
              proposal { reference { label } }
            }
          }
        }
      """
    ).flatMap {  ior =>

      def handle(js: Json) =
        val programCursor = js.hcursor.downFields("setProposalStatus", "program")
        (for {
          prog <- programCursor.downFields("reference", "label").success.traverse(_.as[ProgramReference])
          prop <- programCursor.downFields("proposal", "reference", "label").success.traverse(_.as[ProposalReference])
        } yield (prog, prop)).leftMap(f => new RuntimeException(f.message)).liftTo[IO]

      ior match
        case Ior.Left(a) => IO.raiseError(new RuntimeException(a.toList.mkString))
        case Ior.Right(b) => handle(b)
        case Ior.Both(a, b) => handle(b)

    }

  def submitProposal(user: User, pid: Program.Id): IO[ProposalReference] =
    setProposalStatus(user, pid, "SUBMITTED").map(_._2.get) // should have a proposal reference now.

  def unsubmitProposal(user: User, pid: Program.Id): IO[Option[ProposalReference]] =
    setProposalStatus(user, pid, "NOT_SUBMITTED").map(_._2)

  def acceptProposal(user: User, pid: Program.Id): IO[Option[ProgramReference]] =
    setProposalStatus(user, pid, "ACCEPTED").map(_._1)

  def deleteProposal(user: User, pid: Program.Id): IO[Boolean] =
    query(user, s"""
      mutation {
        deleteProposal(
          input: {
            programId: "$pid"
          }
        ) {
          result
        }
      }
    """).flatMap { js =>
      js.hcursor.downFields("deleteProposal", "result").as[Boolean].leftMap(f => new RuntimeException(f.message)).liftTo[IO]
    }

  def createObservationAs(user: User, pid: Program.Id, tids: Target.Id*): IO[Observation.Id] =
    createObservationAs(user, pid, None, tids*)

  def createGmosNorthImagingObservationAs(user: User, pid: Program.Id, tids: Target.Id*): IO[Observation.Id] =
    createGmosNorthImagingObservationAs(user, pid, ImageQuality.Preset.PointEight, None, tids*)

  def createGmosNorthImagingObservationAs(
    user:    User,
    pid:     Program.Id,
    iq:      ImageQuality.Preset = ImageQuality.Preset.PointEight,
    offsets: Option[String] = None,
    tids:    Target.Id*
  ): IO[Observation.Id] =
    createObservationWithSpatialOffsets(user, pid, ObservingModeType.GmosNorthImaging, iq, offsets, tids*)

  def createGmosSouthImagingObservationAs(user: User, pid: Program.Id, tids: Target.Id*): IO[Observation.Id] =
    createGmosSouthImagingObservationAs(user, pid, ImageQuality.Preset.PointEight, None, tids*)

  def createGmosSouthImagingObservationAs(
    user:    User,
    pid:     Program.Id,
    iq:      ImageQuality.Preset = ImageQuality.Preset.PointEight,
    offsets: Option[String] = None,
    tids:    Target.Id*
  ): IO[Observation.Id] =
    createObservationWithSpatialOffsets(user, pid, ObservingModeType.GmosSouthImaging, iq, offsets, tids*)

  def createFlamingos2LongSlitObservationAs(user: User, pid: Program.Id, tids: Target.Id*): IO[Observation.Id] =
    createFlamingos2LongSlitObservationAs(user, pid, ImageQuality.Preset.PointEight, None, tids*)

  def createFlamingos2LongSlitObservationAs(
    user:    User,
    pid:     Program.Id,
    iq:      ImageQuality.Preset = ImageQuality.Preset.PointEight,
    offsets: Option[String] = None,
    tids:    Target.Id*
    ): IO[Observation.Id] =
    createObservationWithSpatialOffsets(user, pid, ObservingModeType.Flamingos2LongSlit, iq, offsets, tids*)

  private def createObservationWithSpatialOffsets(
    user:          User,
    pid:           Program.Id,
    observingMode: ObservingModeType,
    iq:            ImageQuality.Preset,
    offsets:       Option[String],
    tids:          Target.Id*
  ): IO[Observation.Id] =
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
                scienceRequirements: ${scienceRequirementsObject(observingMode)}
                observingMode: ${observingModeWithSpatialOffsets(observingMode, offsets)}
                constraintSet: {
                  imageQuality: ${iq.tag.toScreamingSnakeCase}
                }
              }
            }) {
              observation {
                id
              }
            }
          }
        """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  def observationsWhere(user: User, where: String): IO[List[Observation.Id]] =
    query(
      user,
      s"""
         query {
          observations(WHERE: { $where }) {
            matches {
              id
            }
          }
        }
      """
    ).flatMap { json =>
      json.hcursor.downFields("observations", "matches").values.toList.flatten.traverse { json =>
        json.hcursor.downField("id").as[Observation.Id]
      }
      .leftMap(f => new RuntimeException(f.message))
      .liftTo[IO]
    }

  def setObservationExistence(
    user: User,
    oid:  Observation.Id,
    existence: Existence
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(
            input: {
              WHERE: {
                id: {
                  EQ: ${oid.asJson}
                }
              }
              SET: {
                existence: ${existence.tag.toUpperCase}
              }
              includeDeleted: true
            }
          ) {
            observations {
              id
              existence
            }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": {
            "observations": [
              { "id": $oid, "existence": ${existence.tag.toUpperCase} }
            ]
          }
        }
      """.asRight
    )

  def deleteObservation(
    user: User,
    oid:  Observation.Id
  ): IO[Unit] =
    setObservationExistence(user, oid, Existence.Deleted)

  def restoreObservation(
    user: User,
    oid:  Observation.Id
  ): IO[Unit] =
    setObservationExistence(user, oid, Existence.Present)

  def setScienceBandAs(user: User, oid: Observation.Id, band: Option[ScienceBand]): IO[Unit] =
    query(
      user,
      s"""
        mutation {
          updateObservations(
            input: {
              SET: {
                scienceBand: ${band.map(_.tag.toScreamingSnakeCase).getOrElse("null")}
              }
              WHERE: {
                id: { EQ: "$oid" }
              }
            }
          ) {
            observations {
              id
            }
          }
        }
      """
    ).void

  private def scienceRequirementsObject(observingMode: ObservingModeType): String =
    observingMode match
      case ObservingModeType.Flamingos2LongSlit =>
        """{
        exposureTimeMode: {
          signalToNoise: {
            value: 100.0
            at: { nanometers: 1210 }
          }
        }
        spectroscopy: {
          wavelength: { nanometers: 1200 }
          resolution: 100
          wavelengthCoverage: { nanometers: 20 }
          focalPlane: SINGLE_SLIT
          focalPlaneAngle: { microarcseconds: 0 }
        }
      }"""
      case ObservingModeType.GmosNorthImaging =>
        """{
          exposureTimeMode: {
            signalToNoise: {
              value: 100.0
              at: { nanometers: 1210 }
            }
          }
          imaging: {
            minimumFov: {
              arcseconds: 100
            },
            narrowFilters: false
            broadFilters: false
            combinedFilters: true
          }
        }"""
      case ObservingModeType.GmosSouthImaging =>
        """{
          exposureTimeMode: {
            signalToNoise: {
              value: 100.0
              at: { nanometers: 1210 }
            }
          }
          imaging: {
            minimumFov: {
              arcseconds: 100
            },
            narrowFilters: false
            broadFilters: false
            combinedFilters: true
          }
        }"""
      case ObservingModeType.GmosNorthLongSlit |
           ObservingModeType.GmosSouthLongSlit =>
        """{
            exposureTimeMode: {
              signalToNoise: {
                value: 100.0
                at: { nanometers: 510 }
              }
            }
            spectroscopy: {
              wavelength: { nanometers: 500 }
              resolution: 100
              wavelengthCoverage: { nanometers: 20 }
              focalPlane: SINGLE_SLIT
              focalPlaneAngle: { microarcseconds: 0 }
            }
          }"""

  private def observingModeObject(observingMode: ObservingModeType): String =
    observingMode match
      case ObservingModeType.Flamingos2LongSlit =>
        """{
          flamingos2LongSlit: {
            disperser: R1200_HK
            filter: Y
            fpu: LONG_SLIT_2
          }
        }"""
      case ObservingModeType.GmosNorthImaging =>
        """{
          gmosNorthImaging: {
            filters: [
              { filter: R_PRIME },
              { filter: G_PRIME }
            ]
          }
        }"""
      case ObservingModeType.GmosSouthImaging =>
        """{
          gmosSouthImaging: {
            filters: [
              { filter: R_PRIME },
              { filter: G_PRIME }
            ]
          }
        }"""
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

  private def observingModeWithSpatialOffsets(observingMode: ObservingModeType, offsets: Option[String]): String =
    observingMode match
      case ObservingModeType.GmosNorthImaging =>
        val offsetsField = offsets.fold("")(offsets => s", offsets: $offsets")
        s"""{
          gmosNorthImaging: {
            filters: [
              {
                filter: R_PRIME
              },
              {
                filter: G_PRIME
              }
            ]
            $offsetsField
          }
        }"""
      case ObservingModeType.GmosSouthImaging =>
        val offsetsField = offsets.fold("")(offsets => s", offsets: $offsets")
        s"""{
          gmosSouthImaging: {
            filters: [
              {
                filter: R_PRIME
              },
              {
                filter: G_PRIME
              }
            ]
            $offsetsField
          }
        }"""
      case ObservingModeType.Flamingos2LongSlit =>
        val offsetsField = offsets.fold("")(offsets => s", explicitOffsets: $offsets")
        s"""{
          flamingos2LongSlit: {
            disperser: R1200_HK
            filter: Y
            fpu: LONG_SLIT_2
            $offsetsField
          }
        }"""
      case _ => ""

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

  def createObservationWithBlindOffsetAs(user: User, pid: Program.Id, blindOffsetName: String, tids: Target.Id*): IO[(Observation.Id, Target.Id)] =
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
                  blindOffsetTarget: {
                    name: ${blindOffsetName.asJson}
                    sidereal: {
                      ra: { degrees: "12.345" }
                      dec: { degrees: "45.678" }
                      epoch: "J2000.000"
                    }
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
                  }
                }
              }
            }) {
              observation {
                id
                targetEnvironment {
                  blindOffsetTarget {
                    id
                  }
                }
              }
            }
          }
        """
    ).map { json =>
      val obs = json.hcursor.downFields("createObservation", "observation")
      val obsId = obs.downField("id").require[Observation.Id]
      val targetId = obs.downField("targetEnvironment").downField("blindOffsetTarget").downField("id").require[Target.Id]
      (obsId, targetId)

    }

  def resetAcquisitionAs(user: User, oid: Observation.Id): IO[Unit] =
    query(
      user  = user,
      query = s"""
        mutation {
          resetAcquisition(input: {
            observationId: "$oid"
          }) {
            observation { id }
          }
        }
      """
    ).void

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

  private val DefaultSourceProfile: String =
    """
      sourceProfile: {
        point: {
          bandNormalized: {
            sed: {
              stellarLibrary: B5_III
            }
            brightnesses: [
              {
                band: R
                value: 15.0
                units: VEGA_MAGNITUDE
              }
            ]
          }
        }
      }
    """

  def createAllTargetTypesAs(
    user: User,
    pid:  Program.Id,
    sourceProfile: String = DefaultSourceProfile
  ): IO[List[Target.Id]] =
    (createSiderealTargetAs(user, pid, sourceProfile = sourceProfile),
     createNonsiderealTargetAs(user, pid, sourceProfile = sourceProfile),
     createOpportunityTargetAs(user, pid, sourceProfile = sourceProfile)
    ).mapN(List(_, _, _))

  def createTargetAs(
    user: User,
    pid:  Program.Id,
    name: String = "No Name",
    sourceProfile: String = DefaultSourceProfile
  ): IO[Target.Id] =
    createSiderealTargetAs(user, pid, name, sourceProfile)

  def createSiderealTargetAs(
    user: User,
    pid:  Program.Id,
    name: String = "No Name",
    sourceProfile: String = DefaultSourceProfile
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
                  radialVelocity: {
                    kilometersPerSecond: 0.0
                  }
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

  def createOpportunityTargetAs(
    user: User,
    pid:  Program.Id,
    name: String = "No Name",
    sourceProfile: String = DefaultSourceProfile
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
                opportunity: {
                  region: {
                    rightAscensionArc: { type: FULL }
                    declinationArc: {
                      type: PARTIAL
                      start: { degrees: 10 }
                      end: { degrees: 70 }
                    }
                  }
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

  def createNonsiderealTargetAs(
    user: User,
    pid:  Program.Id,
    name: String = "No Name",
    sourceProfile: String = DefaultSourceProfile
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
                nonsidereal: {
                  keyType: COMET
                  des: "1P"
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

  def createIncompleteTargetAs(user: User, pid: Program.Id, name: String = "No Name"): IO[Target.Id] =
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
              }
            }
          ) {
            target { id }
          }
        }
      """
    ).flatMap: js =>
      js.hcursor
        .downField("createTarget")
        .downField("target")
        .downField("id")
        .as[Target.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  def readAllocations(
    topLevelField: String,
    json:          Json
  ): IO[Set[(TimeAccountingCategory, ScienceBand, BigDecimal)]] =
    json.hcursor.downFields(topLevelField, "allocations").values.toList.flatten.traverse { obj =>
      val c = obj.hcursor
      (for {
        p <- c.downField("category").as[TimeAccountingCategory]
        b <- c.downField("scienceBand").as[ScienceBand]
        d <- c.downFields("duration", "hours").as[BigDecimal]
      } yield (p, b, d))
       .leftMap(f => new RuntimeException(f.getMessage))
       .liftTo[IO]
    }.map(_.toSet)

  def setAllocationsAs(
    user:        User,
    pid:         Program.Id,
    allocations: List[AllocationInput]
  ): IO[Json] =
    query(
      user = user,
      query = s"""
        mutation {
          setAllocations(input: {
            programId:   ${pid.asJson}
            allocations: ${allocations.map { a =>
              s"""
                {
                  category: ${a.category.tag.toScreamingSnakeCase}
                  scienceBand: ${a.scienceBand.tag.toScreamingSnakeCase}
                  duration: {
                    hours: "${a.duration.toHours}"
                  }
                }
              """
            }.mkString("[\n", ",\n", "]")}
          }) {
            allocations {
              category
              scienceBand
              duration { hours }
            }
          }
        }
      """
    )

  def setOneAllocationAs(
    user: User,
    pid: Program.Id,
    category: TimeAccountingCategory,
    scienceBand: ScienceBand,
    duration: TimeSpan,
  ): IO[Json] =
    setAllocationsAs(user, pid, List(AllocationInput(category, scienceBand, duration)))

  def linkUserAs(
    user: User,
    mid: ProgramUser.Id,
    uid:  User.Id
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          linkUser(input: {
            programUserId: ${mid.asJson}
            userId: ${uid.asJson}
          }) {
            user { id }
          }
        }
      """,
      expected = json"""
        {
          "linkUser" : {
            "user": { "id": $mid }
          }
        }
      """.asRight
    )

  def createUsers(users: User*): IO[Unit] =
    users.toList.traverse_(createProgramAs(_)) // TODO: something cheaper

  def piProgramUserIdAs(
    user: User,
    pid:  Program.Id
  ): IO[ProgramUser.Id] =
    query(
      user,
      s"""
        query {
          program(programId: "$pid") {
            pi { id }
          }
        }
      """
    ).map: js =>
      js.hcursor
        .downFields("program", "pi", "id").require[ProgramUser.Id]

  def updateAsterisms(
    user: User,
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

  def expectTargetExistenceAs(user: User, tid: Target.Id, shouldBePresent: Boolean): IO[Unit] =
    val existence = if (shouldBePresent) Existence.Present else Existence.Deleted
    expect(
      user = user,
      query = s"""
        query {
          target(targetId: "$tid") {
            existence
          }
        }
      """,
      expected = json"""
        {
          "target": {
            "existence": ${existence.tag.toUpperCase().asJson}
          }
        }
      """.asRight
    )

  def expectTargetNotFoundAs(user: User, tid: Target.Id): IO[Unit] =
    expect(
      user = user,
      query = s"""
        query {
          target(targetId: "$tid") {
            id
          }
        }
      """,
      expected = json"""
        {
          "target": null
        }
      """.asRight
    )

  def createGroupAs(
    user: User,
    pid: Program.Id,
    parentGroupId: Option[Group.Id] = None,
    parentIndex: Option[NonNegShort] = None,
    minRequired: Option[NonNegShort] = None,
    minimumInterval: Option[TimeSpan] = None,
    maximumInterval: Option[TimeSpan] = None,
    initialContents: Option[List[Either[Group.Id, Observation.Id]]] = None,
    name: Option[String] = None
  ): IO[Group.Id] =
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
                ${minimumInterval.foldMap(ts => s"minimumInterval: { microseconds: \"${ts.toMicroseconds}\" }")}
                ${maximumInterval.foldMap(ts => s"maximumInterval: { microseconds: \"${ts.toMicroseconds}\" }")}
                ${name.foldMap(n => s"name: ${n.asJson.spaces2}")}
              }
              ${
                initialContents.foldMap: es =>
                  es.map {
                    case Left(gid)  => s"{ groupId: ${gid.asJson.noSpaces} }\n"
                    case Right(oid) => s"{ observationId: ${oid.asJson.noSpaces} }\n"
                  }.mkString("initialContents: [", " ", "]")
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

  def groupElementsAs(user: User, pid: Program.Id, gid: Option[Group.Id], includeDeleted: Boolean = false): IO[List[Either[Group.Id, Observation.Id]]] =
    query(user, s"""query { program(programId: "$pid") { allGroupElements(includeDeleted: $includeDeleted) { existence parentGroupId group { id } observation { id } } } }""")
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

  protected def dynamicConfigGmos(instrument: Instrument): String =
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

  protected def dynamicConfigFlamingos2(instrument: Instrument): String =
    s"""
      ${instrument.fieldName}: {
        exposure: {
          seconds: 1200,
        },
        disperser: R1200_JH,
        filter: Y,
        readMode: MEDIUM,
        lyotWheel: F16,
        fpu: {
          builtin: LONG_SLIT_1
        },
        decker: LONG_SLIT,
        readoutMode: SCIENCE,
        reads: READS_4
      }
    """

  protected def dynamicConfig(instrument: Instrument): String =
    instrument match
      case Instrument.Flamingos2 => dynamicConfigFlamingos2(instrument)
      case Instrument.GmosNorth  => dynamicConfigGmos(instrument)
      case Instrument.GmosSouth  => dynamicConfigGmos(instrument)
      case _                     => "Unexpected instrument"

  val stepConfigScienceInput: String =
    """
      stepConfig: {
        science: true
      }
    """

  val telescopeConfigInput: String =
    """
      telescopeConfig: {
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

    query(user = user, query = q).map: json =>
      val c = json.hcursor.downFields("addSequenceEvent", "event")
      val e = for
        i <- c.downField("id").as[ExecutionEvent.Id]
        r <- c.downField("received").as[Timestamp]
        o <- c.downFields("observation", "id").as[Observation.Id]
        n <- c.downField("idempotencyKey").as[Option[IdempotencyKey]]
      yield SequenceEvent(i, r, o, vid, n, cmd)
      e.fold(f => throw new RuntimeException(f.message), identity)
  }

  def addSlewEventAs(
    user: User,
    oid:  Observation.Id,
    stg:  SlewStage
  ): IO[SlewEvent] = {
    val q = s"""
      mutation {
        addSlewEvent(input: {
          observationId: "$oid",
          slewStage: ${stg.tag.toUpperCase}
        }) {
          event {
            id
            received
            visit { id }
          }
        }
      }
    """

    query(user = user, query = q).map: json =>
      val c = json.hcursor.downFields("addSlewEvent", "event")
      val e = for
        i <- c.downField("id").as[ExecutionEvent.Id]
        r <- c.downField("received").as[Timestamp]
        v <- c.downFields("visit", "id").as[Visit.Id]
        n <- c.downField("idempotencyKey").as[Option[IdempotencyKey]]
      yield SlewEvent(i, r, oid, v, n, stg)
      e.fold(f => throw new RuntimeException(f.message), identity)
  }

  def recordAtomAs(user: User, instrument: Instrument, vid: Visit.Id, sequenceType: SequenceType = SequenceType.Science): IO[Atom.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            recordAtom(input: {
              visitId: ${vid.asJson},
              instrument: ${instrument.tag.toScreamingSnakeCase},
              sequenceType: ${sequenceType.tag.toScreamingSnakeCase}
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
    recordStepAs(user, aid, instrument, dynamicConfig(instrument), stepConfigScienceInput, telescopeConfigInput)

  def recordStepAs(
    user:                 User,
    aid:                  Atom.Id,
    instrument:           Instrument,
    instrumentInput:      String,
    stepConfigInput:      String,
    telescopeConfigInput: String
  ): IO[Step.Id] = {

    val name = s"record${instrument.tag}Step"

    val q = s"""
      mutation {
        $name(input: {
          atomId: ${aid.asJson},
          $instrumentInput,
          $stepConfigInput,
          $telescopeConfigInput,
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
    telescopeConfig: TelescopeConfig,
    observeClass:    ObserveClass = ObserveClass.Science
  ): IO[Step.Id] = {

    val name = s"record${instrument.tag}Step"

    def step = stepConfig.asJson.mapObject(_.remove("stepType"))

    // HACK: to make it easy to write test cases we take the instrument dynamic
    // config scala object and turn it into JSON, relying on the fact that the
    // input is explicitly structured to be equivalent to the output.  There's
    // just one problem, the `centralWavelength` is a computed value that
    // appears only in the output.  So, we prune `centralWavelength` from the
    // JSON here.
    val instJson = instrumentInput
                     .asJson
                     .hcursor
                     .downField("centralWavelength")
                     .delete
                     .top
                     .fold(instrumentInput.asJson)(_.asJson)

    val vars = Json.obj(
      "input" -> Json.obj(
        "atomId" -> aid.asJson,
        instrument.fieldName -> instJson,
        "stepConfig" -> (stepConfig match {
          case StepConfig.Bias          => Json.obj("bias"      -> true.asJson)
          case StepConfig.Dark          => Json.obj("dark"      -> true.asJson)
          case StepConfig.Gcal(_,_,_,_) => Json.obj("gcal"      -> step)
          case StepConfig.Science       => Json.obj("science"   -> true.asJson)
          case StepConfig.SmartGcal(_)  => Json.obj("smartGcal" -> step)
        }),
        "telescopeConfig" -> telescopeConfig.asJson,
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
            atom { id }
          }
        }
      }
    """

    query(user = user, query = q).map: json =>
      val c = json.hcursor.downFields("addStepEvent", "event")
      val e = for
        i <- c.downField("id").as[ExecutionEvent.Id]
        r <- c.downField("received").as[Timestamp]
        o <- c.downFields("observation", "id").as[Observation.Id]
        v <- c.downFields("visit", "id").as[Visit.Id]
        n <- c.downField("idempotencyKey").as[Option[IdempotencyKey]]
        a <- c.downFields("atom", "id").as[Atom.Id]
      yield StepEvent(i, r, o, v, n, a, sid, stage)
      e.fold(f => throw new RuntimeException(f.message), identity)
  }

  def addAtomEventAs(
    user:           User,
    aid:            Atom.Id,
    stage:          AtomStage,
    idempotencyKey: Option[IdempotencyKey] = None,
    clientId:       Option[Client.Id]      = None
  ): IO[AtomEvent] =
    val q = s"""
      mutation {
        addAtomEvent(input: {
          atomId:    "$aid",
          atomStage: ${stage.tag.toScreamingSnakeCase}
          ${idempotencyKey.fold("")(idm => s"idempotencyKey: \"$idm\"")}
          ${clientId.fold("")(cid => s"clientId: \"$cid\"")}
        }) {
          event {
            id
            received
            observation { id }
            visit { id }
            idempotencyKey
            clientId
          }
        }
      }
    """
    query(user = user, query = q).map: json =>
      val c = json.hcursor.downFields("addAtomEvent", "event")
      val e = for
        i <- c.downField("id").as[ExecutionEvent.Id]
        r <- c.downField("received").as[Timestamp]
        o <- c.downFields("observation", "id").as[Observation.Id]
        v <- c.downFields("visit", "id").as[Visit.Id]
        n <- c.downField("idempotencyKey").as[Option[IdempotencyKey]]
        x <- c.downField("clientId").as[Option[Client.Id]]
      yield
        assertEquals(n, x.map(c => IdempotencyKey(c.toUuid)))
        AtomEvent(i, r, o, v, n, aid, stage)
      e.fold(f => throw new RuntimeException(f.message), identity)

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
            atom { id }
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
        n <- c.downField("idempotencyKey").as[Option[IdempotencyKey]]
        a <- c.downFields("atom", "id").as[Atom.Id]
        s <- c.downFields("step", "id").as[Step.Id]
      } yield DatasetEvent(i, r, o, v, n, a, s, did, stage)
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

  def getCalibrationRoleFromDb(tid: Target.Id): IO[Option[CalibrationRole]] = {
    val query = sql"select c_calibration_role from t_target where c_target_id = $target_id".query(calibration_role.opt)
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(query).use(_.unique(tid)))
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

  def moveGroupAs(user: User, groupToMove: Group.Id, newParent: Option[Group.Id]): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateGroups(input: {
            SET: {
              parentGroup: ${newParent.asJson}
            }
            WHERE: {
              id: {
                EQ: ${groupToMove.asJson}
              }
            }
          }) {
            groups {
              id
              parentId
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "updateGroups": {
              "groups": [
                {
                  "id": $groupToMove,
                  "parentId": $newParent
                }
              ]
            }
          }
        """
      )
    )

  def addProgramUserAs(
    user:        User,
    pid:         Program.Id,
    role:        ProgramUserRole   = ProgramUserRole.Coi,
    partnerLink: PartnerLink       = PartnerLink.HasPartner(Partner.US),
    preferred:   UserProfile       = UserProfile.Empty,
    education:   EducationalStatus = EducationalStatus.PhD,
    thesis:      Boolean           = false,
    gender:      Gender            = Gender.NotSpecified
  ): IO[ProgramUser.Id] =
    extension (o: Option[String]) def strOrNull: String = o.fold("null")(s => s""""$s"""")

    query(
      user  = user,
      query = s"""
        mutation {
          addProgramUser(
            input: {
              programId: "$pid"
              role: ${role.tag.toScreamingSnakeCase}
              SET: {
                partnerLink: {
                  ${
                    partnerLink match
                      case PartnerLink.HasPartner(partner)   => s"partner: ${partner.tag.toScreamingSnakeCase}"
                      case _                                 => s"linkType: ${partnerLink.linkType.tag.toScreamingSnakeCase}"
                  }
                }
                preferredProfile: {
                  givenName: ${preferred.givenName.strOrNull}
                  familyName: ${preferred.familyName.strOrNull}
                  creditName: ${preferred.creditName.strOrNull}
                  email: ${preferred.email.strOrNull}
                }
                educationalStatus: ${education.tag.toScreamingSnakeCase}
                thesis: $thesis
                gender: ${gender.tag.toScreamingSnakeCase}
              }
            }
          ) { programUser { id } }
        }
        """
    ).map: j =>
      j.hcursor.downFields("addProgramUser", "programUser", "id").require[ProgramUser.Id]

  val defaultPiEmail: NonEmptyString = "pi@someprestigiousplace.com".refined

  def updateProgramUserAs(
    user:       User,
    puid:       ProgramUser.Id,
    partnerLink: PartnerLink,
    email: Option[NonEmptyString] = defaultPiEmail.some
  ): IO[Unit] =
    val preferred = email.foldMap(e =>
      s"""
        preferredProfile: {
          email: "$e"
        }
      """
    )
    query(
      user = user,
      query = s"""
        mutation {
          updateProgramUsers(input: {
            WHERE: {
              id: {
                EQ: "$puid"
              }
            }
            SET: {
              partnerLink: {
                ${
                  partnerLink match
                    case PartnerLink.HasPartner(partner)   => s"partner: ${partner.tag.toScreamingSnakeCase}"
                    case _                                 => s"linkType: ${partnerLink.linkType.tag.toScreamingSnakeCase}"
                }
              }
              $preferred
            }
          }) {
            programUsers { id }
          }
        }
      """
    ).void

  def addCoisAs(u: User, pid: Program.Id, ps: List[Partner] = List(Partner.CA, Partner.US)): IO[Unit] =
    ps.traverse_ : p =>
      addProgramUserAs(u, pid, partnerLink = PartnerLink.HasPartner(p))

  def deleteProgramUserAs(
    user: User,
    mid:  ProgramUser.Id
  ): IO[Boolean] =
    query(
      user = user,
      query = s"""
        mutation {
          deleteProgramUser(input: {
            programUserId: "$mid"
          }) {
            result
          }
        }
      """
    ).map: js =>
      js.hcursor.downFields("deleteProgramUser", "result").require[Boolean]

  def listProgramUsersAs(
    user: User,
    pid:  Program.Id
  ): IO[List[(ProgramUser.Id, ProgramUserRole, Option[User.Id])]] =
    query(
      user  = user,
      query = s"""
        query {
          program(programId: "$pid") {
            users {
              id
              role
              user { id }
            }
          }
        }
      """
    ).map: js =>

      def userId(c: io.circe.HCursor): io.circe.Decoder.Result[Option[User.Id]] =
        c.downField("user").focus match
          case Some(v) if v.isNull => Right(None)
          case _                   => c.downFields("user", "id").as[Option[User.Id]]

      val cs = js.hcursor.downFields("program", "users").values.get.toList.map(_.hcursor)
      cs.map: c =>
        (for
          i <- c.get[ProgramUser.Id]("id")
          r <- c.get[ProgramUserRole]("role")
          u <- userId(c)
        yield (i, r, u)).fold(f => sys.error(f.message), identity)

  def createUserInvitationAs(
    user: User,
    mid:  ProgramUser.Id,
    recipientEmail: EmailAddress = EmailAddress.From.getOption("bob@dobbs.com").get,
  ): IO[UserInvitation] =
    query(
      user = user,
      query = s"""
      mutation {
        createUserInvitation(
          input: {
            programUserId: "$mid"
            recipientEmail: "$recipientEmail"
          }
        ) { key }
      }
      """
    ).map: js =>
      js.hcursor.downFields("createUserInvitation", "key").require[UserInvitation]

  def redeemUserInvitationAs(u: User, inv: UserInvitation, accept: Boolean = true): IO[UserInvitation.Id] =
    query(
      user = u,
      query = s"""
        mutation {
          redeemUserInvitation(input: {
            key: "${UserInvitation.fromString.reverseGet(inv)}"
            accept: $accept
          }) {
            invitation { id }
          }
        }
      """
    ).map: j =>
      j.hcursor.downFields("redeemUserInvitation", "invitation", "id").require[UserInvitation.Id]

  def revokeUserInvitationAs(u: User, id: UserInvitation.Id): IO[UserInvitation.Id] =
    query(
      user = u,
      query = s"""
        mutation {
          revokeUserInvitation(input: { id: "${UserInvitation.Id.fromString.reverseGet(id)}" }) {
            invitation { id }
          }
        }
      """
    ).map: j =>
      j.hcursor.downFields("revokeUserInvitation", "invitation", "id").require[UserInvitation.Id]

  def getEmailIdForInvitation(id: UserInvitation.Id): IO[Option[EmailId]] =
    val query = sql"select c_email_id from t_invitation where c_invitation_id = $user_invitation_id".query(email_id)
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(query).use(_.option(id)))

  // email will be inserted with a status of Queued
  def insertEmail(
    emailId: EmailId,
    programId: Program.Id,
    from: EmailAddress = EmailAddress.unsafeFrom("me@us.com"),
    to: EmailAddress = EmailAddress.unsafeFrom("bob@dobbs.com"),
    subject: NonEmptyString = "A subject of interest".refined,
    textMessage: NonEmptyString = "And the message is...".refined,
    htmlMessage: Option[NonEmptyString] = None,
  ): IO[Unit] = {
    val af = EmailService.Statements.insertEmail(emailId, programId, from, to, subject, textMessage, htmlMessage)
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(af.fragment.command).use(_.execute(af.argument).void))
  }

  def getEmailStatus(id: EmailId): IO[EmailStatus] = {
    val query = sql"select c_status from t_email where c_email_id = $email_id".query(email_status)
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(query).use(_.unique(id)))
  }

  def getEmailStatusesByAddress(address: NonEmptyString): IO[List[EmailStatus]] = {
    val query = sql"select c_status from t_email where c_recipient_email = $text_nonempty".query(email_status)
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(query).use(_.stream(address, chunkSize = 1024).compile.toList))
  }

  def ensureNoEmailsForAddress(address: NonEmptyString): IO[Unit] =
    getEmailStatusesByAddress(address).flatMap: es =>
      es match
        case Nil => IO.unit
        case _   => IO.raiseError(new Exception(s"Emails found for address $address"))

  def ensureSomeQueuedEmailsForAddress(address: NonEmptyString, count: Int): IO[Unit] =
    getEmailStatusesByAddress(address).flatMap: es =>
      val queued = es.count(_ == EmailStatus.Queued)
      if queued =!= count then
        IO.raiseError(new Exception(s"Expected $count queued emails for address $address, found $queued"))
      else IO.unit

  def updateEmailStatus(id: EmailId, status: EmailStatus): IO[Unit] = {
    val command = sql"update t_email set c_status = $email_status where c_email_id = $email_id".command
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(command).use(_.execute(status, id).void))
  }

  def updateGroupSystem(id: Group.Id, system: Boolean): IO[Unit] = {
    val command = sql"update t_group set c_system = $bool where c_group_id = $group_id".command
    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(command).use(_.execute(system, id).void))
  }

  def setObservationCalibrationRole(oids: List[Observation.Id], role: CalibrationRole): IO[Unit] =
    val af = void"UPDATE t_observation " |+|
      sql"SET c_calibration_role = $calibration_role "(role) |+|
      void"WHERE c_observation_id IN (" |+|
        oids.map(sql"$observation_id").intercalate(void", ") |+| void")"

    FMain.databasePoolResource[IO](databaseConfig).flatten
      .use(_.prepareR(af.fragment.command).use(_.execute(af.argument).void))

  def cloneGroupAs(user: User, gid: Group.Id): IO[Group.Id] =
    query(
      user = user,
      query = s"""
        mutation {
          cloneGroup(input: {
            groupId: "$gid"
          }) {
            newGroup {
              id
            }
          }
        }
        """
    ).map: j =>
      j.hcursor.downFields("cloneGroup", "newGroup", "id").require[Group.Id]

  def setObservationTimeAndDuration(
    user:  User,
    oid:   Observation.Id,
    obsTime: Option[Timestamp],
    obsDuration: Option[TimeSpan]
  ): IO[Unit] = {
    val time = obsTime.fold("null")(ts => s"\"${ts.isoFormat}\"")
    // microseconds can be bigger than max int. The schema is a Long but values outside of
    // the Int range must be a string.
    val duration = obsDuration.fold("null")(ts => s"{ microseconds: \"${ts.toMicroseconds}\" }")
    val q = s"""
      mutation {
        updateObservationsTimes(input: {
          SET: {
            observationTime: $time
            observationDuration: $duration
          }
          WHERE: {
            id: {EQ: ${oid.asJson}}}
        }) {
          observations {
            observationTime
            observationDuration { microseconds }
          }
        }
      }
    """

    val expectDur: Json = obsDuration.fold(Json.Null)(ts => Json.obj("microseconds" -> ts.toMicroseconds.asJson))
    val expected = json"""
      {
        "updateObservationsTimes": {
          "observations": [
            {
              "observationTime": ${obsTime.map(_.format)},
              "observationDuration": $expectDur
            }
          ]
        }
      }
    """.asRight
    expect(user = user, query = q, expected = expected)
  }

  def setGuideTargetName(
    user: User,
    oid: Observation.Id,
    guideTargetName: Option[String]
  ): IO[Unit] = {
    val q = s"""
      mutation {
        setGuideTargetName(
          input: {
            observationId: ${oid.asJson}
            targetName: ${guideTargetName.asJson}
          }
        ) {
          observation {
            id
          }
        }
      }
    """
    query(user = user, query = q).void
  }

  def createConfigurationRequestAs(user: User, oid: Observation.Id, justification: Option[NonEmptyString] = None): IO[ConfigurationRequest.Id] =
    query(
      user = user,
      query = s"""
        mutation {
          createConfigurationRequest(input: {
            observationId: "$oid",
            SET: {
              justification: ${justification.asJson}
            }
          }) {
            id
          }
        }
      """
    ).map: json =>
      json
        .hcursor
        .downFields("createConfigurationRequest", "id")
        .require[ConfigurationRequest.Id]

  def setObservationWorkflowState(user: User, oid: Observation.Id, wfs: ObservationWorkflowState): IO[ObservationWorkflowState] =
    query(
      user,
      s"""
        mutation {
          setObservationWorkflowState(input: {
            observationId: "$oid"
            state: ${wfs.tag.toUpperCase}
          }) {
            state
          }
        }
        """
    ).map: json =>
      json.hcursor.downFields("setObservationWorkflowState", "state").require[ObservationWorkflowState]

  // write workflow state directly without going through obscalc
  def setCalculatedWorkflowState(oid: Observation.Id, state: ObservationWorkflowState): IO[Unit] =
    session.use(_.execute(sql"""
      UPDATE t_obscalc
      SET c_workflow_state = ${observation_workflow_state}
      WHERE c_observation_id = ${observation_id}
    """.command)(state, oid)).void

  // write workflow state directly without going through obscalc
  def setWorkflowState(oid: Observation.Id, state: ObservationWorkflowState): IO[Unit] =
    session.use(_.execute(sql"""
      UPDATE t_observation
      SET c_workflow_user_state = ${observation_workflow_user_state}
      WHERE c_observation_id = ${observation_id}
    """.command)(state, oid)).void

  private def itcQuery(oids: Observation.Id*) =
    s"""
      query {
        observations(
          WHERE: {
            id: { IN: ${oids.asJson} }
          }
        ) {
          matches {
            itc {
              science {
                selected {
                  targetId
                }
              }
            }
          }
        }
      }
    """

  def computeItcResultAs(user: User, oid: Observation.Id): IO[Unit] =
    query(user, itcQuery(oid)).void

  def createFastTurnaroundProposal(
    user: User,
    pid: Program.Id,
    reviewerId: Option[String] = None,
    mentorId: Option[String] = None
  ): IO[Unit] = {
    val reviewerField = reviewerId.foldMap(id => s"""reviewerId: "$id"""")
    val mentorField = mentorId.foldMap(id => s"""mentorId: "$id"""")
    val additionalFields = List(reviewerField, mentorField).filter(_.nonEmpty).mkString("\n")

    val query = s"""
      mutation {
        createProposal(
          input: {
            programId: "$pid"
            SET: {
              category: COSMOLOGY
              type: {
                fastTurnaround: {
                  toOActivation: NONE
                  minPercentTime: 50
                  $additionalFields
                }
              }
            }
          }
        ) {
          proposal {
            category
            type {
              scienceSubtype
              ... on FastTurnaround {
                toOActivation
                minPercentTime
                reviewer {
                  id
                  role
                }
                mentor {
                  id
                  role
                }
              }
            }
          }
        }
      }
    """

    val expectedReviewer: Json = reviewerId.map(id => json"""{"id": $id, "role": "COI"}""").getOrElse(Json.Null)
    val expectedMentor: Json = mentorId.map(id => json"""{"id": $id, "role": "COI"}""").getOrElse(Json.Null)

    expect(
      user = user,
      query = query,
      expected = json"""
        {
          "createProposal": {
            "proposal": {
              "category": "COSMOLOGY",
              "type": {
                "scienceSubtype": "FAST_TURNAROUND",
                "toOActivation": "NONE",
                "minPercentTime": 50,
                "reviewer": $expectedReviewer,
                "mentor": $expectedMentor
              }
            }
          }
        }
      """.asRight
    )
  }

  def createFastTurnaroundProposalError(
    user: User,
    pid: Program.Id,
    userId: String
  ): IO[Unit] = {
    val query = s"""
      mutation {
        createProposal(
          input: {
            programId: "$pid"
            SET: {
              category: COSMOLOGY
              type: {
                fastTurnaround: {
                  toOActivation: NONE
                  minPercentTime: 50
                  reviewerId: "$userId"
                  mentorId: "$userId"
                }
              }
            }
          }
        ) {
          proposal {
            category
          }
        }
      }
    """

    expectOdbError(
      user = user,
      query = query,
      expected = {
        case OdbError.InvalidArgument(Some("The same user cannot be both reviewer and mentor on a proposal")) => // expected
      }
    )
  }

  def createFastTurnaroundProposalForUpdate(
    user: User,
    pid: Program.Id,
    reviewerId: Option[String] = None,
    mentorId: Option[String] = None
  ): IO[Unit] = {
    val reviewerField = reviewerId.foldMap(id => s"""reviewerId: "$id"""")
    val mentorField = mentorId.foldMap(id => s"""mentorId: "$id"""")
    val additionalFields = List(reviewerField, mentorField).filter(_.nonEmpty).mkString("\n")

    query(
      user = user,
      query = s"""
        mutation {
          createProposal(
            input: {
              programId: "$pid"
              SET: {
                category: COSMOLOGY
                type: {
                  fastTurnaround: {
                    toOActivation: NONE
                    minPercentTime: 50
                    $additionalFields
                  }
                }
              }
            }
          ) {
            proposal {
              category
            }
          }
        }
      """
    ).void
  }

  def updateFastTurnaroundProposal(
    user: User,
    pid: Program.Id,
    reviewerId: Option[String] = None,
    mentorId: Option[String] = None
  ): IO[Unit] = {
    val reviewerField = reviewerId match {
      case Some(id) => s"""reviewerId: "$id""""
      case None => "reviewerId: null"
    }
    val mentorField = mentorId match {
      case Some(id) => s"""mentorId: "$id""""
      case None => "mentorId: null"
    }

    val query = s"""
      mutation {
        updateProposal(
          input: {
            programId: "$pid"
            SET: {
              type: {
                fastTurnaround: {
                  $reviewerField
                  $mentorField
                }
              }
            }
          }
        ) {
          proposal {
            type {
              scienceSubtype
              ... on FastTurnaround {
                toOActivation
                minPercentTime
                reviewer {
                  id
                  role
                }
                mentor {
                  id
                  role
                }
              }
            }
          }
        }
      }
    """

    val expectedReviewer: Json = reviewerId.map(id => json"""{"id": $id, "role": "COI"}""").getOrElse(Json.Null)
    val expectedMentor: Json = mentorId.map(id => json"""{"id": $id, "role": "COI"}""").getOrElse(Json.Null)

    expect(
      user = user,
      query = query,
      expected = json"""
        {
          "updateProposal": {
            "proposal": {
              "type": {
                "scienceSubtype": "FAST_TURNAROUND",
                "toOActivation": "NONE",
                "minPercentTime": 50,
                "reviewer": $expectedReviewer,
                "mentor": $expectedMentor
              }
            }
          }
        }
      """.asRight
    )
  }

  def updateFastTurnaroundProposalError(
    user: User,
    pid: Program.Id,
    userId: String
  ): IO[Unit] = {
    val query = s"""
      mutation {
        updateProposal(
          input: {
            programId: "$pid"
            SET: {
              type: {
                fastTurnaround: {
                  reviewerId: "$userId"
                  mentorId: "$userId"
                }
              }
            }
          }
        ) {
          proposal {
            type {
              scienceSubtype
            }
          }
        }
      }
    """

    expectOdbError(
      user = user,
      query = query,
      expected = {
        case OdbError.InvalidArgument(Some("The same user cannot be both reviewer and mentor on a proposal")) => // expected
      }
    )
  }

}
