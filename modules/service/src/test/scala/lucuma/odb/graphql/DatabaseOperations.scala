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
import lucuma.odb.service.EmailService
import lucuma.odb.service.ObscalcService
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
      given Services[IO] = services
      requireServiceAccessOrThrow:
        val srv  = ObscalcService.instantiate[IO]
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

  protected def scienceRequirementsObject(observingMode: ObservingModeType): String =
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

  /** Subclasses can override to add more handlers. */
  def horizonsFixture: Map[Set[(String, String)], String] = Map(

    Set(
      ("format", "text"), ("MAKE_EPHEM", "YES"), ("CENTER", "T15"), ("COMMAND", "'NAME=1P;CAP'"), ("START_TIME", "'2025-May-02 00:00:00.000'"), ("STOP_TIME", "'2025-May-03 00:00:00.000'"), ("STEP_SIZE", "60m"), ("extra_prec", "YES"), ("time_digits", "FRACSEC"), ("QUANTITIES", "'1,3,8,9'")) ->
      """
      |API VERSION: 1.2
      |API SOURCE: NASA/JPL Horizons API
      |
      |*******************************************************************************
      |JPL/HORIZONS                      1P/Halley                2025-Nov-24 11:37:17
      |Rec #:90000030        Soln.date: 2025-Nov-21_15:57:34   # obs: 8518 (1835-1994)
      |
      |IAU76/J2000 helio. ecliptic osc. elements (au, days, deg., period=Julian yrs):
      |
      |  EPOCH=  2439875.5 ! 1968-Jan-20.0000000 (TDB)    RMSW= n.a.
      |  EC= .9679359956953212   QR= .5748638313743413   TP= 2446469.9736161465
      |  OM= 59.09894720612437   W= 112.2414314637764    IN= 162.1905300439129
      |  A= 17.92863504856929    MA= 274.3823371364693   ADIST= 35.28240626576424
      |  PER= 75.915252807404    N= .012983244           ANGMOM= .018296559
      |  DAN= 1.78543            DDN= .82795             L= 305.8544912
      |  B= 16.4450919           MOID= .0745097          TP= 1986-Feb-08.4736161465
      |
      |Comet physical (GM= km^3/s^2; RAD= km):
      |  GM= n.a.                RAD= 5.5
      |  M1=  5.5      M2=  13.6     k1=  8.     k2=  5.      PHCOF=  .030
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au):
      |  AMRAT=  0.                                      DT=  0.
      |  A1= 4.887055233121E-10  A2= 1.554720290005E-10  A3= 0.
      |Standard model:
      |  ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808
      |
      |COMET comments 
      |1: soln ref.= JPL#75, data arc: 1835-08-21 to 1994-01-11
      |2: k1=8.0, k2=5.0, phase coef.=0.03;
      |*******************************************************************************
      |
      |
      |*******************************************************************************
      |Ephemeris / API_USER Mon Nov 24 11:37:17 2025 Pasadena, USA      / Horizons    
      |*******************************************************************************
      |Target body name: 1P/Halley                       {source: JPL#75}
      |Center body name: Earth (399)                     {source: DE441}
      |Center-site name: Gemini North Observatory, Maunakea
      |*******************************************************************************
      |Start time      : A.D. 2025-May-02 00:00:00.0000 UT      
      |Stop  time      : A.D. 2025-May-03 00:00:00.0000 UT      
      |Step-size       : 60 minutes
      |*******************************************************************************
      |Target pole/equ : undefined
      |Target radii    : 5.5 km                                                       
      |Center geodetic : 204.5309, 19.8238126, 4.24672   {E-lon(deg),Lat(deg),Alt(km)}
      |Center cylindric: 204.5309,6006.47419,2150.79851  {E-lon(deg),Dxy(km),Dz(km)}
      |Center pole/equ : ITRF93                          {East-longitude positive}
      |Center radii    : 6378.137, 6378.137, 6356.752 km {Equator_a, b, pole_c}       
      |Target primary  : Sun
      |Vis. interferer : MOON (R_eq= 1737.400) km        {source: DE441}
      |Rel. light bend : Sun                             {source: DE441}
      |Rel. lght bnd GM: 1.3271E+11 km^3/s^2                                          
      |Small-body perts: Yes                             {source: SB441-N16}
      |Atmos refraction: NO (AIRLESS)
      |RA format       : HMS
      |Time format     : CAL 
      |Calendar mode   : Mixed Julian/Gregorian
      |EOP file        : eop.251121.p260217                                           
      |EOP coverage    : DATA-BASED 1962-JAN-20 TO 2025-NOV-21. PREDICTS-> 2026-FEB-16
      |Units conversion: 1 au= 149597870.700 km, c= 299792.458 km/s, 1 day= 86400.0 s 
      |Table cut-offs 1: Elevation (-90.0deg=NO ),Airmass (>38.000=NO), Daylight (NO )
      |Table cut-offs 2: Solar elongation (  0.0,180.0=NO ),Local Hour Angle( 0.0=NO )
      |Table cut-offs 3: RA/DEC angular rate (     0.0=NO )                           
      |*******************************************************************************
      |Initial IAU76/J2000 heliocentric ecliptic osculating elements (au, days, deg.):
      |  EPOCH=  2439875.5 ! 1968-Jan-20.0000000 (TDB)    RMSW= n.a.                  
      |  EC= .9679359956953212   QR= .5748638313743413   TP= 2446469.9736161465      
      |  OM= 59.09894720612437   W= 112.2414314637764    IN= 162.1905300439129       
      |  Equivalent ICRF heliocentric cartesian coordinates (au, au/d):
      |  X=-1.331029360169393E+01  Y= 2.541249958785733E+01  Z= 2.637549316318327E+00
      |  VX= 1.418949944126011E-03 VY=-1.422475975617656E-03 VZ= 4.131321199969281E-05
      |Comet physical (GM= km^3/s^2; RAD= km):                                        
      |  GM= n.a.                RAD= 5.5                                            
      |  M1=  5.5      M2=  13.6     k1=  8.     k2=  5.      PHCOF=  .030           
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au): 
      |  AMRAT=  0.                                      DT=  0.                     
      |  A1= 4.887055233121E-10  A2= 1.554720290005E-10  A3= 0.                      
      |Standard model:                                                               
      |  ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808    
      |*****************************************************************************************************************
      |Date__(UT)__HR:MN:SC.fff     R.A._________(ICRF)_________DEC  dRA*cosD d(DEC)/dt  a-mass mag_ex    T-mag   N-mag
      |*****************************************************************************************************************
      |$$SOE
      |2025-May-02 00:00:00.000 *m  08 09 46.797665 +03 21 19.54244  0.585974  1.072429   1.931  0.210   25.598  29.111
      |2025-May-02 01:00:00.000 *m  08 09 46.836306 +03 21 20.61530  0.576317  1.071027   1.415  0.154   25.598  29.111
      |2025-May-02 02:00:00.000 *m  08 09 46.874400 +03 21 21.68666  0.569751  1.069460   1.177  0.128   25.598  29.111
      |2025-May-02 03:00:00.000 *m  08 09 46.912179 +03 21 22.75638  0.566918  1.067778   1.069  0.116   25.598  29.111
      |2025-May-02 04:00:00.000 *m  08 09 46.949905 +03 21 23.82438  0.568205  1.066042   1.043  0.114   25.598  29.111
      |2025-May-02 05:00:00.000 Cm  08 09 46.987858 +03 21 24.89065  0.573718  1.064313   1.090  0.119   25.598  29.111
      |2025-May-02 06:00:00.000 Am  08 09 47.026316 +03 21 25.95524  0.583271  1.062656   1.227  0.134   25.598  29.111
      |2025-May-02 07:00:00.000  m  08 09 47.065535 +03 21 27.01825  0.596403  1.061126   1.518  0.165   25.598  29.111
      |2025-May-02 08:00:00.000  m  08 09 47.105732 +03 21 28.07985  0.612406  1.059774   2.179  0.237   25.598  29.111
      |2025-May-02 09:00:00.000  m  08 09 47.147069 +03 21 29.14024  0.630376  1.058636   4.323  0.471   25.598  29.111
      |2025-May-02 10:00:00.000     08 09 47.189642 +03 21 30.19964  0.649275  1.057733    n.a.   n.a.   25.598  29.111
      |2025-May-02 11:00:00.000     08 09 47.233479 +03 21 31.25831  0.668001  1.057072    n.a.   n.a.   25.598  29.111
      |2025-May-02 12:00:00.000     08 09 47.278529 +03 21 32.31647  0.685463  1.056642    n.a.   n.a.   25.599  29.111
      |2025-May-02 13:00:00.000     08 09 47.324675 +03 21 33.37433  0.700657  1.056415    n.a.   n.a.   25.599  29.111
      |2025-May-02 14:00:00.000     08 09 47.371736 +03 21 34.43209  0.712735  1.056353    n.a.   n.a.   25.599  29.111
      |2025-May-02 15:00:00.000 A   08 09 47.419481 +03 21 35.48986  0.721062  1.056402    n.a.   n.a.   25.599  29.111
      |2025-May-02 16:00:00.000 *   08 09 47.467645 +03 21 36.54773  0.725260  1.056503    n.a.   n.a.   25.599  29.111
      |2025-May-02 17:00:00.000 *   08 09 47.515949 +03 21 37.60571  0.725233  1.056593    n.a.   n.a.   25.599  29.111
      |2025-May-02 18:00:00.000 *   08 09 47.564114 +03 21 38.66374  0.721176  1.056609    n.a.   n.a.   25.599  29.111
      |2025-May-02 19:00:00.000 *   08 09 47.611887 +03 21 39.72171  0.713560  1.056495    n.a.   n.a.   25.599  29.111
      |2025-May-02 20:00:00.000 *   08 09 47.659051 +03 21 40.77946  0.703098  1.056201    n.a.   n.a.   25.599  29.111
      |2025-May-02 21:00:00.000 *m  08 09 47.705446 +03 21 41.83679  0.690699  1.055691    n.a.   n.a.   25.599  29.111
      |2025-May-02 22:00:00.000 *m  08 09 47.750978 +03 21 42.89347  0.677406  1.054945  12.398  1.350   25.599  29.111
      |2025-May-02 23:00:00.000 *m  08 09 47.795623 +03 21 43.94925  0.664320  1.053955   3.214  0.350   25.599  29.111
      |2025-May-03 00:00:00.000 *m  08 09 47.839432 +03 21 45.00389  0.652531  1.052735   1.882  0.205   25.599  29.111
      |$$EOE
      |*****************************************************************************************************************
      |Column meaning:
      |
      |TIME
      |
      |  Times PRIOR to 1962 are UT1, a mean-solar time closely related to the
      |prior but now-deprecated GMT. Times AFTER 1962 are in UTC, the current
      |civil or "wall-clock" time-scale. UTC is kept within 0.9 seconds of UT1
      |using integer leap-seconds for 1972 and later years.
      |
      |  Conversion from the internal Barycentric Dynamical Time (TDB) of solar
      |system dynamics to the non-uniform civil UT time-scale requested for output
      |has not been determined for UTC times after the next July or January 1st.
      |Therefore, the last known leap-second is used as a constant over future
      |intervals.
      |
      |  Time tags refer to the UT time-scale conversion from TDB on Earth
      |regardless of observer location within the solar system, although clock
      |rates may differ due to the local gravity field and no analog to "UT"
      |may be defined for that location.
      |
      |  Any 'b' symbol in the 1st-column denotes a B.C. date. First-column blank
      |(" ") denotes an A.D. date.
      |
      |CALENDAR SYSTEM
      |
      |  Mixed calendar mode was active such that calendar dates after AD 1582-Oct-15
      |(if any) are in the modern Gregorian system. Dates prior to 1582-Oct-5 (if any)
      |are in the Julian calendar system, which is automatically extended for dates
      |prior to its adoption on 45-Jan-1 BC.  The Julian calendar is useful for
      |matching historical dates. The Gregorian calendar more accurately corresponds
      |to the Earth's orbital motion and seasons. A "Gregorian-only" calendar mode is
      |available if such physical events are the primary interest.
      |
      |  NOTE: "n.a." in output means quantity "not available" at the print-time.
      |
      |SOLAR PRESENCE (OBSERVING SITE)
      |  Time tag is followed by a blank, then a solar-presence symbol:
      |
      |      '*'  Daylight (refracted solar upper-limb on or above apparent horizon)
      |      'C'  Civil twilight/dawn
      |      'N'  Nautical twilight/dawn
      |      'A'  Astronomical twilight/dawn
      |      ' '  Night OR geocentric ephemeris
      |
      |LUNAR PRESENCE (OBSERVING SITE)
      |  The solar-presence symbol is immediately followed by a lunar-presence symbol:
      |
      |      'm'  Refracted upper-limb of Moon on or above apparent horizon
      |      ' '  Refracted upper-limb of Moon below apparent horizon OR geocentric
      |            ephemeris
      |
      |'R.A._________(ICRF)_________DEC' =
      |  Astrometric right ascension and declination of the target center with
      |respect to the observing site (coordinate origin) in the reference frame of
      |the planetary ephemeris (ICRF). Compensated for down-leg light-time delay
      |aberration.
      |
      |  Units: RA  in hours-minutes-seconds of time,    HH MM SS.ff{ffff}
      |        DEC in degrees-minutes-seconds of arc,  sDD MN SC.f{ffff}
      |
      |'dRA*cosD d(DEC)/dt' =
      |  The angular rate of change in apparent RA and DEC of the target. This is
      |with respect to the non-inertial IAU76/80 Earth true equator and equinox
      |of-date reference frame.  d(RA)/dt is multiplied by the cosine of declination
      |to provide a linear rate in the plane-of-sky. Units: ARCSECONDS PER HOUR
      |
      |'a-mass mag_ex' =
      |    RELATIVE optical airmass and visual magnitude extinction. Airmass is the
      |ratio between the absolute optical airmass for the targets' refracted CENTER
      |point to the absolute optical airmass at zenith. Also output is the estimated
      |visual magnitude extinction due to the atmosphere, as seen by the observer.
      |AVAILABLE ONLY FOR TOPOCENTRIC EARTH SITES WHEN THE TARGET IS ABOVE THE
      |HORIZON.  Units: none (airmass) and magnitudes (extinction).
      |
      |'T-mag   N-mag' =
      |  Comets' apparent visual total magnitude ("T-mag") and nuclear magnitude
      |("N-mag") using the standard IAU model:
      |
      |  T-mag= M1 + 5*log10(delta) + k1*log10(r)
      |  N-mag= M2 + 5*log10(delta) + k2*log10(r) + phcof*beta
      |
      |  Units: MAGNITUDES
      |
      |Computations by ...
      |
      |    Solar System Dynamics Group, Horizons On-Line Ephemeris System
      |    4800 Oak Grove Drive, Jet Propulsion Laboratory
      |    Pasadena, CA  91109   USA
      |
      |    General site: https://ssd.jpl.nasa.gov/
      |    Mailing list: https://ssd.jpl.nasa.gov/email_list.html
      |    System news : https://ssd.jpl.nasa.gov/horizons/news.html
      |    User Guide  : https://ssd.jpl.nasa.gov/horizons/manual.html
      |    Connect     : browser        https://ssd.jpl.nasa.gov/horizons/app.html#/x
      |                  API            https://ssd-api.jpl.nasa.gov/doc/horizons.html
      |                  command-line   telnet ssd.jpl.nasa.gov 6775
      |                  e-mail/batch   https://ssd.jpl.nasa.gov/ftp/ssd/horizons_batch.txt
      |                  scripts        https://ssd.jpl.nasa.gov/ftp/ssd/SCRIPTS
      |    Author      : Jon.D.Giorgini@jpl.nasa.gov
      |
      |*****************************************************************************************************************
      |""".stripMargin
        )

}
