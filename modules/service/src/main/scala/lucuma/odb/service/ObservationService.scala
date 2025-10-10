// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.implicits.*
import eu.timepit.refined.api.Refined.value
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Group
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Program
import lucuma.core.model.StandardRole.*
import lucuma.core.model.Target
import lucuma.core.syntax.string.*
import lucuma.odb.data.Existence
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.ExposureTimeModeType
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.data.Tag
import lucuma.odb.graphql.input.ConstraintSetInput
import lucuma.odb.graphql.input.ElevationRangeInput
import lucuma.odb.graphql.input.ImagingScienceRequirementsInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ObservationTimesInput
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.graphql.input.PosAngleConstraintInput
import lucuma.odb.graphql.input.ScienceRequirementsInput
import lucuma.odb.graphql.input.SpectroscopyScienceRequirementsInput
import lucuma.odb.graphql.input.TargetEnvironmentInput
import lucuma.odb.graphql.input.TimingWindowInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.all.*
import skunk.exception.PostgresErrorException
import skunk.implicits.*

import Services.Syntax.*

sealed trait ObservationService[F[_]] {

  /**
   * Finds the observation id consistent with the given ids (if any).
   */
  def resolveOid(
    oid: Option[Observation.Id],
    ref: Option[ObservationReference]
  )(using SuperUserAccess): F[Result[Observation.Id]]

  def selectProgram(
    oid: Observation.Id
  )(using Transaction[F]): F[Result[Program.Id]]

  def createObservation(
    input: AccessControl.CheckedWithId[ObservationPropertiesInput.Create, Program.Id]
  )(using Transaction[F]): F[Result[Observation.Id]]

  def updateObservations(
    update: AccessControl.Checked[ObservationPropertiesInput.Edit]
  )(using Transaction[F]): F[Result[Map[Program.Id, List[Observation.Id]]]]

  def updateObservationsTimes(
    update: AccessControl.Checked[ObservationTimesInput]
  )(using Transaction[F]): F[Result[Map[Program.Id, List[Observation.Id]]]]

  def cloneObservation(
    input: AccessControl.CheckedWithId[Option[ObservationPropertiesInput.Edit], Observation.Id]
  )(using Transaction[F]): F[Result[ObservationService.CloneIds]]

  def deleteCalibrationObservations(
    oids: NonEmptyList[Observation.Id]
  )(using Transaction[F], ServiceAccess): F[Result[Unit]]

  def resetAcquisition(
    input: AccessControl.CheckedWithId[Unit, Observation.Id]
  )(using Transaction[F]): F[Result[Observation.Id]]

  def selectBands(
    pid: Program.Id
  )(using Transaction[F]): F[Map[Observation.Id, Option[ScienceBand]]]

}

object ObservationService {

  final case class ItcParams(
    constraints:     ConstraintSet,
    signalToNoise:   SignalToNoise,
    signalToNoiseAt: Option[Wavelength],
    observingMode:   ObservingModeType
  )

  final case class DatabaseConstraint(
    constraint: String,
    message:    String
  )

  val MissingAirMassConstraint: DatabaseConstraint =
    DatabaseConstraint(
      "air_mass_neither_or_both",
      "airMass constraint requires both min and max values."
    )

  val MissingHourAngleConstraint: DatabaseConstraint =
    DatabaseConstraint(
      "hour_angle_neither_or_both",
      "hourAngle constraint requires both minHours and maxHours."
    )

  val MissingScienceBandConstraint: DatabaseConstraint =
    DatabaseConstraint(
      "obs_status_science_band",
      "a science band must be assigned to ready, executing or executed observations"
    )

  val BothExplicitCoordinatesConstraint: DatabaseConstraint =
    DatabaseConstraint(
      "explicit_base_neither_or_both",
      "explicitBase requires both ra and dec"
    )

  def GenericConstraintViolationMessage(m: String): String =
    s"Database constraint violation produced by input: $m"

  val DatabaseConstraints: List[DatabaseConstraint] =
    List(
      MissingAirMassConstraint,
      MissingHourAngleConstraint,
      MissingScienceBandConstraint,
      BothExplicitCoordinatesConstraint
    )

  def constraintViolationMessage(ex: PostgresErrorException): String =
    DatabaseConstraints
      .find { dc => ex.message.contains(dc.constraint) }
      .map(_.message)
      .getOrElse(GenericConstraintViolationMessage(ex.message))

  // observation validation messages
  def InvalidInstrumentMsg(instr: Instrument) = s"Instrument $instr not part of Call for Proposals."
  def MissingDataMsg(otid: Option[Target.Id], paramName: String) =
    otid.fold(s"Missing $paramName")(tid => s"Missing $paramName for target $tid")
  def InvalidScienceBandMsg(b: ScienceBand) = s"Science Band ${b.tag.toScreamingSnakeCase} has no time allocation."
  val ConfigurationForReviewMsg = "Observation must be reviewed prior to execution."
  object ConfigurationRequestMsg:
    val Unavailable  = "Configuration approval status could not be determined."
    val NotRequested = "Configuration is unapproved (approval has not been requested)."
    val Denied       = "Configuration is unapproved (request was denied)."
    val Pending      = "Configuration is unapproved (request is pending)."

  case class CloneIds(
    originalId: Observation.Id,
    cloneId:    Observation.Id
  )

  case class ObservationValidationInfo(
    instrument: Option[Instrument],
    ra:         Option[RightAscension],
    dec:        Option[Declination],
    forReview:  Boolean,
    role:       Option[CalibrationRole],
    proposalStatus: Tag,
  ) {
    def isMarkedReady: Boolean = false // TODO
  }

  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ObservationService[F] =
    new ObservationService[F] {

      val resolver = new IdResolver("observation", Statements.selectOid, _.label)

      override def resolveOid(
        oid: Option[Observation.Id],
        ref: Option[ObservationReference]
      )(using SuperUserAccess): F[Result[Observation.Id]] =
        resolver.resolve(oid, ref)

      override def selectProgram(
        oid: Observation.Id
      )(using Transaction[F]): F[Result[Program.Id]] =
        session.option(Statements.selectPid)(oid).map: p =>
          p.fold(OdbError.InvalidObservation(oid, s"Program for observation $oid not found.".some).asFailure)(_.success)

      private def setTimingWindows(
        oids:          List[Observation.Id],
        timingWindows: Option[List[TimingWindowInput]],
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =
        timingWindows
          .traverse(timingWindowService.createFunction)
          .traverse { optF =>
            optF.fold(().pure[F])( f => f(oids, transaction) )
          }

      /** Create the observation itself, with no asterism. */
      private def createObservationImpl(
        programId: Program.Id,
        SET:       ObservationPropertiesInput.Create
      )(using Transaction[F], SuperUserAccess): F[Result[Observation.Id]] =
        Trace[F].span("createObservation") {
          session.execute(sql"set constraints all deferred".command) >>
          session.prepareR(GroupService.Statements.OpenHole).use(_.unique(programId, SET.group, SET.groupIndex)).flatMap { ix =>
            val oEtm = SET.scienceRequirements.flatMap(_.exposureTimeMode.toOption)

            Statements
              .insertObservation(programId, SET, ix)

              .flatTraverse: af =>
                session.prepareR(af.fragment.query(observation_id)).use: pq =>
                  pq.unique(af.argument).map(Result.success)

              .flatMap: rOid =>
                rOid
                  .flatTraverse: oid =>
                    SET.observingMode.fold(oid.success.pure[F]): m =>
                      observingModeServices
                        .create(m, oEtm, List(oid))
                        .map(_.as(oid))

              .flatTap: rOid =>
                (rOid.toOption, oEtm)
                  .tupled
                  .traverse_ : (oid, etm) =>
                    services
                      .exposureTimeModeService
                      .insertOne(oid, ExposureTimeModeRole.Requirement, etm)

              .flatTap: rOid =>
                rOid.flatTraverse: oid =>
                  Services.asSuperUser(setTimingWindows(List(oid), SET.timingWindows))

              .flatMap: rOid =>
                SET.attachments.fold(rOid.pure[F]): aids =>
                  rOid.flatTraverse: oid =>
                    obsAttachmentAssignmentService
                      .insertAssignments(programId, List(oid), aids)
                      .map(_.map(_ => oid))

          }.recoverWith:
             case SqlState.CheckViolation(ex) =>
               OdbError.InvalidArgument(Some(constraintViolationMessage(ex))).asFailureF
        }

      // This will fully delete a calibration observation
      // It assumes the simple case where the observation has no extra timing windows or attachments
      // targets are not deleted here because they may be shared with other observations
      // Orphaned targets should be cleaned up separately via deleteOrphanCalibrationTargets
      def deleteCalibrationObservations(
        oids: NonEmptyList[Observation.Id]
      )(using Transaction[F], ServiceAccess): F[Result[Unit]] = {
        val existenceOff = ObservationPropertiesInput.Edit.Empty.copy(
          existence = Existence.Deleted.some,
          group     = Nullable.Null
        )

        // delete asterisms and observations
        for {
          _    <- oids.traverse { o =>
                    // set the existence to deleted, so it gets removed from groups too
                    updateObservations:
                      Services.asSuperUser:
                        AccessControl.unchecked(existenceOff, List(o), observation_id)
                  }
                  // Delete asterism_target entries for these observations
          _    <- session.executeCommand(Statements.deleteAsterismsForObservations(oids))
                  // Delete the observations themselves
          _    <- session.executeCommand(Statements.deleteCalibrationObservations(oids))
        } yield Result.unit

      }

      override def createObservation(
        input: AccessControl.CheckedWithId[ObservationPropertiesInput.Create, Program.Id]
      )(using Transaction[F]): F[Result[Observation.Id]] =
        input.foldWithId(
          OdbError.InvalidArgument().asFailureF // typically handled by caller
        ): (SET, pid) =>
          ResultT(Services.asSuperUser(createObservationImpl(pid, SET)))
            .flatMap: oid =>
              SET
                .asterism
                .traverse: a =>
                  ResultT:
                    Services.asSuperUser:
                      asterismService.insertAsterism(pid, NonEmptyList.one(oid), a)
                .as(oid)
            .flatMap: oid =>
              SET
                .targetEnvironment
                .flatMap(te => te.blindOffsetTarget.map((_, te.blindOffsetType)))
                .traverse: (targetInput, blindOffsetType) =>
                  ResultT:
                    Services.asSuperUser:
                      blindOffsetsService.createBlindOffset(pid, oid, targetInput, blindOffsetType)
                .as(oid)
            .value
            .flatTap: r =>
              transaction.rollback.unlessA(r.hasValue)

      @annotation.nowarn("msg=unused implicit parameter")
      def selectObservingModes(
        which: List[Observation.Id]
      )(using Transaction[F]): F[Map[Option[ObservingModeType], List[Observation.Id]]] =
        NonEmptyList
          .fromList(which)
          .fold(Applicative[F].pure(Map.empty)) { oids =>
            val af = Statements.selectObservingModes(oids)
            session.prepareR(af.fragment.query(observation_id ~ observing_mode_type.opt)).use { pq =>
              pq.stream(af.argument, chunkSize = 1024).compile.toList.map {
                _.groupBy(_._2).view.mapValues(_.unzip._1).toMap
              }
            }
          }

      private def updateObservingModeType(
        newMode: Option[ObservingModeType],
        which:   NonEmptyList[Observation.Id]
      ): F[Unit] = {
        val af = Statements.updateObservingModeType(newMode, which)
        session.prepareR(af.fragment.command).use { pq =>
          pq.execute(af.argument).void
        }
      }

      private def updateObservingModes(
        nEdit: Nullable[ObservingModeInput.Edit],
        oids:  NonEmptyList[Observation.Id],
        etm:   Option[ExposureTimeMode]
      )(using Transaction[F], SuperUserAccess): F[Result[Unit]] =

        nEdit.toOptionOption.fold(Result.unit.pure[F]) { oEdit =>
          for {
            m <- selectObservingModes(oids.toList)
            _ <- updateObservingModeType(oEdit.flatMap(_.observingModeType), oids)
            r <- m.toList.traverse { case (existingMode, matchingOids) =>

              (existingMode, oEdit) match {
                case (Some(ex), Some(edit)) if edit.observingModeType.contains(ex) =>
                  // update existing
                  observingModeServices.updateFunction(edit).traverse(_(matchingOids))

                case (Some(ex), Some(edit)) =>
                  for {
                    // delete existing
                    _ <- observingModeServices.deleteFunction(ex)(matchingOids)

                    // create new
                    r <- observingModeServices.createViaUpdate(edit, etm, matchingOids)
                  } yield r

                case (None,    Some(edit)) =>
                  // create new
                  observingModeServices.createViaUpdate(edit, etm, matchingOids)

                case (Some(ex), None) =>
                  // delete existing
                  observingModeServices.deleteFunction(ex)(matchingOids).as(Result.unit)

                case _  =>
                  // do nothing
                  Result.unit.pure[F]
              }
            }.map(_.sequence.void)
          } yield r
        }

      // Applying the same move to a list of observations will put them all together in the
      // destination group (or at the top level) in no particular order.
      def moveObservations(
        groupId: Nullable[Group.Id],
        groupIndex: Option[NonNegShort],
        which: AppliedFragment
      ): F[Unit] =
        (groupId, groupIndex) match
          case (Nullable.Absent, None) => ().pure[F] // do nothing if neither is specified
          case (gid, index) =>
            val af = Statements.moveObservations(gid.toOption, index, which)
            session.prepareR(af.fragment.query(void)).use(pq => pq.stream(af.argument, 512).compile.drain)

      override def updateObservations(
        update: AccessControl.Checked[ObservationPropertiesInput.Edit]
      )(using Transaction[F]): F[Result[Map[Program.Id, List[Observation.Id]]]] =
        Trace[F].span("updateObservation") {
          update.fold(Result(Map.empty).pure[F]): (SET, which) =>
            def validateBand(pids: => List[Program.Id]): ResultT[F, Unit] =
              SET.scienceBand.toOption.fold(ResultT.unit): band =>
                ResultT:
                  Services.asSuperUser:
                    allocationService.validateBand(band, pids)

            val updates: ResultT[F, Map[Program.Id, List[Observation.Id]]] =
              for {
                r <- ResultT(Statements.updateObservations(SET, which).traverse { af =>
                        session.prepareR(af.fragment.query(program_id *: observation_id)).use { pq =>
                          pq.stream(af.argument, chunkSize = 1024).compile.toList
                        }
                    })
                g  = r.groupMap(_._1)(_._2)                                        // grouped:   Map[Program.Id, List[Observation.Id]]
                u  = g.values.reduceOption(_ ++ _).flatMap(NonEmptyList.fromList)  // ungrouped: NonEmptyList[Observation.Id]

                e  = SET.scienceRequirements.map(_.exposureTimeMode).getOrElse(Nullable.Absent)

                _ <- ResultT.liftF:
                       u.fold(().pure[F]): u =>
                         e.fold(
                           services.exposureTimeModeService.delete(u, ExposureTimeModeRole.Requirement.some),
                           ().pure[F],
                           e => services.exposureTimeModeService.updateMany(u, ExposureTimeModeRole.Requirement, e)
                         )

                _ <- validateBand(g.keys.toList)
                _ <- ResultT(u.map(u => Services.asSuperUser(updateObservingModes(SET.observingMode, u, e.toOption))).getOrElse(Result.unit.pure[F]))
                _ <- ResultT(Services.asSuperUser(setTimingWindows(u.foldMap(_.toList), SET.timingWindows.foldPresent(_.orEmpty))))
                _ <- ResultT(g.toList.traverse { case (pid, oids) =>
                      obsAttachmentAssignmentService.setAssignments(pid, oids, SET.attachments)
                    }.map(_.sequence))
                _ <- ResultT(Services.asSuperUser(g.toList.flatTraverse { case (pid, oids) =>
                      oids.traverse(oid => blindOffsetsService.updateBlindOffset(pid, oid, SET.targetEnvironment))
                    }.map(_.sequence)))
            } yield g

            for {
              _ <- session.execute(sql"set constraints all deferred".command)
              _ <- moveObservations(SET.group, SET.groupIndex, which)
              r <- updates.value.recoverWith {
                    case SqlState.CheckViolation(ex) =>
                      OdbError.InvalidArgument(Some(constraintViolationMessage(ex))).asFailureF
                  }
              _ <- transaction.rollback.unlessA(r.hasValue) // rollback if something failed
            } yield r
        }

      override def updateObservationsTimes(
        update: AccessControl.Checked[ObservationTimesInput]
      )(using Transaction[F]): F[Result[Map[Program.Id, List[Observation.Id]]]] =
        Trace[F].span("updateObservationTimes"):
          update.fold(Result(Map.empty).pure[F]): (set, which) =>
            Statements.updateObsTime(set, which).traverse: af =>
              session
                .prepareR(af.fragment.query(program_id *: observation_id))
                .use: pq =>
                  pq.stream(af.argument, chunkSize = 1024)
                    .compile
                    .toList
                .map: list =>
                  list.groupMap(_._1)(_._2)

      /** Clone the observation. We assume access has been checked already. */
      private def cloneObservationUnconditionally(
        observationId: Observation.Id,
        SET:           Option[ObservationPropertiesInput.Edit]
      )(using Transaction[F]): F[Result[(Program.Id, Observation.Id)]] = {

        // First we need the pid, observing mode, and grouping information
        val selPid = sql"select c_program_id, c_observing_mode_type, c_group_id, c_group_index from t_observation where c_observation_id = $observation_id"
        session.prepareR(selPid.query(program_id *: observing_mode_type.opt *: group_id.opt *: int2_nonneg)).use(_.unique(observationId)).flatMap {

          case (pid, observingMode, gid, gix) =>

            // Desired group index is gix + 1
            val destGroupIndex = NonNegShort.unsafeFrom((gix.value + 1).toShort)

            // Ok the obs exists, so let's clone its main row in t_observation. If this returns
            // None then it means the user doesn't have permission to see the obs.
            val cObsStmt = Statements.cloneObservation(observationId, destGroupIndex)
            val cloneObs = session.prepareR(cObsStmt.fragment.query(observation_id)).use(_.option(cObsStmt.argument))

            // Action to open a hole in the destination program/group after the observation we're cloning
            val openHole: F[NonNegShort] =
              session.execute(sql"set constraints all deferred".command) >>
              session.prepareR(sql"select group_open_hole($program_id, ${group_id.opt}, ${int2_nonneg.opt})".query(int2_nonneg)).use { pq =>
                pq.unique(pid, gid, destGroupIndex.some)
              }

            // Ok let's do the clone.
            (openHole >> cloneObs).flatMap {

              case None =>
                // User doesn't have permission to see the obs
                Result.failure(s"No such observation: $observationId").pure[F]

              case Some(oid2) =>

                val cloneRelatedItems =
                  Services.asSuperUser:
                    asterismService.cloneAsterism(observationId, oid2) >>
                    exposureTimeModeService.clone(observationId, oid2) >>
                    observingMode.traverse(observingModeServices.cloneFunction(_)(observationId, oid2)) >>
                    timingWindowService.cloneTimingWindows(observationId, oid2) >>
                    obsAttachmentAssignmentService.cloneAssignments(observationId, oid2)

                val cloneBlindOffset = // only clone if it won't be overwritten by the updateObservations
                  if SET.flatMap(_.targetEnvironment).fold(true)(_.blindOffsetTarget.isAbsent) then
                    blindOffsetsService.cloneBlindOffset(pid, observationId, oid2)
                  else Result.unit.pure

                val doUpdate =
                  SET match
                    case None    => Result((pid, oid2)).pure[F] // nothing to do
                    case Some(s) =>
                      updateObservations(Services.asSuperUser(AccessControl.unchecked(s, List(oid2), observation_id))).map { r =>
                          // We probably don't need to check this return value, but I feel bad not doing it.
                          r.map(_.toList).flatMap {
                            case List((`pid`, List(`oid2`))) => Result((pid, oid2))
                            case other                       => Result.failure(s"Observation update: expected ($pid, [$oid2]), found ${other.mkString("[", ",", "]")}")
                          }
                        }
                        .flatTap {
                          r => transaction.rollback.unlessA(r.hasValue)
                        }
                (
                  for
                    _ <- ResultT.liftF(cloneRelatedItems)
                    _ <- ResultT(cloneBlindOffset)
                    r <- ResultT(doUpdate)
                  yield r
                ).value
            }
        }
      }

      def cloneObservation(
        input: AccessControl.CheckedWithId[Option[ObservationPropertiesInput.Edit], Observation.Id]
      )(using Transaction[F]): F[Result[ObservationService.CloneIds]] =
        input.foldWithId(OdbError.InvalidArgument().asFailureF): (oSET, origOid) =>
          cloneObservationUnconditionally(origOid, oSET).flatMap: res =>
            res.flatTraverse: (pid, newOid) =>
              Services.asSuperUser:
                asterismService
                  .setAsterism(pid, NonEmptyList.of(newOid), oSET.fold(Nullable.Absent)(_.asterism))
                  .map(_.as(CloneIds(origOid, newOid)))

      override def resetAcquisition(
        input: AccessControl.CheckedWithId[Unit, Observation.Id]
      )(using Transaction[F]): F[Result[Observation.Id]] =
        input.foldWithId(OdbError.InvalidArgument().asFailureF): (_, oid) =>
          session.execute(Statements.ResetAcquisition)(oid).as(oid.success)

      override def selectBands(
        pid: Program.Id
      )(using Transaction[F]): F[Map[Observation.Id, Option[ScienceBand]]] =
        session
          .execute(Statements.SelectBands)(pid)
          .map(_.toMap)

    }


  private object Statements {

    extension (m: ExposureTimeMode)
      def tpe: ExposureTimeModeType =
        m match
          case ExposureTimeMode.SignalToNoiseMode(_, _)   => ExposureTimeModeType.SignalToNoiseMode
          case ExposureTimeMode.TimeAndCountMode(_, _, _) => ExposureTimeModeType.TimeAndCountMode

    val selectOid: Query[ObservationReference, Observation.Id] =
      sql"""
        SELECT c_observation_id
          FROM t_observation
         WHERE c_observation_reference = $observation_reference
      """.query(observation_id)

    val selectPid: Query[Observation.Id, Program.Id] =
      sql"""
        SELECT c_program_id
          FROM t_observation
         WHERE c_observation_id = $observation_id
      """.query(program_id)

    def insertObservation(
      programId: Program.Id,
      SET:       ObservationPropertiesInput.Create,
      groupIndex: NonNegShort,
    ): Result[AppliedFragment] =
      SET.constraintSet.traverse(_.create).map { cs =>
        insertObservation(
          programId,
          SET.group,
          groupIndex,
          SET.subtitle,
          SET.existence.getOrElse(Existence.Default),
          SET.scienceBand,
          SET.posAngleConstraint.flatMap(_.mode).getOrElse(PosAngleConstraintMode.Unbounded),
          SET.posAngleConstraint.flatMap(_.angle).getOrElse(Angle.Angle0),
          SET.targetEnvironment.flatMap(_.explicitBase),
          cs.getOrElse(ConstraintSetInput.NominalConstraints),
          SET.scienceRequirements,
          SET.observingMode.flatMap(_.observingModeType),
          SET.observingMode.flatMap(_.observingModeType).map(_.instrument),
          SET.observerNotes,
          SET.targetEnvironment.flatMap(_.useBlindOffset).getOrElse(false)
        )
      }

//        val exposureTimeMode: Option[ExposureTimeMode] =
//          scienceRequirements.flatMap(_.exposureTimeMode.toOption)
//
//        val exposureTimeModeType: Option[ExposureTimeModeType] =
//          exposureTimeMode.map(_.tpe)
//
//        val signalToNoiseExposureTimeMode: Option[ExposureTimeMode.SignalToNoiseMode] =
//          exposureTimeMode.flatMap(ExposureTimeMode.signalToNoise.getOption)
//
//        val timeAndCountExposureTimeMode: Option[ExposureTimeMode.TimeAndCountMode] =
//          exposureTimeMode.flatMap(ExposureTimeMode.timeAndCount.getOption)

//           exposureTimeModeType                                                                                                    ,
//           exposureTimeMode.map(_.at)                                                                                              ,
//           signalToNoiseExposureTimeMode.map(_.value)                                                                              ,
//           timeAndCountExposureTimeMode.map(_.time)                                                                                ,
//           timeAndCountExposureTimeMode.map(_.count)                                                                               ,

//      Option[ExposureTimeModeType]     ,
//      Option[Wavelength]               ,
//      Option[SignalToNoise]            ,
//      Option[TimeSpan]                 ,
//      Option[PosInt]                   ,

//          c_exp_time_mode,
//          c_etm_signal_to_noise_at,
//          c_etm_signal_to_noise,
//          c_etm_exp_time,
//          c_etm_exp_count,

//          ${exposure_time_mode_type.opt},
//          ${wavelength_pm.opt},
//          ${signal_to_noise.opt},
//          ${time_span.opt},
//          ${int4_pos.opt},

    private def insertObservation(
      programId:           Program.Id,
      groupId:             Option[Group.Id],
      groupIndex:          NonNegShort,
      subtitle:            Option[NonEmptyString],
      existence:           Existence,
      scienceBand:         Option[ScienceBand],
      posAngleConsMode:    PosAngleConstraintMode,
      posAngle:            Angle,
      explicitBase:        Option[Coordinates],
      constraintSet:       ConstraintSet,
      scienceRequirements: Option[ScienceRequirementsInput],
      modeType:            Option[ObservingModeType],
      instrument:          Option[Instrument],
      observerNotes:       Option[NonEmptyString],
      useBlindOffset:      Boolean,
    ): AppliedFragment = {

      val insert: AppliedFragment = {
        val spectroscopy: Option[SpectroscopyScienceRequirementsInput] =
          scienceRequirements.flatMap(_.spectroscopy)

        val imaging: Option[ImagingScienceRequirementsInput] =
          scienceRequirements.flatMap(_.imaging)

        InsertObservation.apply(
          programId                                                                                                                ,
           groupId                                                                                                                 ,
           groupIndex                                                                                                              ,
           subtitle                                                                                                                ,
           existence                                                                                                               ,
           scienceBand                                                                                                             ,
           posAngleConsMode                                                                                                        ,
           posAngle                                                                                                                ,
           explicitBase.map(_.ra)                                                                                                  ,
           explicitBase.map(_.dec)                                                                                                 ,
           constraintSet.cloudExtinction                                                                                           ,
           constraintSet.imageQuality                                                                                              ,
           constraintSet.skyBackground                                                                                             ,
           constraintSet.waterVapor                                                                                                ,
           ElevationRange.airMass.getOption(constraintSet.elevationRange).map(am => PosBigDecimal.unsafeFrom(am.min.toBigDecimal)) , // TODO: fix in core
           ElevationRange.airMass.getOption(constraintSet.elevationRange).map(am => PosBigDecimal.unsafeFrom(am.max.toBigDecimal)) ,
           ElevationRange.hourAngle.getOption(constraintSet.elevationRange).map(_.minHours.toBigDecimal)                           ,
           ElevationRange.hourAngle.getOption(constraintSet.elevationRange).map(_.maxHours.toBigDecimal)                           ,
           spectroscopy.flatMap(_.wavelength.toOption)                                                                             ,
           spectroscopy.flatMap(_.resolution.toOption)                                                                             ,
           spectroscopy.flatMap(_.wavelengthCoverage.toOption)                                                                     ,
           spectroscopy.flatMap(_.focalPlane.toOption)                                                                             ,
           spectroscopy.flatMap(_.focalPlaneAngle.toOption)                                                                        ,
           spectroscopy.flatMap(_.capability.toOption)                                                                             ,
           imaging.flatMap(_.minimumFov.toOption)                                                                                  ,
           imaging.flatMap(_.narrowFilters.toOption)                                                                               ,
           imaging.flatMap(_.broadFilters.toOption)                                                                                ,
           imaging.flatMap(_.combinedFilters.toOption)                                                                             ,
           modeType                                                                                                                ,
           instrument                                                                                                              ,
           observerNotes                                                                                                           ,
           useBlindOffset                                                                                                          ,
        )
      }

      val returning: AppliedFragment =
        void"RETURNING c_observation_id"

      // done!
      insert |+| returning

    }

    val InsertObservation: Fragment[(
      Program.Id                       ,
      Option[Group.Id]                 ,
      NonNegShort                      ,
      Option[NonEmptyString]           ,
      Existence                        ,
      Option[ScienceBand]              ,
      PosAngleConstraintMode           ,
      Angle                            ,
      Option[RightAscension]           ,
      Option[Declination]              ,
      CloudExtinction.Preset           ,
      ImageQuality.Preset              ,
      SkyBackground                    ,
      WaterVapor                       ,
      Option[PosBigDecimal]            ,
      Option[PosBigDecimal]            ,
      Option[BigDecimal]               ,
      Option[BigDecimal]               ,
      Option[Wavelength]               ,
      Option[PosInt]                   ,
      Option[Wavelength]               ,
      Option[FocalPlane]               ,
      Option[Angle]                    ,
      Option[SpectroscopyCapabilities] ,
      Option[Angle]                    ,
      Option[Boolean]                  ,
      Option[Boolean]                  ,
      Option[Boolean]                  ,
      Option[ObservingModeType]        ,
      Option[Instrument]               ,
      Option[NonEmptyString]           ,
      Boolean                          ,
    )] =
      sql"""
        INSERT INTO t_observation (
          c_program_id,
          c_group_id,
          c_group_index,
          c_subtitle,
          c_existence,
          c_science_band,
          c_pac_mode,
          c_pac_angle,
          c_explicit_ra,
          c_explicit_dec,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_air_mass_min,
          c_air_mass_max,
          c_hour_angle_min,
          c_hour_angle_max,
          c_spec_wavelength,
          c_spec_resolution,
          c_spec_wavelength_coverage,
          c_spec_focal_plane,
          c_spec_focal_plane_angle,
          c_spec_capability,
          c_img_minimum_fov,
          c_img_narrow_filters,
          c_img_broad_filters,
          c_img_combined_filters,
          c_observing_mode_type,
          c_instrument,
          c_observer_notes,
          c_use_blind_offset
        )
        SELECT
          $program_id,
          ${group_id.opt},
          $int2_nonneg,
          ${text_nonempty.opt},
          $existence,
          ${science_band.opt},
          $pac_mode,
          $angle_µas,
          ${right_ascension.opt},
          ${declination.opt},
          $cloud_extinction_preset,
          $image_quality_preset,
          $sky_background,
          $water_vapor,
          ${air_mass_range_value.opt},
          ${air_mass_range_value.opt},
          ${hour_angle_range_value.opt},
          ${hour_angle_range_value.opt},
          ${wavelength_pm.opt},
          ${int4_pos.opt},
          ${wavelength_pm.opt},
          ${focal_plane.opt},
          ${angle_µas.opt},
          ${spectroscopy_capabilities.opt},
          ${angle_µas.opt},
          ${bool.opt},
          ${bool.opt},
          ${bool.opt},
          ${observing_mode_type.opt},
          ${instrument.opt},
          ${text_nonempty.opt},
          $bool
      """

    def selectObservingModes(
      observationIds: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"SELECT c_observation_id, c_observing_mode_type FROM t_observation " |+|
        void"WHERE c_observation_id IN ("                                      |+|
          observationIds.map(sql"$observation_id").intercalate(void", ")       |+|
        void")"

    def posAngleConstraintUpdates(in: PosAngleConstraintInput): List[AppliedFragment] = {

      val upMode  = sql"c_pac_mode  = $pac_mode"
      val upAngle = sql"c_pac_angle = $angle_µas"

      in.mode.map(upMode).toList ++ in.angle.map(upAngle).toList
    }

    def explicitBaseUpdates(in: TargetEnvironmentInput.Edit): Result[List[AppliedFragment]] = {

      val upRa  = sql"c_explicit_ra = ${right_ascension.opt}"
      val upDec = sql"c_explicit_dec = ${declination.opt}"

      in.explicitBase match {
        case Nullable.Null   => Result(List(upRa(none), upDec(none)))
        case Nullable.Absent => Result(Nil)
        case NonNull(value)  =>
          value.ra.map(r => upRa(r.some)).toList ++ value.dec.map(d => upDec(d.some)).toList match {
            case Nil => Result.failure("At least one of ra or dec must be specified for an edit")
            case lst => Result(lst)
          }
      }
    }

    def elevationRangeUpdates(in: ElevationRangeInput): Result[List[AppliedFragment]] = {
      val upAirMassMin   = sql"c_air_mass_min = ${air_mass_range_value.opt}"
      val upAirMassMax   = sql"c_air_mass_max = ${air_mass_range_value.opt}"
      val upHourAngleMin = sql"c_hour_angle_min = ${hour_angle_range_value.opt}"
      val upHourAngleMax = sql"c_hour_angle_max = ${hour_angle_range_value.opt}"

      val airMass: List[AppliedFragment] =
        List(
          in.airMass.flatMap(_.minPosBigDecimal).map(v => upAirMassMin(v.some)),
          in.airMass.flatMap(_.maxPosBigDecimal).map(v => upAirMassMax(v.some))
        ).flattenOption

      val hourAngle: List[AppliedFragment] =
        List(
          in.hourAngle.flatMap(_.minBigDecimal).map(v => upHourAngleMin(v.some)),
          in.hourAngle.flatMap(_.maxBigDecimal).map(v => upHourAngleMax(v.some))
        ).flattenOption

      (airMass, hourAngle) match {
        case (Nil, Nil) => Result(List.empty[AppliedFragment])
        case (am, Nil)  => Result(upHourAngleMin(None) :: upHourAngleMax(None) :: am)
        case (Nil, ha)  => Result(upAirMassMin(None) :: upAirMassMax(None) :: ha)
        case (_, _)     => Result.failure("Only one of airMass or hourAngle may be specified.")
      }
    }

    def constraintSetUpdates(in: ConstraintSetInput): Result[List[AppliedFragment]] = {
      val upCloud = sql"c_cloud_extinction = $cloud_extinction_preset"
      val upImage = sql"c_image_quality = $image_quality_preset"
      val upSky   = sql"c_sky_background = $sky_background"
      val upWater = sql"c_water_vapor = $water_vapor"

      val ups: List[AppliedFragment] =
        List(
          in.cloudExtinction.map(upCloud),
          in.imageQuality.map(upImage),
          in.skyBackground.map(upSky),
          in.waterVapor.map(upWater)
        ).flattenOption

      in.elevationRange
        .toList
        .flatTraverse(elevationRangeUpdates)
        .map(_ ++ ups)
    }

    def spectroscopyRequirementsUpdates(in: SpectroscopyScienceRequirementsInput): List[AppliedFragment] = {

      val upWavelength         = sql"c_spec_wavelength = ${wavelength_pm.opt}"
      val upResolution         = sql"c_spec_resolution = ${int4_pos.opt}"
      val upWavelengthCoverage = sql"c_spec_wavelength_coverage = ${wavelength_pm.opt}"
      val upFocalPlane         = sql"c_spec_focal_plane = ${focal_plane.opt}"
      val upFocalPlaneAngle    = sql"c_spec_focal_plane_angle = ${angle_µas.opt}"
      val upCapability         = sql"c_spec_capability = ${spectroscopy_capabilities.opt}"


      List(
        in.wavelength.foldPresent(upWavelength),
        in.resolution.foldPresent(upResolution),
        in.wavelengthCoverage.foldPresent(upWavelengthCoverage),
        in.focalPlane.foldPresent(upFocalPlane),
        in.focalPlaneAngle.foldPresent(upFocalPlaneAngle),
        in.capability.foldPresent(upCapability)
      ).flattenOption
    }

    def imagingRequirementsUpdates(in: ImagingScienceRequirementsInput): List[AppliedFragment] = {

      val upMinimumFov         = sql"c_img_minimum_fov = ${angle_µas.opt}"
      val upNarrowFilters      = sql"c_img_narrow_filters = ${bool.opt}"
      val upBroadFilters       = sql"c_img_broad_filters = ${bool.opt}"
      val upCombinedFilters    = sql"c_img_combined_filters = ${bool.opt}"

      List(
        in.minimumFov.foldPresent(upMinimumFov),
        in.narrowFilters.foldPresent(upNarrowFilters),
        in.broadFilters.foldPresent(upBroadFilters),
        in.combinedFilters.foldPresent(upCombinedFilters)
      ).flattenOption
    }

    def scienceRequirementsUpdates(in: ScienceRequirementsInput): List[AppliedFragment] =

      // we clear fields based on the science mode that's being set
      val clearSpectroscopy = in.imaging.isDefined && in.spectroscopy.isEmpty
      val clearImaging = in.spectroscopy.isDefined && in.imaging.isEmpty

      val spectroscopyClear =
        Option.when(clearSpectroscopy)(List(
          void"c_spec_wavelength = NULL",
          void"c_spec_resolution = NULL",
          void"c_spec_wavelength_coverage = NULL",
          void"c_spec_focal_plane = NULL",
          void"c_spec_focal_plane_angle = NULL",
          void"c_spec_capability = NULL"
        )).orEmpty

      val imagingClear =
        Option.when(clearImaging)(List(
          void"c_img_minimum_fov = NULL",
          void"c_img_narrow_filters = NULL",
          void"c_img_broad_filters = NULL",
          void"c_img_combined_filters = NULL"
        )).orEmpty

      spectroscopyClear ++ imagingClear ++
        in.spectroscopy.toList.flatMap(spectroscopyRequirementsUpdates) ++
        in.imaging.toList.flatMap(imagingRequirementsUpdates)

    def updates(SET: ObservationPropertiesInput.Edit): Result[Option[NonEmptyList[AppliedFragment]]] = {
      val upExistence         = sql"c_existence = $existence"
      val upSubtitle          = sql"c_subtitle = ${text_nonempty.opt}"
      val upScienceBand       = sql"c_science_band = ${science_band.opt}"
      val upObserverNotes     = sql"c_observer_notes = ${text_nonempty.opt}"
      val upUseBlindOffset    = sql"c_use_blind_offset = $bool"

      val ups: List[AppliedFragment] =
        List(
          SET.existence.map(upExistence),
          SET.subtitle.foldPresent(upSubtitle),
          SET.scienceBand.foldPresent(upScienceBand),
          SET.observerNotes.foldPresent(upObserverNotes),
          SET.targetEnvironment.flatMap(_.useBlindOffset).map(upUseBlindOffset)
        ).flatten

      val posAngleConstraint: List[AppliedFragment] =
        SET.posAngleConstraint
           .toList
           .flatMap(posAngleConstraintUpdates)

      val scienceRequirements: List[AppliedFragment] =
        SET.scienceRequirements
           .toList
           .flatMap(scienceRequirementsUpdates)

      val explicitBase: Result[List[AppliedFragment]] =
        SET.targetEnvironment
           .toList
           .flatTraverse(explicitBaseUpdates)

      val constraintSet: Result[List[AppliedFragment]] =
        SET.constraintSet
           .toList
           .flatTraverse(constraintSetUpdates)

      (explicitBase, constraintSet).mapN { (eb, cs) =>
        NonEmptyList.fromList(eb ++ cs ++ ups ++ posAngleConstraint ++ scienceRequirements)
      }
    }

    def updateObservations(
      SET:   ObservationPropertiesInput.Edit,
      which: AppliedFragment
    ): Result[AppliedFragment] = {

      def update(us: NonEmptyList[AppliedFragment]): AppliedFragment =
        void"UPDATE t_observation "                                              |+|
          void"SET " |+| us.intercalate(void", ") |+| void" "                    |+|
          void"WHERE t_observation.c_observation_id IN (" |+| which |+| void") " |+|
          void"RETURNING t_observation.c_program_id, t_observation.c_observation_id"

      def selectOnly: AppliedFragment =
        void"SELECT o.c_program_id, o.c_observation_id "             |+|
          void"FROM t_observation o "                                |+|
          void"WHERE o.c_observation_id IN (" |+| which |+| void")"

      updates(SET).map(_.fold(selectOnly)(update))
    }

    def updateObsTime(
      SET:   ObservationTimesInput,
      which: AppliedFragment
    ): Result[AppliedFragment] = {
      val updates: List[AppliedFragment] =
        List(
          SET.observationTime.foldPresent(ts => sql"c_observation_time = ${core_timestamp.opt}"(ts)),
          SET.observationDuration.foldPresent(ts => sql"c_observation_duration = ${time_span.opt}"(ts)),
        ).flatten

      def update(us: NonEmptyList[AppliedFragment]): AppliedFragment =
        void"UPDATE t_observation "                                  |+|
          void"SET " |+| us.intercalate(void", ") |+| void" "        |+|
          void"WHERE c_observation_id IN (" |+| which |+| void")"    |+|
          void"RETURNING t_observation.c_program_id, t_observation.c_observation_id"

      def selectOnly: AppliedFragment =
        void"SELECT o.c_program_id, o.c_observation_id "             |+|
          void"FROM t_observation o "                                |+|
          void"WHERE o.c_observation_id IN (" |+| which |+| void")"

      Result(NonEmptyList.fromList(updates).fold(selectOnly)(update))
    }

    def updateObservingModeType(
      newMode: Option[ObservingModeType],
      which:   NonEmptyList[Observation.Id]
    ): AppliedFragment =
      void"UPDATE t_observation " |+|
         void"SET " |+|
            sql"c_observing_mode_type = ${observing_mode_type.opt}"(newMode) |+| void", " |+|
            sql"c_instrument = ${instrument.opt}"(newMode.map(_.instrument)) |+| void" " |+|
       void"WHERE c_observation_id IN (" |+| which.map(sql"${observation_id}").intercalate(void", ") |+| void")"

    /**
     * Clone the base slice (just t_observation) and return the new obs id, or none if the original
     * doesn't exist or isn't accessible.
     */
    def cloneObservation(oid: Observation.Id, gix: NonNegShort): AppliedFragment =
      sql"""
        INSERT INTO t_observation (
          c_program_id,
          c_group_id,
          c_group_index,
          c_title,
          c_subtitle,
          c_instrument,
          c_science_band,
          c_observation_time,
          c_pts_pi,
          c_pts_uncharged,
          c_pts_execution,
          c_pac_mode,
          c_pac_angle,
          c_explicit_ra,
          c_explicit_dec,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_air_mass_min,
          c_air_mass_max,
          c_hour_angle_min,
          c_hour_angle_max,
          c_spec_wavelength,
          c_spec_resolution,
          c_spec_wavelength_coverage,
          c_spec_focal_plane,
          c_spec_focal_plane_angle,
          c_spec_capability,
          c_observing_mode_type,
          c_img_minimum_fov,
          c_img_narrow_filters,
          c_img_broad_filters,
          c_img_combined_filters,
          c_observer_notes,
          c_use_blind_offset,
          c_blind_offset_type
        )
        SELECT
          c_program_id,
          c_group_id,
          $int2_nonneg,
          c_title,
          c_subtitle,
          c_instrument,
          c_science_band,
          c_observation_time,
          c_pts_pi,
          c_pts_uncharged,
          c_pts_execution,
          c_pac_mode,
          c_pac_angle,
          c_explicit_ra,
          c_explicit_dec,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_air_mass_min,
          c_air_mass_max,
          c_hour_angle_min,
          c_hour_angle_max,
          c_spec_wavelength,
          c_spec_resolution,
          c_spec_wavelength_coverage,
          c_spec_focal_plane,
          c_spec_focal_plane_angle,
          c_spec_capability,
          c_observing_mode_type,
          c_img_minimum_fov,
          c_img_narrow_filters,
          c_img_broad_filters,
          c_img_combined_filters,
          c_observer_notes,
          c_use_blind_offset,
          c_blind_offset_type
      FROM t_observation
      WHERE c_observation_id = $observation_id
      RETURNING c_observation_id
      """.apply(gix, oid)

    def moveObservations(gid: Option[Group.Id], index: Option[NonNegShort], which: AppliedFragment): AppliedFragment =
      sql"""
        SELECT group_move_observation(c_observation_id, ${group_id.opt}, ${int2_nonneg.opt})
        FROM t_observation
        WHERE c_observation_id IN (
      """.apply(gid, index) |+| which |+| void")"

    // Brute force statements to delete a calibration observations
    def linkedTargets(oids: NonEmptyList[Observation.Id]): Query[List[Observation.Id], Target.Id] =
      (sql"""
        SELECT
          c_target_id
        FROM t_asterism_target
        WHERE c_observation_id IN(${observation_id.list(oids.size)})""")
        .query(target_id)

    def deleteLinkedAsterisms(tids: NonEmptyList[Target.Id]): AppliedFragment =
      void"DELETE FROM t_asterism_target " |+|
        void"WHERE c_target_id IN (" |+|
          tids.map(sql"$target_id").intercalate(void", ") |+| void")"

    def deleteAsterismsForObservations(oids: NonEmptyList[Observation.Id]): AppliedFragment =
      void"DELETE FROM t_asterism_target " |+|
        void"WHERE c_observation_id IN (" |+|
          oids.map(sql"$observation_id").intercalate(void", ") |+| void")"

    def deleteTargets(tids: NonEmptyList[Target.Id]): AppliedFragment =
      void"DELETE FROM t_target " |+|
        void"WHERE c_target_id IN (" |+|
          tids.map(sql"$target_id").intercalate(void", ") |+| void")"

    def deleteCalibrationObservations(oids: NonEmptyList[Observation.Id]): AppliedFragment =
      void"DELETE FROM t_observation " |+|
        void"WHERE c_observation_id IN (" |+|
          oids.map(sql"$observation_id").intercalate(void", ") |+| void")"

    val ResetAcquisition: Command[Observation.Id] =
      sql"""
        UPDATE t_observation
           SET c_acq_reset_time = now()
         WHERE c_observation_id = $observation_id
      """.command

    val SelectBands: Query[Program.Id, (Observation.Id, Option[ScienceBand])] =
      sql"""
        SELECT
          c_observation_id,
          c_science_band
        FROM t_observation
        WHERE c_program_id = $program_id AND c_existence = 'present'
      """.query(observation_id *: science_band.opt)
  }

}
