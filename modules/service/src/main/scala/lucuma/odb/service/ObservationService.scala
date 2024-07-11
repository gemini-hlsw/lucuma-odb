// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.api.Refined.value
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ScienceMode
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.CallForProposals
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Group
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservingNight
import lucuma.core.model.Program
import lucuma.core.model.StandardRole.*
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.itc.client.ItcClient
import lucuma.odb.data.DateInterval
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.Absent
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.data.ObservingModeType
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.data.ScienceBand
import lucuma.odb.graphql.given
import lucuma.odb.graphql.input.CloneObservationInput
import lucuma.odb.graphql.input.ConstraintSetInput
import lucuma.odb.graphql.input.CreateObservationInput
import lucuma.odb.graphql.input.ElevationRangeInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.ObservingModeInput
import lucuma.odb.graphql.input.PosAngleConstraintInput
import lucuma.odb.graphql.input.ScienceRequirementsInput
import lucuma.odb.graphql.input.SpectroscopyScienceRequirementsInput
import lucuma.odb.graphql.input.TargetEnvironmentInput
import lucuma.odb.graphql.input.TimingWindowInput
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.service.GeneratorParamsService.Error as GenParamsError
import lucuma.odb.syntax.instrument.*
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Codecs.group_id
import lucuma.odb.util.Codecs.int2_nonneg
import natchez.Trace
import skunk.*
import skunk.exception.PostgresErrorException
import skunk.implicits.*

import java.time.Duration

import Services.Syntax.*

sealed trait ObservationService[F[_]] {

  /**
   * Finds the observation id consistent with the given ids (if any).
   */
  def resolveOid(
    oid: Option[Observation.Id],
    ref: Option[ObservationReference]
  ): F[Result[Observation.Id]]

  def createObservation(
    input: CreateObservationInput
  )(using Transaction[F]): F[Result[Observation.Id]]

  def selectObservations(
    which: AppliedFragment
  )(using Transaction[F]): F[List[Observation.Id]]

  def selectObservingModes(
    which: List[Observation.Id]
  )(using Transaction[F]): F[Map[Option[ObservingModeType], List[Observation.Id]]]

  def updateObservations(
    SET:   ObservationPropertiesInput.Edit,
    which: AppliedFragment
  )(using Transaction[F]): F[Result[Map[Program.Id, List[Observation.Id]]]]

  def setScienceBand(
    pid:  Program.Id,
    band: ScienceBand
  )(using Transaction[F], Services.StaffAccess): F[List[Observation.Id]]

  def cloneObservation(
    input: CloneObservationInput
  )(using Transaction[F]): F[Result[ObservationService.CloneIds]]

  def observationValidations(
    pid: Program.Id,
    oid: Observation.Id,
    itcClient: ItcClient[F],
  )(using Transaction[F]): F[List[ObservationValidation]]
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
      BothExplicitCoordinatesConstraint
    )

  def constraintViolationMessage(ex: PostgresErrorException): String =
    DatabaseConstraints
      .find { dc => ex.message.contains(dc.constraint) }
      .map(_.message)
      .getOrElse(GenericConstraintViolationMessage(ex.message))

  // observation validation messages
  val AsterismOutOfRangeMsg = "Asterism out of Call for Proposals limits."
  val ExplicitBaseOutOfRangeMsg = "Explicit base out of Call for Proposals limits."
  def InvalidInstrumentMsg(instr: Instrument) = s"Instrument $instr not part of Call for Proposals."
  def MissingDataMsg(otid: Option[Target.Id], paramName: String) =
    otid.fold(s"Missing $paramName")(tid => s"Missing $paramName for target $tid")

  case class CloneIds(
    originalId: Observation.Id,
    cloneId:    Observation.Id
  )

  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ObservationService[F] =
    new ObservationService[F] {

      val resolver = new IdResolver("observation", Statements.selectOid, _.label)

      override def resolveOid(
        oid: Option[Observation.Id],
        ref: Option[ObservationReference]
      ): F[Result[Observation.Id]] =
        resolver.resolve(oid, ref)

      private def setTimingWindows(
        oids:          List[Observation.Id],
        timingWindows: Option[List[TimingWindowInput]],
      )(using Transaction[F]): F[Result[Unit]] =
        timingWindows
          .traverse(timingWindowService.createFunction)
          .map { optF =>
            optF.fold(().pure[F])( f => f(oids, transaction) )
          }.sequence

      /** Create the observation itself, with no asterism. */
      private def createObservationImpl(
        programId: Program.Id,
        SET:       ObservationPropertiesInput.Create
      )(using Transaction[F]): F[Result[Observation.Id]] =
        Trace[F].span("createObservation") {
          session.execute(sql"set constraints all deferred".command) >>
          session.prepareR(GroupService.Statements.OpenHole).use(_.unique(programId, SET.group, SET.groupIndex)).flatMap { ix =>
            Statements
              .insertObservationAs(user, programId, SET, ix)
              .flatTraverse { af =>
                session.prepareR(af.fragment.query(observation_id)).use { pq =>
                  pq.option(af.argument).map {
                    case Some(oid) => Result(oid)
                    case None      => OdbError.NotAuthorized(user.id).asFailure
                  }
                }.flatMap { rOid =>

                  val rOptF = SET.observingMode.traverse(observingModeServices.createFunction)
                  (rOid, rOptF).parMapN { (oid, optF) =>
                    optF.fold(oid.pure[F]) { f => f(List(oid), transaction).as(oid) }
                  }.sequence

                }
              }.flatTap { rOid =>
                rOid.flatTraverse { oid => setTimingWindows(List(oid), SET.timingWindows) }
              }.flatMap { rOid =>
                SET.obsAttachments.fold(rOid.pure[F]) { aids =>
                  rOid.flatTraverse { oid =>
                    obsAttachmentAssignmentService
                      .insertAssignments(programId, List(oid), aids)
                      .map(_.map(_ => oid))
                  }
                }
              }
          }
        }

      override def createObservation(
        input: CreateObservationInput
      )(using Transaction[F]): F[Result[Observation.Id]] = {

        def create(pid: Program.Id): F[Result[Observation.Id]] =
          createObservationImpl(pid, input.SET.getOrElse(ObservationPropertiesInput.Create.Default))

        def insertAsterism(pid: Program.Id, oid: Observation.Id): F[Result[Unit]] =
          input.asterism.toOption.traverse { a =>
            asterismService.insertAsterism(pid, NonEmptyList.one(oid), a)
          }.map(_.getOrElse(Result.unit))

        val go =
          for
            pid <- ResultT(programService.resolvePid(input.programId, input.proposalReference, input.programReference))
            oid <- ResultT(create(pid))
            _   <- ResultT(insertAsterism(pid, oid))
          yield oid

        go.value.flatTap: r =>
          transaction.rollback.unlessA(r.hasValue)

      }

      override def selectObservations(
        which: AppliedFragment
      )(using Transaction[F]): F[List[Observation.Id]] =
        session.prepareR(which.fragment.query(observation_id)).use { pq =>
          pq.stream(which.argument, chunkSize = 1024).compile.toList
        }

      override def selectObservingModes(
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
        which:   List[Observation.Id]
      ): F[Unit] = {
        val af = Statements.updateObservingModeType(newMode, which)
        session.prepareR(af.fragment.command).use { pq =>
          pq.execute(af.argument).void
        }
      }

      private def updateObservingModes(
        nEdit: Nullable[ObservingModeInput.Edit],
        oids:  List[Observation.Id],
      )(using Transaction[F]): F[Result[Unit]] =

        nEdit.toOptionOption.fold(Result.unit.pure[F]) { oEdit =>
          for {
            m <- selectObservingModes(oids)
            _ <- updateObservingModeType(oEdit.flatMap(_.observingModeType), oids)
            r <- m.toList.traverse { case (existingMode, matchingOids) =>
              (existingMode, oEdit) match {
                case (Some(ex), Some(edit)) if edit.observingModeType.contains(ex) =>
                  // update existing
                  observingModeServices.updateFunction(edit).traverse(f => f(matchingOids, transaction))

                case (Some(ex), Some(edit)) =>
                  for {
                    // delete existing
                    _ <- observingModeServices.deleteFunction(ex)(matchingOids, transaction)

                    // create new
                    r <- observingModeServices.createViaUpdateFunction(edit).traverse(f => f(matchingOids, transaction))
                  } yield r

                case (None,    Some(edit)) =>
                  // create new
                  observingModeServices.createViaUpdateFunction(edit).traverse(f => f(matchingOids, transaction))

                case (Some(ex), None) =>
                  // delete existing
                  observingModeServices.deleteFunction(ex)(matchingOids, transaction).as(Result.unit)

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
        SET:   ObservationPropertiesInput.Edit,
        which: AppliedFragment
      )(using Transaction[F]): F[Result[Map[Program.Id, List[Observation.Id]]]] =
        Trace[F].span("updateObservation") {
          val updates: ResultT[F, Map[Program.Id, List[Observation.Id]]] =
            for {
              r <- ResultT(Statements.updateObservations(SET, which).traverse { af =>
                      session.prepareR(af.fragment.query(program_id *: observation_id)).use { pq =>
                        pq.stream(af.argument, chunkSize = 1024).compile.toList
                      }
                   })
              g  = r.groupMap(_._1)(_._2)                 // grouped:   Map[Program.Id, List[Observation.Id]]
              u  = g.values.reduceOption(_ ++ _).orEmpty  // ungrouped: List[Observation.Id]
              _ <- ResultT(updateObservingModes(SET.observingMode, u))
              _ <- ResultT(setTimingWindows(u, SET.timingWindows.foldPresent(_.orEmpty)))
              _ <- ResultT(g.toList.traverse { case (pid, oids) =>
                     obsAttachmentAssignmentService.setAssignments(pid, oids, SET.obsAttachments)
                   }.map(_.sequence))
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

      override def setScienceBand(
        pid:  Program.Id,
        band: ScienceBand
      )(using Transaction[F], Services.StaffAccess): F[List[Observation.Id]] =
        Trace[F].span("setScienceBand") {
          session.execute(Statements.SetScienceBand)(pid, band)
        }

      private def cloneObservationImpl(
        observationId: Observation.Id,
        SET:           Option[ObservationPropertiesInput.Edit]
      )(using Transaction[F]): F[Result[(Program.Id, Observation.Id)]] = {

        // First we need the pid, observing mode, and grouping information
        val selPid = sql"select c_program_id, c_observing_mode_type, c_group_id, c_group_index from t_observation where c_observation_id = $observation_id"
        session.prepareR(selPid.query(program_id *: observing_mode_type.opt *: group_id.opt *: int2_nonneg)).use(_.option(observationId)).flatMap {

          case None => Result.failure(s"No such observation: $observationId").pure[F]

          case Some((pid, observingMode, gid, gix)) =>

            // Desired group index is gix + 1
            val destGroupIndex = NonNegShort.unsafeFrom((gix.value + 1).toShort)

            // Ok the obs exists, so let's clone its main row in t_observation. If this returns
            // None then it means the user doesn't have permission to see the obs.
            val cObsStmt = Statements.cloneObservation(pid, observationId, user, destGroupIndex)
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
                  asterismService.cloneAsterism(observationId, oid2) >>
                  observingMode.traverse(observingModeServices.cloneFunction(_)(observationId, oid2)) >>
                  timingWindowService.cloneTimingWindows(observationId, oid2) >>
                  obsAttachmentAssignmentService.cloneAssignments(observationId, oid2)

                val doUpdate =
                  SET match
                    case None    => Result((pid, oid2)).pure[F] // nothing to do
                    case Some(s) =>
                      updateObservations(s, sql"select $observation_id".apply(oid2))
                        .map { r =>
                          // We probably don't need to check this return value, but I feel bad not doing it.
                          r.map(_.toList).flatMap {
                            case List((`pid`, List(`oid2`))) => Result((pid, oid2))
                            case other                       => Result.failure(s"Observation update: expected ($pid, [$oid2]), found ${other.mkString("[", ",", "]")}")
                          }
                        }
                        .flatTap {
                          r => transaction.rollback.unlessA(r.hasValue)
                        }

                cloneRelatedItems >> doUpdate

            }
        }
      }

      def cloneObservation(
        input: CloneObservationInput
      )(using Transaction[F]): F[Result[CloneIds]] =
        (for
          origOid       <- ResultT(resolveOid(input.observationId, input.observationRef))
          (pid, newOid) <- ResultT(cloneObservationImpl(origOid, input.SET))
          _             <- ResultT(asterismService.setAsterism(pid, NonEmptyList.of(newOid), input.asterism))
        yield CloneIds(origOid, newOid)).value

      // ***********************
      // All about the validations
      // ***********************
      def cfpError(msg: String): ObservationValidation =
        ObservationValidation.callForProposals(msg)

      extension (ra: RightAscension)
        def isInInterval(raStart: RightAscension, raEnd: RightAscension): Boolean =
          if (raStart > raEnd) raStart <= ra || ra <= raEnd
          else raStart <= ra && ra <= raEnd

      extension (dec: Declination)
        def isInInterval(decStart: Declination, decEnd: Declination): Boolean =
          decStart <= dec && dec <= decEnd

      extension (ge: GeneratorParamsService.Error)
        def toObsValidation: ObservationValidation = ge match
          case GenParamsError.MissingData(otid, paramName) => ObservationValidation.configuration(MissingDataMsg(otid, paramName))
          case _                                           => ObservationValidation.configuration(ge.format)

      override def observationValidations(
        pid: Program.Id,
        oid: Observation.Id,
        itcClient: ItcClient[F]
      )(using Transaction[F]): F[List[ObservationValidation]] = {
        
        val generatorValidations: F[(ObservationValidationMap, Option[GeneratorParams])] =
          generatorParamsService.selectOne(pid, oid).map {
            case Right(ps) => (ObservationValidationMap.empty, ps.some)
            case Left(errors) => (ObservationValidationMap.fromList(errors.map(_.toObsValidation).toList), none)
          }

        val optCfpId: F[Option[CallForProposals.Id]] =
          session.option(Statements.ProgramCfpId)(pid).map(_.flatten)

        def validateAsterismRaDec(
          raStart: RightAscension,
          raEnd: RightAscension,
          decStart: Declination,
          decEnd: Declination,
          site: Site,
          active: DateInterval
        ): F[Option[ObservationValidation]] =
          asterismService.getAsterism(pid,oid)
            .map { l =>
              val targets = l.map(_._2)
              // The lack of a target will get reported in the generator errors
              val start    = ObservingNight.fromSiteAndLocalDate(site, active.start).start
              val end      = ObservingNight.fromSiteAndLocalDate(site, active.end).end
              val duration = Duration.between(start, end)
              val center   = start.plus(duration.dividedBy(2L))

              (for {
                asterism <- NonEmptyList.fromList(targets)
                tracking  = ObjectTracking.fromAsterism(asterism)
                coordsAt <- tracking.at(center)
                coords    = coordsAt.value
              } yield {
                if (coords.ra.isInInterval(raStart, raEnd) && coords.dec.isInInterval(decStart, decEnd)) none
                else cfpError(AsterismOutOfRangeMsg).some
              }).flatten
            }

        def validateRaDec(
          cid: CallForProposals.Id,
          inst: Option[Instrument],
          explicitBase: Option[(RightAscension, Declination)]
        ): F[Option[ObservationValidation]] =
          (for {
            site <- OptionT.fromOption(inst.map(_.site.toList).collect {
              case List(Site.GN) => Site.GN
              case List(Site.GS) => Site.GS
            })
            (raStart, raEnd, decStart, decEnd, active) <- OptionT.liftF(session.unique(Statements.cfpInformation(site))(cid))
            // if the observation has explicit base declared, use that
            validation <- explicitBase.fold(OptionT(validateAsterismRaDec(raStart, raEnd, decStart, decEnd, site, active))) { (ra, dec) =>
              OptionT.fromOption(Option.unless(ra.isInInterval(raStart, raEnd) && dec.isInInterval(decStart, decEnd))(cfpError(ExplicitBaseOutOfRangeMsg)))
            }
          } yield validation).value

        def validateInstrument(cid: CallForProposals.Id, optInstr: Option[Instrument]): F[Option[ObservationValidation]] = {
          // If there is no instrument in the observation, that will get caught with the generatorValidations
          optInstr.fold(none.pure){ instr =>
            session.stream(Statements.CfpInstruments)(cid, chunkSize = 1024)
              .compile
              .toList
              .map(l =>
                if(l.isEmpty || l.contains(instr)) none
                else cfpError(InvalidInstrumentMsg(instr)).some
              )
          }
        }

        val obsInfo: F[(Option[Instrument], Option[RightAscension], Option[Declination])] =
          session.unique(Statements.ObservationValidationInfo)(oid)

        val cfpValidations: F[ObservationValidationMap] = {
          optCfpId.flatMap(
            _.fold(ObservationValidationMap.empty.pure){ cid =>
              for {
                (oinstr, ora, odec) <- obsInfo
                valInstr            <- validateInstrument(cid, oinstr)
                explicitBase     = (ora, odec).tupled
                valRaDec            <- validateRaDec(cid, oinstr, explicitBase)
              } yield ObservationValidationMap.fromList(List(valInstr, valRaDec).flatten)
             }
          )
        }

        def itcValidations(params: GeneratorParams): F[ObservationValidationMap] =
          itcService(itcClient).selectOne(pid, oid, params).map:
            // N.B. there will soon be more cases here
            case Some(_) => ObservationValidationMap.empty
            case None => ObservationValidationMap.empty.add(ObservationValidation.itc("ITC results are not present."))

        for {
          (genVals, op) <- generatorValidations
          cfpVals       <- cfpValidations
          itcVals       <- op.filter(_ => cfpVals.isEmpty).foldMapM(itcValidations) // only compute this if cfp and gen are ok
        } yield (genVals |+| itcVals |+| cfpVals).toList
      }
    }

  object Statements {

    val selectOid: Query[ObservationReference, Observation.Id] =
      sql"""
        SELECT c_observation_id
          FROM t_observation
         WHERE c_observation_reference = $observation_reference
      """.query(observation_id)

    import ProgramService.Statements.existsUserAccess
    import ProgramService.Statements.whereUserAccess

    def insertObservationAs(
      user:      User,
      programId: Program.Id,
      SET:       ObservationPropertiesInput.Create,
      groupIndex: NonNegShort,
    ): Result[AppliedFragment] =
      SET.constraintSet.traverse(_.create).map { cs =>
        insertObservationAs(
          user,
          programId,
          SET.group,
          groupIndex,
          SET.subtitle,
          SET.existence.getOrElse(Existence.Default),
          SET.status.getOrElse(ObsStatus.New),
          SET.activeStatus.getOrElse(ObsActiveStatus.Active),
          SET.scienceBand,
          SET.visualizationTime,
          SET.posAngleConstraint.flatMap(_.mode).getOrElse(PosAngleConstraintMode.Unbounded),
          SET.posAngleConstraint.flatMap(_.angle).getOrElse(Angle.Angle0),
          SET.targetEnvironment.flatMap(_.explicitBase),
          cs.getOrElse(ConstraintSetInput.NominalConstraints),
          SET.scienceRequirements,
          SET.observingMode.flatMap(_.observingModeType),
          SET.observingMode.flatMap(_.observingModeType).map(_.instrument),
          SET.observerNotes
        )
      }

    def insertObservationAs(
      user:                User,
      programId:           Program.Id,
      groupId:             Option[Group.Id],
      groupIndex:          NonNegShort,
      subtitle:            Option[NonEmptyString],
      existence:           Existence,
      status:              ObsStatus,
      activeState:         ObsActiveStatus,
      scienceBand:         Option[ScienceBand],
      visualizationTime:   Option[Timestamp],
      posAngleConsMode:    PosAngleConstraintMode,
      posAngle:            Angle,
      explicitBase:        Option[Coordinates],
      constraintSet:       ConstraintSet,
      scienceRequirements: Option[ScienceRequirementsInput],
      modeType:            Option[ObservingModeType],
      instrument:          Option[Instrument],
      observerNotes:       Option[NonEmptyString]
    ): AppliedFragment = {

      val insert: AppliedFragment = {
        val spectroscopy: Option[SpectroscopyScienceRequirementsInput] =
          scienceRequirements.flatMap(_.spectroscopy)

        InsertObservation.apply(
          programId    ,
           groupId     ,
           groupIndex  ,
           subtitle    ,
           existence   ,
           status      ,
           activeState ,
           scienceBand ,
           visualizationTime             ,
           posAngleConsMode              ,
           posAngle                      ,
           explicitBase.map(_.ra)        ,
           explicitBase.map(_.dec)       ,
           constraintSet.cloudExtinction ,
           constraintSet.imageQuality    ,
           constraintSet.skyBackground   ,
           constraintSet.waterVapor      ,
           ElevationRange.airMass.getOption(constraintSet.elevationRange).map(am => PosBigDecimal.unsafeFrom(am.min.value)) ,  // TODO: fix in core
           ElevationRange.airMass.getOption(constraintSet.elevationRange).map(am => PosBigDecimal.unsafeFrom(am.max.value)) ,
           ElevationRange.hourAngle.getOption(constraintSet.elevationRange).map(_.minHours.value)                           ,
           ElevationRange.hourAngle.getOption(constraintSet.elevationRange).map(_.maxHours.value)                           ,
           scienceRequirements.flatMap(_.mode).getOrElse(ScienceMode.Spectroscopy)  ,
           spectroscopy.flatMap(_.wavelength.toOption)                              ,
           spectroscopy.flatMap(_.resolution.toOption)                              ,
           spectroscopy.flatMap(_.signalToNoise.toOption)                           ,
           spectroscopy.flatMap(_.signalToNoiseAt.toOption)                         ,
           spectroscopy.flatMap(_.wavelengthCoverage.toOption)                      ,
           spectroscopy.flatMap(_.focalPlane.toOption)                              ,
           spectroscopy.flatMap(_.focalPlaneAngle.toOption)                         ,
           spectroscopy.flatMap(_.capability.toOption)                              ,
           modeType                                                                 ,
           instrument                                                               ,
           observerNotes
        )
      }

      val returning: AppliedFragment =
        void"RETURNING c_observation_id"

      // done!
      insert |+| whereUserAccess(user, programId) |+| returning

    }

    val InsertObservation: Fragment[(
      Program.Id                       ,
      Option[Group.Id]                 ,
      NonNegShort                      ,
      Option[NonEmptyString]           ,
      Existence                        ,
      ObsStatus                        ,
      ObsActiveStatus                  ,
      Option[ScienceBand]              ,
      Option[Timestamp]                ,
      PosAngleConstraintMode           ,
      Angle                            ,
      Option[RightAscension]           ,
      Option[Declination]              ,
      CloudExtinction                  ,
      ImageQuality                     ,
      SkyBackground                    ,
      WaterVapor                       ,
      Option[PosBigDecimal]            ,
      Option[PosBigDecimal]            ,
      Option[BigDecimal]               ,
      Option[BigDecimal]               ,
      ScienceMode                      ,
      Option[Wavelength]               ,
      Option[PosInt]                   ,
      Option[SignalToNoise]            ,
      Option[Wavelength]               ,
      Option[Wavelength]               ,
      Option[FocalPlane]               ,
      Option[Angle]                    ,
      Option[SpectroscopyCapabilities] ,
      Option[ObservingModeType]        ,
      Option[Instrument]               ,
      Option[NonEmptyString]
    )] =
      sql"""
        INSERT INTO t_observation (
          c_program_id,
          c_group_id,
          c_group_index,
          c_subtitle,
          c_existence,
          c_status,
          c_active_status,
          c_science_band,
          c_visualization_time,
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
          c_science_mode,
          c_spec_wavelength,
          c_spec_resolution,
          c_spec_signal_to_noise,
          c_spec_signal_to_noise_at,
          c_spec_wavelength_coverage,
          c_spec_focal_plane,
          c_spec_focal_plane_angle,
          c_spec_capability,
          c_observing_mode_type,
          c_instrument,
          c_observer_notes
        )
        SELECT
          $program_id,
          ${group_id.opt},
          $int2_nonneg,
          ${text_nonempty.opt},
          $existence,
          $obs_status,
          $obs_active_status,
          ${science_band.opt},
          ${core_timestamp.opt},
          $pac_mode,
          $angle_µas,
          ${right_ascension.opt},
          ${declination.opt},
          $cloud_extinction,
          $image_quality,
          $sky_background,
          $water_vapor,
          ${air_mass_range_value.opt},
          ${air_mass_range_value.opt},
          ${hour_angle_range_value.opt},
          ${hour_angle_range_value.opt},
          $science_mode,
          ${wavelength_pm.opt},
          ${int4_pos.opt},
          ${signal_to_noise.opt},
          ${wavelength_pm.opt},
          ${wavelength_pm.opt},
          ${focal_plane.opt},
          ${angle_µas.opt},
          ${spectroscopy_capabilities.opt},
          ${observing_mode_type.opt},
          ${instrument.opt},
          ${text_nonempty.opt}
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
      val upCloud = sql"c_cloud_extinction = $cloud_extinction"
      val upImage = sql"c_image_quality = $image_quality"
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
      val upSignalToNoise      = sql"c_spec_signal_to_noise = ${signal_to_noise.opt}"
      val upSignalToNoiseAt    = sql"c_spec_signal_to_noise_at = ${wavelength_pm.opt}"
      val upWavelengthCoverage = sql"c_spec_wavelength_coverage = ${wavelength_pm.opt}"
      val upFocalPlane         = sql"c_spec_focal_plane = ${focal_plane.opt}"
      val upFocalPlaneAngle    = sql"c_spec_focal_plane_angle = ${angle_µas.opt}"
      val upCapability         = sql"c_spec_capability = ${spectroscopy_capabilities.opt}"

      List(
        in.wavelength.foldPresent(upWavelength),
        in.resolution.foldPresent(upResolution),
        in.signalToNoise.foldPresent(upSignalToNoise),
        in.signalToNoiseAt.foldPresent(upSignalToNoiseAt),
        in.wavelengthCoverage.foldPresent(upWavelengthCoverage),
        in.focalPlane.foldPresent(upFocalPlane),
        in.focalPlaneAngle.foldPresent(upFocalPlaneAngle),
        in.capability.foldPresent(upCapability)
      ).flattenOption
    }

    def scienceRequirementsUpdates(in: ScienceRequirementsInput): List[AppliedFragment] = {
      val upMode = sql"c_science_mode = $science_mode"
      val ups    = in.mode.map(upMode).toList

      ups ++ in.spectroscopy.toList.flatMap(spectroscopyRequirementsUpdates)

    }

    def updates(SET: ObservationPropertiesInput.Edit): Result[Option[NonEmptyList[AppliedFragment]]] = {
      val upExistence         = sql"c_existence = $existence"
      val upSubtitle          = sql"c_subtitle = ${text_nonempty.opt}"
      val upStatus            = sql"c_status = $obs_status"
      val upActive            = sql"c_active_status = $obs_active_status"
      val upScienceBand       = sql"c_science_band = ${science_band.opt}"
      val upVisualizationTime = sql"c_visualization_time = ${core_timestamp.opt}"
      val upObserverNotes     = sql"c_observer_notes = ${text_nonempty.opt}"

      val ups: List[AppliedFragment] =
        List(
          SET.existence.map(upExistence),
          SET.subtitle.foldPresent(upSubtitle),
          SET.status.map(upStatus),
          SET.activeStatus.map(upActive),
          SET.scienceBand.foldPresent(upScienceBand),
          SET.observerNotes.foldPresent(upObserverNotes),
          SET.visualizationTime.foldPresent(upVisualizationTime),
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

    def updateObservingModeType(
      newMode: Option[ObservingModeType],
      which:   List[Observation.Id]
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
    def cloneObservation(pid: Program.Id, oid: Observation.Id, user: User, gix: NonNegShort): AppliedFragment =
      sql"""
        INSERT INTO t_observation (
          c_program_id,
          c_group_id,
          c_group_index,
          c_title,
          c_subtitle,
          c_instrument,
          c_status,
          c_active_status,
          c_science_band,
          c_visualization_time,
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
          c_science_mode,
          c_spec_wavelength,
          c_spec_resolution,
          c_spec_signal_to_noise,
          c_spec_signal_to_noise_at,
          c_spec_wavelength_coverage,
          c_spec_focal_plane,
          c_spec_focal_plane_angle,
          c_spec_capability,
          c_observing_mode_type
        )
        SELECT
          c_program_id,
          c_group_id,
          $int2_nonneg,
          c_title,
          c_subtitle,
          c_instrument,
          'new',
          'active',
          c_science_band,
          c_visualization_time,
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
          c_science_mode,
          c_spec_wavelength,
          c_spec_resolution,
          c_spec_signal_to_noise,
          c_spec_signal_to_noise_at,
          c_spec_wavelength_coverage,
          c_spec_focal_plane,
          c_spec_focal_plane_angle,
          c_spec_capability,
          c_observing_mode_type
      FROM t_observation
      WHERE c_observation_id = $observation_id
      """.apply(gix, oid) |+|
      ProgramService.Statements.existsUserAccess(user, pid).foldMap(void"AND " |+| _) |+|
      void"""
        RETURNING c_observation_id
      """

    val SetScienceBand: Query[(Program.Id, ScienceBand), Observation.Id] =
      sql"""
        UPDATE t_observation
        SET c_science_band = $science_band
        WHERE c_program_id = $program_id
        RETURNING c_observation_id
      """.query(observation_id)
         .contramap { (s, p) => (p, s) }

    def moveObservations(gid: Option[Group.Id], index: Option[NonNegShort], which: AppliedFragment): AppliedFragment =
      sql"""
        SELECT group_move_observation(c_observation_id, ${group_id.opt}, ${int2_nonneg.opt})
        FROM t_observation
        WHERE c_observation_id IN (
      """.apply(gid, index) |+| which |+| void")"

    val ProgramCfpId: Query[Program.Id, Option[CallForProposals.Id]] =
      sql"""
        SELECT c_cfp_id
        FROM t_proposal
        WHERE c_program_id = $program_id
      """.query(cfp_id.opt)

    val ObservationValidationInfo:
      Query[Observation.Id, (Option[Instrument], Option[RightAscension], Option[Declination])] =
      sql"""
        SELECT
          c_instrument,
          c_explicit_ra,
          c_explicit_dec
        FROM t_observation
        WHERE c_observation_id = $observation_id
      """.query(instrument.opt *: right_ascension.opt *: declination.opt)

    def cfpInformation(
      site: Site
    ): Query[CallForProposals.Id, (RightAscension, RightAscension, Declination, Declination, DateInterval)] =
      val ns = site match {
        case Site.GN => "north"
        case Site.GS => "south"
      }
      sql"""
        SELECT
          c_#${ns}_ra_start,
          c_#${ns}_ra_end,
          c_#${ns}_dec_start,
          c_#${ns}_dec_end,
          c_active_start,
          c_active_end
        FROM t_cfp
        WHERE c_cfp_id = $cfp_id
      """.query(right_ascension *: right_ascension *: declination *: declination *: date_interval)

    val CfpInstruments: Query[CallForProposals.Id, Instrument] =
      sql"""
        SELECT c_instrument
        FROM t_cfp_instrument
        WHERE c_cfp_id = $cfp_id
      """.query(instrument)
  }

}
