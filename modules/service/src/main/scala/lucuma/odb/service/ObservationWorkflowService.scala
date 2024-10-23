// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.Concurrent
import cats.implicits.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsActiveStatus.Active
import lucuma.core.enums.ObsActiveStatus.Inactive
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.CallForProposals
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservingNight
import lucuma.core.model.Program
import lucuma.core.model.StandardRole.*
import lucuma.core.model.Target
import lucuma.core.syntax.string.*
import lucuma.core.util.DateInterval
import lucuma.core.util.Enumerated
import lucuma.itc.client.ItcClient
import lucuma.odb.data.ExecutionStatus
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.data.ObservationWorkflowState
import lucuma.odb.data.Tag
import lucuma.odb.data.UserStatus
import lucuma.odb.data.ValidationStatus
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.service.GeneratorParamsService.Error as GenParamsError
import lucuma.odb.syntax.instrument.*
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.codec.boolean.bool
import skunk.implicits.*

import java.time.Duration

import Services.Syntax.*

sealed trait ObservationWorkflowService[F[_]] {

  def observationValidations(
    pid: Program.Id,
    oid: Observation.Id,
    itcClient: ItcClient[F],
  )(using Transaction[F]): F[Result[(ObservationWorkflowState, List[ObservationValidation])]]

}

object ObservationWorkflowService {

  /* Validation Messages */
  object Messages:
    
    val AsterismOutOfRange = "Asterism out of Call for Proposals limits."
    val ExplicitBaseOutOfRange = "Explicit base out of Call for Proposals limits."
    val ConfigurationForReview = "Observation must be reviewed prior to execution."

    def invalidInstrument(instr: Instrument): String = 
      s"Instrument $instr not part of Call for Proposals."

    def missingData(otid: Option[Target.Id], paramName: String): String = 
      otid.fold(s"Missing $paramName")(tid => s"Missing $paramName for target $tid")

    def invalidScienceBand(b: ScienceBand): String = 
      s"Science Band ${b.tag.toScreamingSnakeCase} has no time allocation."

    object ConfigurationRequest:
      val Unavailable  = "Configuration approval status could not be determined."
      val NotRequested = "Configuration is unapproved (approval has not been requested)."
      val Denied       = "Configuration is unapproved (request was denied)."
      val Pending      = "Configuration is unapproved (request is pending)."
    
  /* Validation Info Record */
  case class ObservationValidationInfo(
    instrument: Option[Instrument],
    ra:         Option[RightAscension],
    dec:        Option[Declination],
    forReview:  Boolean,
    role:       Option[CalibrationRole],
    proposalStatus: Tag,
    activeStatus: ObsActiveStatus
  ) {
    def isMarkedReady: Boolean = false // TODO
  }

  /* Some Syntax. */
  extension (ra: RightAscension)
    private def isInInterval(raStart: RightAscension, raEnd: RightAscension): Boolean =
      if (raStart > raEnd) raStart <= ra || ra <= raEnd
      else raStart <= ra && ra <= raEnd

  extension (dec: Declination)
    private def isInInterval(decStart: Declination, decEnd: Declination): Boolean =
      decStart <= dec && dec <= decEnd

  extension (ge: GeneratorParamsService.Error)
    private def toObsValidation: ObservationValidation = ge match
      case GenParamsError.MissingData(otid, paramName) => ObservationValidation.configuration(Messages.missingData(otid, paramName))
      case _                                           => ObservationValidation.configuration(ge.format)

  /* Construct an instance. */
  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ObservationWorkflowService[F] =
    new ObservationWorkflowService[F] {

      // A stable identifier (ie. a `val`) is needed for the enums.
      val enumsVal = enums

      /** Retrieve the CFP id for the specified program, if any */
      private def optCfpId(pid: Program.Id): F[Option[CallForProposals.Id]] =
        session.option(Statements.ProgramCfpId)(pid).map(_.flatten)

      /** Retrieve the generator params, or report an error. */
      private def generatorParams(pid: Program.Id, oid: Observation.Id)(using Transaction[F]): F[Either[ObservationValidationMap, GeneratorParams]] =
        generatorParamsService.selectOne(pid, oid).map {
          case Left(errors)                          => ObservationValidationMap.fromList(errors.map(_.toObsValidation).toList).asLeft
          case Right(GeneratorParams(Left(m), _, _)) => ObservationValidationMap.singleton(ObservationValidation.configuration(m.format)).asLeft
          case Right(ps)                             => ps.asRight
        }

      /** Validate that the asterism is within the specified range. */
      private def validateAsterismRaDec(
        pid: Program.Id, 
        oid: Observation.Id,
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
              else ObservationValidation.callForProposals(Messages.AsterismOutOfRange).some
            }).flatten
          }

      /** Validate that an observation's asteriem is compatible with the specified CFP. */
      private def validateRaDec(
        pid: Program.Id, 
        oid: Observation.Id,
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
          validation <- explicitBase.fold(OptionT(validateAsterismRaDec(pid, oid, raStart, raEnd, decStart, decEnd, site, active))) { (ra, dec) =>
            OptionT.fromOption(Option.unless(ra.isInInterval(raStart, raEnd) && dec.isInInterval(decStart, decEnd))(ObservationValidation.callForProposals(Messages.ExplicitBaseOutOfRange)))
          }
        } yield validation).value

      /* Validate that an observation is compatible with its program's CFP. */
      private def cfpValidations(
        pid: Program.Id, 
        oid: Observation.Id,
        info: ObservationValidationInfo
      ): F[ObservationValidationMap] = {
        optCfpId(pid).flatMap(
          _.fold(ObservationValidationMap.empty.pure){ cid =>
            for {
              valInstr        <- validateInstrument(cid, info.instrument)
              explicitBase     = (info.ra, info.dec).tupled
              valRaDec        <- validateRaDec(pid, oid, cid, info.instrument, explicitBase)
              valForactivation = Option.when(info.forReview)(ObservationValidation.configuration(Messages.ConfigurationForReview))
            } yield ObservationValidationMap.fromList(List(valInstr, valRaDec, valForactivation).flatten)
            }
        )
      }

      private def validateInstrument(cid: CallForProposals.Id, optInstr: Option[Instrument]): F[Option[ObservationValidation]] = {
        // If there is no instrument in the observation, that will get caught with the generatorValidations
        optInstr.fold(none.pure){ instr =>
          session.stream(Statements.CfpInstruments)(cid, chunkSize = 1024)
            .compile
            .toList
            .map(l =>
              if(l.isEmpty || l.contains(instr)) none
              else ObservationValidation.callForProposals(Messages.invalidInstrument(instr)).some
            )
        }
      }

      private def obsInfo(oid: Observation.Id): F[ObservationValidationInfo] =
        session.unique(Statements.ObservationValidationInfo)(oid)

      private def itcValidations(
        pid: Program.Id, 
        oid: Observation.Id,
        itcClient: ItcClient[F], 
        params: GeneratorParams
      )(using Transaction[F]): F[ObservationValidationMap] =
        itcService(itcClient).selectOne(pid, oid, params).map:
          // N.B. there will soon be more cases here
          case Some(_) => ObservationValidationMap.empty
          case None => ObservationValidationMap.empty.add(ObservationValidation.itc("ITC results are not present."))

      private def validateScienceBand(oid: Observation.Id): F[ObservationValidationMap] =
        session
          .option(Statements.SelectInvalidBand)(oid)
          .map { invalidBand =>
            val m = ObservationValidationMap.empty
            invalidBand.fold(m)(b => m.add(ObservationValidation.configuration(Messages.invalidScienceBand(b))))
          }

      private def validateConfiguration(oid: Observation.Id)(using Transaction[F]): F[ObservationValidationMap] =
        configurationService.selectRequests(oid).map: r =>
          val m = ObservationValidationMap.empty
          r.toOption match
            case None => m.add(ObservationValidation.configuration(Messages.ConfigurationRequest.Unavailable))
            case Some(Nil) => m.add(ObservationValidation.configuration(Messages.ConfigurationRequest.NotRequested))
            case Some(lst) if lst.exists(_.status === ConfigurationRequestStatus.Approved) => m
            case Some(lst) if lst.forall(_.status === ConfigurationRequestStatus.Denied) => m.add(ObservationValidation.configuration(Messages.ConfigurationRequest.Denied))
            case _ => m.add(ObservationValidation.configuration(Messages.ConfigurationRequest.Pending))

      private def computeExecutionStatus(oid: Observation.Id): ResultT[F, Option[ExecutionStatus]] =
        ResultT.pure(oid).as(None)
        
      // Compute the observation status, as well as a list of legal transitions,
      private def computeObservationStatus(
        oid: Observation.Id, 
        info: ObservationValidationInfo, 
        isAccepted: Boolean, 
        codes: Set[ObservationValidationCode]
      ): ResultT[F, ObservationWorkflowState] =
        computeExecutionStatus(oid).map { executionStatus =>

          val validationStatus: ValidationStatus = 
            codes.maxOption match
              case None                                                  => ValidationStatus.Defined
              case Some(ObservationValidationCode.ConfigurationError)    => ValidationStatus.Undefined
              case Some(ObservationValidationCode.CallForProposalsError) => ValidationStatus.Undefined
              case Some(ObservationValidationCode.ItcError)              => ValidationStatus.Undefined                                    

          def userStatus(validationStatus: ValidationStatus): Option[UserStatus] =
            info.activeStatus match
              case Inactive => Some(UserStatus.Inactive)
              case Active   => Option.when(info.isMarkedReady && validationStatus == ValidationStatus.Defined)(UserStatus.Ready)

          // Our final status is the execution status (if any), else the user status (if any), else the validation status,
          // with the one exception that user status Inactive overrides execution status Ongoing
          val status: ObservationWorkflowState =
            (executionStatus, userStatus(validationStatus)) match
              case (None, None)     => validationStatus
              case (None, Some(us)) => us              
              case (Some(es), None) => es              
              case (Some(ExecutionStatus.Ongoing), Some(UserStatus.Inactive)) => UserStatus.Inactive
              case (Some(es), _)    => es
                     
          val allowedTransitions: List[ObservationWorkflowState] =
            status match
              case UserStatus.Inactive         => List(validationStatus)
              case ValidationStatus.Undefined  => List(UserStatus.Inactive)
              case ValidationStatus.Unapproved => List(UserStatus.Inactive)
              case ValidationStatus.Defined    => List(UserStatus.Inactive) ++ Option.when(isAccepted)(UserStatus.Ready)
              case UserStatus.Ready            => List(UserStatus.Inactive, validationStatus)
              case ExecutionStatus.Ongoing     => List(UserStatus.Inactive)
              case ExecutionStatus.Completed   => Nil

          (status, allowedTransitions)._1

        }


      override def observationValidations(
        pid: Program.Id,
        oid: Observation.Id,
        itcClient: ItcClient[F]
      )(using Transaction[F]): F[Result[(ObservationWorkflowState, List[ObservationValidation])]] =
        obsInfo(oid).flatMap: info =>
          if (info.role.isDefined) Result((UserStatus.Ready, List.empty)).pure // it's a calibration. always ready?
          else {

            /* Partial computation of validation errors, excluding configuration checking. */
            val partialMap = 
              for {
                gen      <- generatorParams(pid, oid)
                genVals   = gen.swap.getOrElse(ObservationValidationMap.empty)
                cfpVals  <- cfpValidations(pid, oid, info)
                itcVals  <- Option.when(cfpVals.isEmpty)(gen.toOption).flatten.foldMapM(itcValidations(pid, oid, itcClient, _)) // only compute this if cfp and gen are ok
                bandVals <- validateScienceBand(oid)
              } yield (genVals |+| itcVals |+| cfpVals |+| bandVals)

            /* Has the proposal been accepted? */
            val isAccepted: Result[Boolean] =
              Result.fromOption(
                Enumerated[enumsVal.ProposalStatus].fromTag(info.proposalStatus.value).map(_ === enumsVal.ProposalStatus.Accepted),
                s"Unexpected enum value for ProposalStatus: ${info.proposalStatus.value}"
              )

            // Only compute configuration request status if everything else is ok and the proposal has been accepted
            def fullMap(isAccepted: Boolean): F[ObservationValidationMap] =
              partialMap.flatMap: m =>
                if m.isEmpty && isAccepted then validateConfiguration(oid)
                else m.pure[F]          

            val result: ResultT[F, (ObservationWorkflowState, List[ObservationValidation])] =
              for
                accepted <- ResultT(isAccepted.pure[F])
                warnings <- ResultT.liftF(fullMap(accepted))
                status   <- computeObservationStatus(oid, info, accepted, warnings.toMap.keySet)
              yield (status, warnings.toList)

            result.value
            
          }

  }

  object Statements {

    val ProgramCfpId: Query[Program.Id, Option[CallForProposals.Id]] =
      sql"""
        SELECT c_cfp_id
        FROM t_proposal
        WHERE c_program_id = $program_id
      """.query(cfp_id.opt)

    val ObservationValidationInfo:
      Query[Observation.Id, ObservationValidationInfo] =
      sql"""
        SELECT
          o.c_instrument,
          o.c_explicit_ra,
          o.c_explicit_dec,
          o.c_for_review,
          o.c_calibration_role,
          p.c_proposal_status,
          o.c_active_status
        FROM t_observation o
        JOIN t_program p on p.c_program_id = o.c_program_id
        WHERE c_observation_id = $observation_id
      """
      .query(instrument.opt *: right_ascension.opt *: declination.opt *: bool *: calibration_role.opt *: tag *: obs_active_status)
      .to[ObservationValidationInfo]


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

    // Select the science band of an observation if it is not NULL and there is
    // no corresponding time allocation for it.
    val SelectInvalidBand: Query[Observation.Id, ScienceBand] =
      sql"""
        SELECT c_science_band
        FROM t_observation o
        WHERE o.c_observation_id = $observation_id
          AND (
            o.c_science_band IS NOT NULL AND
            o.c_science_band NOT IN (
              SELECT DISTINCT c_science_band
              FROM t_allocation a
              WHERE a.c_program_id = o.c_program_id
            )
          )
      """.query(science_band)

  }

}
