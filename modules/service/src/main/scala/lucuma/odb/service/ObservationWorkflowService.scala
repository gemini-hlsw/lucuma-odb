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
import lucuma.odb.data.ObservationExecutionState
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.data.ObservationWorkflow
import lucuma.odb.data.ObservationWorkflowState
import lucuma.odb.data.Tag
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.MissingParamSet
import lucuma.odb.sequence.util.CommitHash
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
    oid: Observation.Id,
    itcClient: ItcClient[F],
  )(using Transaction[F]): F[Result[List[ObservationValidation]]]

  def getWorkflow(
    oid: Observation.Id, 
    commitHash: CommitHash, 
    itcClient: ItcClient[F]
  )(using Transaction[F]): F[Result[ObservationWorkflow]]

}

/* Validation Info Record */
case class ObservationValidationInfo(
  pid: Program.Id,
  oid: Observation.Id,
  instrument: Option[Instrument],
  ra: Option[RightAscension],
  dec: Option[Declination],
  forReview: Boolean,
  role: Option[CalibrationRole],
  proposalStatus: Tag,
  activeStatus: ObsActiveStatus,
  cfpid: Option[CallForProposals.Id],
) {
  
  def isMarkedReady: Boolean = 
    false // TODO

  /* Has the proposal been accepted? */
  def isAccepted(using enums: Enums): Result[Boolean] =
    Result.fromOption(
      Enumerated[enums.ProposalStatus].fromTag(proposalStatus.value).map(_ === enums.ProposalStatus.Accepted),
      s"Unexpected enum value for ProposalStatus: ${proposalStatus.value}"
    )
  
}


object ObservationWorkflowService {

  /* Validation Messages */
  object Messages:
    
    val AsterismOutOfRange     = "Asterism out of Call for Proposals limits."
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
    
  /* Some Syntax. */
  extension (ra: RightAscension)
    private def isInInterval(raStart: RightAscension, raEnd: RightAscension): Boolean =
      if (raStart > raEnd) raStart <= ra || ra <= raEnd
      else raStart <= ra && ra <= raEnd

  extension (dec: Declination)
    private def isInInterval(decStart: Declination, decEnd: Declination): Boolean =
      decStart <= dec && dec <= decEnd

  extension (mp: MissingParamSet)
    def toObsValidation: ObservationValidation =
      ObservationValidation.configuration(s"Missing ${mp.params.map(_.name).toList.intercalate(", ")}")

  extension (ge: GeneratorParamsService.Error)
    private def toObsValidation: ObservationValidation = ge match
      case GenParamsError.MissingData(p) => p.toObsValidation
      case _                             => ObservationValidation.configuration(ge.format)

  /* Construct an instance. */
  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ObservationWorkflowService[F] =
    new ObservationWorkflowService[F] {

      // Make the enums available in a stable and implicit way
      given Enums = enums

      /** Retrieve the generator params, or report an error. */
      def generatorParams(info: ObservationValidationInfo)(using Transaction[F]): F[Either[ObservationValidationMap, GeneratorParams]] =
        generatorParamsService.selectOne(info.pid, info.oid).map:
          case Left(error)                           => ObservationValidationMap.singleton(error.toObsValidation).asLeft
          case Right(GeneratorParams(Left(m), _, _)) => ObservationValidationMap.singleton(m.toObsValidation).asLeft
          case Right(ps)                             => ps.asRight

      /** Validate that the asterism is within the specified range. */
      private def validateAsterismRaDec(
        info: ObservationValidationInfo,
        raStart: RightAscension,
        raEnd: RightAscension,
        decStart: Declination,
        decEnd: Declination,
        site: Site,
        active: DateInterval
      ): F[Option[ObservationValidation]] =
        asterismService.getAsterism(info.pid, info.oid)
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
        info: ObservationValidationInfo,
        cid: CallForProposals.Id,
        explicitBase: Option[(RightAscension, Declination)]
      ): F[Option[ObservationValidation]] =
        (for {
          site <- OptionT.fromOption(info.instrument.map(_.site.toList).collect {
            case List(Site.GN) => Site.GN
            case List(Site.GS) => Site.GS
          })
          (raStart, raEnd, decStart, decEnd, active) <- OptionT.liftF(session.unique(Statements.cfpInformation(site))(cid))
          // if the observation has explicit base declared, use that
          validation <- explicitBase.fold(OptionT(validateAsterismRaDec(info, raStart, raEnd, decStart, decEnd, site, active))) { (ra, dec) =>
            OptionT.fromOption(Option.unless(ra.isInInterval(raStart, raEnd) && dec.isInInterval(decStart, decEnd))(ObservationValidation.callForProposals(Messages.ExplicitBaseOutOfRange)))
          }
        } yield validation).value

      /* Validate that an observation is compatible with its program's CFP. */
      private def cfpValidations(info: ObservationValidationInfo): F[ObservationValidationMap] =
        info.cfpid.fold(ObservationValidationMap.empty.pure): cid =>
          for
            valInstr        <- validateInstrument(cid, info.instrument)
            explicitBase     = (info.ra, info.dec).tupled
            valRaDec        <- validateRaDec(info, cid, explicitBase)
            valForactivation = Option.when(info.forReview)(ObservationValidation.configuration(Messages.ConfigurationForReview))
          yield ObservationValidationMap.fromList(List(valInstr, valRaDec, valForactivation).flatten)

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
        info: ObservationValidationInfo,
        itcClient: ItcClient[F], 
        params: GeneratorParams
      )(using Transaction[F]): F[ObservationValidationMap] =
        itcService(itcClient).selectOne(info.pid, info.oid, params).map:
          // N.B. there will soon be more cases here
          case Some(_) => ObservationValidationMap.empty
          case None    => ObservationValidationMap.singleton(ObservationValidation.itc("ITC results are not present."))

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

      // Construct some finer-grained types to make it harder to do something dumb in the status computation.
      import ObservationWorkflowState.*
      type UserState       = Inactive.type  | Ready.type
      type ExecutionState  = Ongoing.type   | Completed.type
      type ValidationState = Undefined.type | Unapproved.type | Defined.type

      private def executionState(info: ObservationValidationInfo, commitHash: CommitHash, itcClient: ItcClient[F])(using Enums): F[Option[ExecutionState]] =
        TimeEstimateCalculatorImplementation.fromSession(session, summon).flatMap: ptc =>
          generator(commitHash, itcClient, ptc).executionState(info.pid, info.oid).map:
            case ObservationExecutionState.NotDefined => None
            case ObservationExecutionState.NotStarted => None
            case ObservationExecutionState.Ongoing    => Some(Ongoing)
            case ObservationExecutionState.Completed  => Some(Completed)
        
      // Compute the observation status, as well as a list of legal transitions,
      private def workflowStateAndTransitions(
        info: ObservationValidationInfo,
        executionState: Option[ExecutionState], 
        codes: List[ObservationValidationCode]
      )(using Enums): Result[(ObservationWorkflowState, List[ObservationWorkflowState])] =
        info.isAccepted.map { isAccepted =>

          // A special ordering where codes are ordered as they would occur in a typical lifecycle.
          given Ordering[ObservationValidationCode] =
            Ordering.by:
              case ObservationValidationCode.CallForProposalsError => 1
              case ObservationValidationCode.ItcError              => 2
              case ObservationValidationCode.ConfigurationError    => 3

          val validationStatus: ValidationState = 
            codes.minOption match
              case None                                                  => Defined
              // ... => Unapproved
              case Some(ObservationValidationCode.ConfigurationError)    => Undefined
              case Some(ObservationValidationCode.CallForProposalsError) => Undefined
              case Some(ObservationValidationCode.ItcError)              => Undefined                                    

          def userStatus(validationStatus: ValidationState): Option[UserState] =
            info.activeStatus match
              case ObsActiveStatus.Inactive => Some(Inactive)
              case ObsActiveStatus.Active   => Option.when(info.isMarkedReady && validationStatus == Defined)(Ready)

          // Our final state is the execution state (if any), else the user state (if any), else the validation state,
          // with the one exception that user state Inactive overrides execution state Ongoing
          val state: ObservationWorkflowState =
            (executionState, userStatus(validationStatus)) match
              case (None, None)     => validationStatus
              case (None, Some(us)) => us              
              case (Some(es), None) => es              
              case (Some(Ongoing), Some(Inactive)) => Inactive
              case (Some(es), _)    => es
                    
          val allowedTransitions: List[ObservationWorkflowState] =
            state match
              case Inactive         => List(validationStatus)
              case Undefined  => List(Inactive)
              case Unapproved => List(Inactive)
              case Defined    => List(Inactive) ++ Option.when(isAccepted)(Ready)
              case Ready            => List(Inactive, validationStatus)
              case Ongoing     => List(Inactive)
              case Completed   => Nil

          (state, allowedTransitions)

        }

      private def observationValidationsImpl(
        info: ObservationValidationInfo,
        itcClient: ItcClient[F],
      )(using Transaction[F]): ResultT[F, (Option[GeneratorParams], List[ObservationValidation])] =
        if info.role.isDefined then ResultT.pure((None, List.empty)) // don't warn for calibrations
        else {

          /* Partial computation of validation errors, excluding configuration checking. */
          val partialMap: F[(Option[GeneratorParams], ObservationValidationMap)] = 
            for {
              gen      <- generatorParams(info)
              genVals   = gen.swap.getOrElse(ObservationValidationMap.empty)
              cfpVals  <- cfpValidations(info)
              itcVals  <- Option.when(cfpVals.isEmpty)(gen.toOption).flatten.foldMapM(itcValidations(info, itcClient, _)) // only compute this if cfp and gen are ok
              bandVals <- validateScienceBand(info.oid)
            } yield (gen.toOption, genVals |+| itcVals |+| cfpVals |+| bandVals)

          // Only compute configuration request status if everything else is ok and the proposal has been accepted
          def fullMap(isAccepted: Boolean): F[(Option[GeneratorParams], ObservationValidationMap)] =
            partialMap.flatMap: (gen, m) =>
              if m.isEmpty && isAccepted then validateConfiguration(info.oid).tupleLeft(gen)
              else (gen, m).pure[F]          

          // Put it together
          for
            accepted <- ResultT(info.isAccepted.pure[F])
            pair     <- ResultT.liftF(fullMap(accepted))
            (gp, warnings) = pair
          yield (gp, warnings.toList)

        }

      override def observationValidations(
        oid: Observation.Id,
        itcClient: ItcClient[F]
      )(using Transaction[F]): F[Result[List[ObservationValidation]]] =
        ResultT.liftF(obsInfo(oid)).flatMap(observationValidationsImpl(_, itcClient)).map(_._2).value

      private def getWorkflowImpl(
        oid: Observation.Id, 
        commitHash: CommitHash,
        itcClient: ItcClient[F]
      )(using Transaction[F]): ResultT[F, ObservationWorkflow] =
        for
          info <- ResultT.liftF(obsInfo(oid))
          pair <- observationValidationsImpl(info, itcClient)
          (gp, errs) = pair
          ex   <- ResultT.liftF(executionState(info, commitHash, itcClient))
          pair <- ResultT(workflowStateAndTransitions(info, ex, errs.map(_.code)).pure[F])
          (s, ss) = pair
        yield ObservationWorkflow(s, ss, errs)

      override def getWorkflow(
        oid: Observation.Id,
        commitHash: CommitHash, 
        itcClient: ItcClient[F]
      )(using Transaction[F]): F[Result[ObservationWorkflow]] =
        getWorkflowImpl(oid, commitHash, itcClient).value

  }

  object Statements {

    val ObservationValidationInfo:
      Query[Observation.Id, ObservationValidationInfo] =
      sql"""
        SELECT
          o.c_program_id,
          o.c_observation_id,
          o.c_instrument,
          o.c_explicit_ra,
          o.c_explicit_dec,
          o.c_for_review,
          o.c_calibration_role,
          p.c_proposal_status,
          o.c_active_status,
          x.c_cfp_id
        FROM t_observation o
        JOIN t_program p on p.c_program_id = o.c_program_id
        LEFT JOIN t_proposal x ON o.c_program_id = x.c_program_id
        WHERE c_observation_id = $observation_id
      """
      .query(program_id *: observation_id *: instrument.opt *: right_ascension.opt *: declination.opt *: bool *: calibration_role.opt *: tag *: obs_active_status *: cfp_id.opt)
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
