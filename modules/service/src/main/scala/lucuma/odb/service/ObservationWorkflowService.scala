// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.implicits.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ExecutionState as CoreExecutionState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.CallCoordinatesLimits
import lucuma.core.model.CallForProposals
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.Program
import lucuma.core.model.SiteCoordinatesLimits
import lucuma.core.model.StandardRole.*
import lucuma.core.model.Target
import lucuma.core.syntax.string.*
import lucuma.core.util.DateInterval
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import lucuma.itc.client.ItcClient
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.Tag
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.MissingParamSet
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.GeneratorParamsService.Error as GenParamsError
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.syntax.instrument.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.*
import skunk.implicits.*

import java.time.Instant

import Services.Syntax.*

sealed trait ObservationWorkflowService[F[_]] {

  def getWorkflows(
    oids: List[Observation.Id],
    commitHash: CommitHash,
    itcClient: ItcClient[F],
    ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using NoTransaction[F], SuperUserAccess): F[Result[Map[Observation.Id, ObservationWorkflow]]]

  def getWorkflow(
    oid: Observation.Id,
    commitHash: CommitHash,
    itcClient: ItcClient[F],
    ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using NoTransaction[F], SuperUserAccess): F[Result[ObservationWorkflow]]

  def getWorkflows(
    pid: Program.Id,
    commitHash: CommitHash,
    itcClient: ItcClient[F],
    ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using NoTransaction[F], SuperUserAccess): F[Result[Map[Observation.Id, ObservationWorkflow]]]

  /**
   * Computes the workflow for the observation using the current results of a
   * background calculation instead of pausing to unfold the execution sequence.
   * As such, the results may be stale and pending update.  Because there is no
   * long-running calculation, `SuperUserAccess` is not required and we demand a
   * `Transaction`.
   */
  def getCalculatedWorkflow(
    oid:  Observation.Id,
    itc:  Option[ItcService.AsterismResults],
    exec: Option[CoreExecutionState]
  )(using Transaction[F]): F[Result[ObservationWorkflow]]

  def setWorkflowState(
    input: AccessControl.CheckedWithId[(ObservationWorkflow, ObservationWorkflowState), Observation.Id],
    commitHash: CommitHash,
    itcClient: ItcClient[F],
    ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using NoTransaction[F]): F[Result[ObservationWorkflow]]

  def filterState(
    oids: List[Observation.Id],
    states: Set[ObservationWorkflowState],
    commitHash: CommitHash,
    itcClient: ItcClient[F],
    ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using NoTransaction[F], SuperUserAccess): F[Result[List[Observation.Id]]]

  def filterState(
    which: AppliedFragment,
    states: Set[ObservationWorkflowState],
    commitHash: CommitHash,
    itcClient: ItcClient[F],
    ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using NoTransaction[F], SuperUserAccess): F[Result[List[Observation.Id]]]

  def filterTargets(
    which: AppliedFragment,
    states: Set[ObservationWorkflowState],
    commitHash: CommitHash,
    itcClient: ItcClient[F],
    ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using NoTransaction[F], SuperUserAccess): F[Result[List[Target.Id]]]

}

/* Validation Info Record */
case class ObservationValidationInfo(
  pid:                Program.Id,
  tpe:                ProgramType,
  oid:                Observation.Id,
  instrument:         Option[Instrument],
  coordinates:        Option[Coordinates],  // explicit base, or coordinates at CFP midpoint, if any
  explicitBase:       Option[Coordinates],
  role:               Option[CalibrationRole],
  userState:          Option[ObservationWorkflowService.UserState],
  declaredComplete:   Boolean,
  proposalStatus:     Tag,
  cfpid:              Option[CallForProposals.Id],
  scienceBand:        Option[ScienceBand],
  asterism:           List[Target],
  generatorParams:    Option[Either[GeneratorParamsService.Error, GeneratorParams]] = None,
  cfpInfo:            Option[CfpInfo] = None,
  programAllocations: Option[NonEmptyList[ScienceBand]] = None,
) {

  /* Has the proposal been accepted? */
  def isAccepted(using enums: Enums): Result[Boolean] =
    Result.fromOption(
      Enumerated[enums.ProposalStatus].fromTag(proposalStatus.value).map(_ === enums.ProposalStatus.Accepted),
      s"Unexpected enum value for ProposalStatus: ${proposalStatus.value}"
    )

  def site: Option[Site] =
    instrument.flatMap(_.site.headOption) // TODO: fix this so there's never more than one instrument

  def isOpportunity: Boolean =
    asterism.exists:
      case t: Target.Opportunity => true
      case _ => false

  lazy val cfpMidpoint: Option[Timestamp] =
    for
      s  <- site
      c  <- cfpInfo
      ts <- Timestamp.fromInstant(c.midpoint(s))
    yield ts

}

case class CfpInfo(
  cfpid:       CallForProposals.Id,
  limits:      CallCoordinatesLimits,
  active:      DateInterval,
  instruments: List[Instrument]
) {

  def midpoint(at: Site): Instant =
    at.midpoint(active)

  def addInstrument(insts: Option[Instrument]): CfpInfo =
    insts.fold(this)(inst => copy(instruments = inst :: instruments))

}


object ObservationWorkflowService {

  // Construct some finer-grained types to make it harder to do something dumb in the status computation.
  import ObservationWorkflowState.*
  type UserState       = Inactive.type  | Ready.type
  type ExecutionState  = Ongoing.type   | Completed.type
  type ValidationState = Undefined.type | Unapproved.type | Defined.type

  /* Validation Messages */
  object Messages {

    val CoordinatesOutOfRange = "Base coordinates out of Call for Proposals limits."

    def invalidInstrument(instr: Instrument): String =
      s"Instrument $instr not part of Call for Proposals."

    def invalidScienceBand(b: ScienceBand): String =
      s"Science Band ${b.tag.toScreamingSnakeCase} has no time allocation."
  }

  extension (ws: ObservationWorkflowState) def asUserState: Option[UserState] =
    ws match
      case Inactive => Some(Inactive)
      case Ready    => Some(Ready)
      case _        => None

  extension (ws: ObservationWorkflowState) def isUserState: Boolean =
    ws.asUserState.isDefined

  extension (es: CoreExecutionState) def workflowExecutionState: Option[ExecutionState] =
    es match
      case CoreExecutionState.Completed        => Completed.some
      case CoreExecutionState.DeclaredComplete => Completed.some
      case CoreExecutionState.Ongoing          => Ongoing.some
      case _                                   => none

  extension [A,B,C](m: Map[A, Either[B, C]]) def separateValues: (Map[A, B], Map[A, C]) =
    m.foldLeft((Map.empty[A,B], Map.empty[A,C])):
      case ((ls, rs), (a, Left(b)))  => (ls + (a -> b), rs)
      case ((ls, rs), (a, Right(c))) => (ls, rs + (a -> c))

  extension (mp: MissingParamSet)
    def toObsValidation: ObservationValidation =
      ObservationValidation.configuration(s"Missing ${mp.params.map(_.name).toList.intercalate(", ")}")

  extension (ge: GeneratorParamsService.Error)
    private def toObsValidation: ObservationValidation = ge match
      case GenParamsError.MissingData(p) => p.toObsValidation
      case _                             => ObservationValidation.configuration(ge.format)

  /* Construct an instance. */
  def instantiate[F[_]: Concurrent](using Services[F]): ObservationWorkflowService[F] =
    new ObservationWorkflowService[F] {

      // Make the enums available in a stable and implicit way
      given Enums = enums

      private def lookupObsDefinitions(
        oids: List[Observation.Id]
      )(using Transaction[F]): F[Map[Observation.Id, ObservationValidationInfo]] = {

        def partialObsInfos(oids: NonEmptyList[Observation.Id]): F[Map[Observation.Id, ObservationValidationInfo]] =
          val enc = observation_id.nel(oids)
          session
            .stream(Statements.ObservationValidationInfosWithoutAsterisms(enc))(oids, 1024)
            .compile
            .toList
            .map: list =>
              list
                .fproductLeft(_.oid)
                .toMap

        def addAsterisms(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
          Services.asSuperUser:
            asterismService
              .getAsterisms(input.keys.toList)
              .map: results =>
                input.map: (oid, info) =>
                  oid -> info.copy(asterism = results.get(oid).foldMap(_.map(_._2)))

        def addGeneratorParams(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
          generatorParamsService
            .selectMany(input.keys.toList)
            .map: results =>
              input.map: (oid, info) =>
                oid -> info.copy(generatorParams = results.get(oid))

        def addCfpInfos(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
          NonEmptyList.fromList(input.values.flatMap(_.cfpid).toList.distinct) match
            case None => input.pure[F]
            case Some(nel) =>
              val enc = cfp_id.nel(nel)
              session
                .stream(Statements.CfpInfos(enc))(nel, 1024)
                .compile
                .fold(Map.empty[CallForProposals.Id, CfpInfo]):
                  case (m, (cfp, oinst)) =>
                    m.updatedWith(cfp.cfpid):
                      case None    => cfp.addInstrument(oinst).some
                      case Some(c) => c.addInstrument(oinst).some
                .map: results =>
                  input.map: (oid, info) =>
                    oid -> info.copy(cfpInfo = info.cfpid.flatMap(results.get))

        def addProgramAllocations(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
          NonEmptyList.fromList(input.values.map(_.pid).toList.distinct) match
            case None => input.pure[F]
            case Some(nel) =>
              val enc = program_id.nel(nel)
              session
                .stream(Statements.ProgramAllocations(enc))(nel, 1024)
                .compile
                .toList
                .map: list =>
                  list.foldMap: (pid, band) =>
                    Map(pid -> NonEmptyList.one(band))
                .map: result =>
                  input.map: (oid, info) =>
                    oid -> info.copy(programAllocations = result.get(info.pid))

        def addCoordinates(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
          input
            .values
            .groupBy(_.cfpMidpoint)
            .collect:
              case (Some(k), v) => k -> v.toList.map(_.oid)
            .toList // we now have a list of (Timestamp, List[oid]) batches
            .foldLeftM(input): // fold over this list with `input` as the starting point, updating when we can find coordinates
              case (accum, (when, batch)) =>
                trackingService
                  .getCoordinatesSnapshotOrRegion(batch, when, false)
                  .map: batchResults =>
                    batchResults
                      .collect:
                        case (oid, Result.Success(Left(snap))) => oid -> snap.base // we only care about results where coordinates are known
                        case (oid, Result.Success(Right(_, Some(eb)))) => oid -> eb
                      .foldLeft(accum):
                        case (accum2, (oid, coords)) =>
                          accum2.updatedWith(oid): op =>
                            op.map(_.copy(coordinates = Some(coords)))

        NonEmptyList.fromList(oids) match
          case None      =>
            Map.empty.pure
          case Some(nel) =>
            partialObsInfos(nel)
              .flatMap(addAsterisms)
              .flatMap(addGeneratorParams)
              .flatMap(addCfpInfos)
              .flatMap(addProgramAllocations)
              .flatMap(addCoordinates)
      }

      private def lookupCachedItcResults(
        input:      Map[Observation.Id, ObservationValidationInfo],
      )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, ItcService.AsterismResults]] =
        itcService
          .selectAll:
            input
              .view
              .mapValues(_.generatorParams)
              .collect:
                case (id, Some(Right(params))) => id -> params
              .toMap


      @annotation.nowarn("msg=unused implicit parameter")
      private def validateConfigurations(infos: NonEmptyList[ObservationValidationInfo])(using Transaction[F]): ResultT[F, Map[Observation.Id, ObservationValidationMap]] =
        ResultT(configurationService.selectRequests(infos.toList.map(i => (i.pid, i.oid)))).map: rs =>
          rs.view
            .map:
              case ((_, oid), lst) =>
                oid -> {
                  val m = ObservationValidationMap.empty
                  if lst.isEmpty then m.add(ObservationValidation.configurationRequestNotRequested)
                  else if lst.exists(_.status === ConfigurationRequestStatus.Approved) then m
                  else if lst.forall(_.status === ConfigurationRequestStatus.Denied) then m.add(ObservationValidation.configurationRequestDenied)
                  else m.add(ObservationValidation.configurationRequestPending)
                }
            .toMap

      // Computes the observation execution state if not cached
      private def executionStates(
        infos:      Map[Observation.Id, ObservationValidationInfo],
        itcResults: Map[Observation.Id, ItcService.AsterismResults],
        commitHash: CommitHash,
        ptc:        TimeEstimateCalculatorImplementation.ForInstrumentMode
      )(using NoTransaction[F], SuperUserAccess): F[Map[Observation.Id, ExecutionState]] =
        generator(commitHash, ptc)
          .executionStates:
            infos
              .toList
              .flatMap: (oid, info) =>
                (itcResults.get(oid), info.generatorParams.flatMap(_.toOption)).mapN: (itc, gps) =>
                  oid -> (info.pid, itc, gps)
              .toMap
          .map: result =>
            result
              .view
              .mapValues(_.workflowExecutionState)
              .collect[(Observation.Id, ExecutionState)]:
                case (oid, Some(es)) => oid -> es
              .toMap

      // Compute the observation status, as well as a list of legal transitions,
      private def workflowStateAndTransitions(
        info:           ObservationValidationInfo,
        executionState: Option[ExecutionState],
        codes:          List[ObservationValidationCode]
      )(using Enums): Result[(ObservationWorkflowState, List[ObservationWorkflowState])] =
        info.isAccepted.map { isAccepted =>

          // A special ordering where codes are ordered as they would occur in a typical lifecycle.
          given Ordering[ObservationValidationCode] =
            Ordering.by:
              case ObservationValidationCode.CallForProposalsError => 1
              case ObservationValidationCode.ConfigurationError => 2
              case ObservationValidationCode.ItcError => 3
              case ObservationValidationCode.ConfigurationRequestUnavailable => 4
              case ObservationValidationCode.ConfigurationRequestNotRequested => 5
              case ObservationValidationCode.ConfigurationRequestDenied => 6
              case ObservationValidationCode.ConfigurationRequestPending => 7

          val validationStatus: ValidationState =
            codes.minOption.fold(Defined):
              case ObservationValidationCode.CallForProposalsError            |
                   ObservationValidationCode.ConfigurationError               |
                   ObservationValidationCode.ItcError                         => Undefined
              case ObservationValidationCode.ConfigurationRequestUnavailable  |
                   ObservationValidationCode.ConfigurationRequestNotRequested |
                   ObservationValidationCode.ConfigurationRequestDenied       |
                   ObservationValidationCode.ConfigurationRequestPending      => Unapproved

          def userStatus(validationStatus: ValidationState): Option[UserState] =
            if info.role.isDefined then Some(Ready) // Calibrations are immediately ready
            else info.userState.flatMap:
              case Inactive => Some(Inactive)       // Inactive overrides validation errors
              case Ready    =>
                validationStatus match              // Validation errors override Ready
                  case Undefined  => None
                  case Unapproved => None
                  case Defined    => Some(Ready)

          // Our final state is the execution state (if any), else the user state (if any), else the validation state,
          // with the one exception that user state Inactive overrides execution state Ongoing
          val state: ObservationWorkflowState =
            (executionState, userStatus(validationStatus)) match
              case (None, None)     => validationStatus
              case (None, Some(us)) => us
              case (Some(Ongoing), Some(Inactive)) => Inactive
              case (Some(es), _)    => es

          val allowedTransitions: List[ObservationWorkflowState] =
            if info.role.isDefined then Nil // User can't set the state for calibrations
            else state match
              case Inactive   => List(executionState.getOrElse(validationStatus))
              case Undefined  => List(Inactive)
              case Unapproved => List(Inactive)
              case Defined    => List(Inactive) ++ Option.when((!info.isOpportunity) && (isAccepted || info.tpe =!= ProgramType.Science))(Ready)
              case Ready      => List(Inactive, validationStatus)
              case Ongoing    => List(Inactive, Completed)
              case Completed  => if info.declaredComplete then List(Ongoing) else Nil

          (state, allowedTransitions)

        }

      private def validateObsDefinition(
        infos:  Map[Observation.Id, ObservationValidationInfo],
        hasItc: Observation.Id => Boolean
      )(using Transaction[F]): ResultT[F, Map[Observation.Id, ObservationValidationMap]] = {

        type Validator = ObservationValidationInfo => ObservationValidationMap

        val (cals, other)         = infos.partition(_._2.role.isDefined)
        val (nonScience, science) = other.partition(_._2.tpe =!= ProgramType.Science)

        // Here are our simple validators

        val generatorValidator: Validator = info =>
          info.generatorParams.foldMap:
            case Left(error)                                    => ObservationValidationMap.singleton(error.toObsValidation)
            case Right(GeneratorParams(Left(m), _, _, _, _, _)) => ObservationValidationMap.singleton(m.toObsValidation)
            case Right(ps)                                      => ObservationValidationMap.empty

        val cfpInstrumentValidator: Validator = info =>
          info.cfpInfo.foldMap: cfp =>
            if cfp.instruments.isEmpty then ObservationValidationMap.empty // weird but original logic does this
            else info.instrument.foldMap: inst =>
              if cfp.instruments.contains(inst) then ObservationValidationMap.empty
              else ObservationValidationMap.singleton(ObservationValidation.callForProposals(Messages.invalidInstrument(inst)))

        val cfpRaDecValidator: Validator = info =>
          info.cfpInfo.foldMap: cfp =>
            info.site.foldMap: site =>
              info.coordinates.foldMap: coords =>
                val ok = cfp.limits.siteLimits(site).inLimits(coords)
                if ok then ObservationValidationMap.empty
                else ObservationValidationMap.singleton(ObservationValidation.callForProposals(Messages.CoordinatesOutOfRange))

        val bandValidator: Validator = info =>
          (info.scienceBand, info.programAllocations).tupled.foldMap: (b, bs) =>
            if bs.toList.contains(b) then ObservationValidationMap.empty
            else ObservationValidationMap.singleton(ObservationValidation.configuration(Messages.invalidScienceBand(b)))

        val itcValidator: Validator = info =>
          if hasItc(info.oid) then ObservationValidationMap.empty
          else ObservationValidationMap.singleton(ObservationValidation.itc("ITC results are not present."))

        // Here are our composed validators

        val calibrationValidator, engValidator: Validator = _ =>
          ObservationValidationMap.empty

        val scienceValidator1: Validator =
          generatorValidator     |+|
          cfpInstrumentValidator |+|
          cfpRaDecValidator      |+|
          bandValidator

        val scienceValidator2: Validator =
          itcValidator

        // And our validation results

        val engResults: Map[Observation.Id, ObservationValidationMap] =
          nonScience.view.mapValues(engValidator).toMap

        val calibrationResults: Map[Observation.Id, ObservationValidationMap] =
          cals.view.mapValues(calibrationValidator).toMap

        val scienceResults1: Map[Observation.Id, ObservationValidationMap] =
          science.view.mapValues(scienceValidator1).toMap

        val scienceResults2: Map[Observation.Id, ObservationValidationMap] =
          science
            .view
            .filterKeys(k => scienceResults1.get(k).forall(_.isEmpty)) // ensure there are no warnigs in stage 1
            .mapValues(scienceValidator2)
            .toMap

        val prelimV: Map[Observation.Id, ObservationValidationMap] =
          calibrationResults |+| engResults |+| scienceResults1 |+| scienceResults2

        val toCheck: List[ObservationValidationInfo] =
          science.values.toList.filter: info =>
            info.isAccepted.toOption.forall(_ === true) && prelimV.get(info.oid).forall(_.isEmpty)

        val configValidations: ResultT[F, Map[Observation.Id, ObservationValidationMap]] =
          NonEmptyList
            .fromList(toCheck)
            .fold(ResultT.pure(Map.empty[Observation.Id, ObservationValidationMap]))(validateConfigurations)

        configValidations.map(prelimV |+| _)

      }

      private def computeWorkflows(
        infos: Map[Observation.Id, ObservationValidationInfo],
        errs:  Map[Observation.Id, ObservationValidationMap],
        execs: Map[Observation.Id, ExecutionState]
      ): Result[Map[Observation.Id, ObservationWorkflow]] =
        infos
          .toList
          .traverse: (oid, info) =>
            val obsErrors = errs.get(oid).toList.flatMap(_.toList)
            workflowStateAndTransitions(info, execs.get(oid), obsErrors.map(_.code))
              .map: (s, ss) =>
                 oid -> ObservationWorkflow(s, ss, obsErrors)
          .map(_.toMap)

      override def getWorkflows(
        oids: List[Observation.Id],
        commitHash: CommitHash,
        itcClient: ItcClient[F],
        ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
      )(using NoTransaction[F], SuperUserAccess): F[Result[Map[Observation.Id, ObservationWorkflow]]] =

        // Data obtained from the database, requiring a transaction.
        val select: F[Result[(
          Map[Observation.Id, ObservationValidationInfo],
          Map[Observation.Id, ObservationValidationMap],
          Map[Observation.Id, ItcService.AsterismResults]
        )]] =
          services.transactionally:
            (
              for
                infos  <- ResultT.liftF(lookupObsDefinitions(oids))               // Map[Observation.Id, ObsDefinition]
                itcRes <- ResultT.liftF(lookupCachedItcResults(infos)) // Map[Observation.Id, ItcService.AsterismResults]
                errs   <- validateObsDefinition(infos, itcRes.keySet.apply)       // Map[Observation.Id, ObservationValidationMap]
              yield (infos, errs, itcRes)
            ).value

        (for
          (infos, errs, itcRes) <- ResultT(select)
          errorFree              = infos.view.filterKeys(oid => errs.get(oid).forall(_.isEmpty)).toMap
          execs                 <- ResultT.liftF(executionStates(errorFree, itcRes, commitHash, ptc))
          workflows             <- ResultT.fromResult(computeWorkflows(infos, errs, execs))
        yield workflows).value

      override def getWorkflow(
        oid: Observation.Id,
        commitHash: CommitHash,
        itcClient: ItcClient[F],
        ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
      )(using NoTransaction[F], SuperUserAccess): F[Result[ObservationWorkflow]] =
        getWorkflows(List(oid), commitHash, itcClient, ptc).map: result =>
          result.flatMap: map =>
            map.get(oid) match
              case Some(wf) => Result(wf)
              case None     => OdbError.InvalidObservation(oid, Some(s"Could not compute workflow for $oid.")).asFailure

      override def getCalculatedWorkflow(
        oid:  Observation.Id,
        itc:  Option[ItcService.AsterismResults],
        exec: Option[CoreExecutionState]
      )(using Transaction[F]): F[Result[ObservationWorkflow]] =
        (for
          infos <- ResultT.liftF(lookupObsDefinitions(List(oid)))
          errs  <- validateObsDefinition(infos, _ => itc.isDefined)
          wfExec = exec.flatMap[ExecutionState](ces => ces.workflowExecutionState)
          execs  = wfExec.fold[Map[Observation.Id, ExecutionState]](Map.empty)(es => Map(oid -> es))
          wfs   <- ResultT.fromResult(computeWorkflows(infos, errs, execs))
          res   <- ResultT.fromResult(Result.fromOption(wfs.get(oid), s"Invalid observation: $oid"))
        yield res).value

      override def getWorkflows(
        pid: Program.Id,
        commitHash: CommitHash,
        itcClient: ItcClient[F],
        ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
      )(using NoTransaction[F], SuperUserAccess): F[Result[Map[Observation.Id, ObservationWorkflow]]] =
        services
          .transactionally:
            session.prepareR(Statements.selectObservationIds).use: pq =>
              pq.stream(pid, 1024).compile.toList
          .flatMap(getWorkflows(_, commitHash, itcClient, ptc))

      override def setWorkflowState(
        input: AccessControl.CheckedWithId[(ObservationWorkflow, ObservationWorkflowState), Observation.Id],
        commitHash: CommitHash,
        itcClient: ItcClient[F],
        ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
      )(using NoTransaction[F]): F[Result[ObservationWorkflow]] =
        input.foldWithId(OdbError.InvalidArgument().asFailureF):
          case ((w, state), oid) =>
            if w.state === state then ResultT.success(w)
            else ResultT:
              // If we're transitioning to or from a UserState, just update that column
              if w.state.isUserState || state.isUserState then
                services.transactionally:
                  session.prepareR(Statements.UpdateUserState).use: pc =>
                    pc.execute(state.asUserState, oid)
                      .as(Result(w.copy(state = state)))
              else // we must be declaring completion (or revoking that declaration)
                import ObservationWorkflowState.*
                (w.state, state) match
                  case (Ongoing, Completed) | (Completed, Ongoing) =>
                    services.transactionally:
                      session.prepareR(Statements.UpdateDeclaredCompletion).use: pc =>
                        pc.execute(state === Completed, oid)
                          .as(Result(w.copy(state = state)))
                  case _ =>
                    // This should never happen but I want to check for it anyway.
                    Result.internalError(s"Transition from ${w.state} to $state was not expected.").pure[F]
          .value

      extension (wf: ObservationWorkflow) def isCompatibleWith(states: Set[ObservationWorkflowState]): Boolean =
        // An allowed transition from ongoing to completed [via declared completion] shouldn't prevent editing,
        // even though editing will be disabled if the transition is taken.
        (wf.state :: wf.validTransitions.filterNot(_ === ObservationWorkflowState.Completed)).forall(states.contains)

      override def filterState(
        oids: List[Observation.Id],
        states: Set[ObservationWorkflowState],
        commitHash: CommitHash,
        itcClient: ItcClient[F],
        ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
      )(using NoTransaction[F], SuperUserAccess): F[Result[List[Observation.Id]]] =
        getWorkflows(oids, commitHash, itcClient, ptc)
          .map: res =>
            res.flatMap: wfs =>
              oids.foldLeft(Result(Nil)): (r, oid) =>
                wfs.get(oid) match
                  case None => r.withProblems(OdbError.InvalidObservation(oid).asProblemNec)
                  case Some(wf) =>
                    if wf.isCompatibleWith(states) then r.map(oid :: _)
                    else r.withProblems:
                      val prefix = s"Observation $oid is ineligibile for this operation due to its workflow state (${wf.state}"
                      val suffix = if wf.validTransitions.isEmpty then ")." else s" with allowed transition to ${wf.validTransitions.mkString("/")})."
                      OdbError.InvalidObservation(oid, (prefix + suffix).some)
                        .asProblemNec

      override def filterState(
        which: AppliedFragment,
        states: Set[ObservationWorkflowState],
        commitHash: CommitHash,
        itcClient: ItcClient[F],
        ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
      )(using NoTransaction[F], SuperUserAccess): F[Result[List[Observation.Id]]] =
        services
          .transactionally:
            session.prepareR(which.fragment.query(observation_id)).use: pq =>
              pq.stream(which.argument, chunkSize = 1024).compile.toList
          .flatMap: oids =>
            filterState(oids, states, commitHash, itcClient, ptc)

      private def getObservationsForTargets(whichTargets: AppliedFragment)(using NoTransaction[F]): F[Map[Target.Id, List[Observation.Id]]] =
        services.transactionally:
          val af = Statements.selectObservationsForTargets(whichTargets)
          session.prepareR(af.fragment.query(target_id *: observation_id.opt)).use: pq =>
            pq.stream(af.argument, 1024).compile.toList.map: list =>
              list.groupMap(_._1)(_._2).view.mapValues(_.flatten).toMap

      override def filterTargets(
        which: AppliedFragment,
        states: Set[ObservationWorkflowState],
        commitHash: CommitHash,
        itcClient: ItcClient[F],
        ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode
      )(using NoTransaction[F], SuperUserAccess): F[Result[List[Target.Id]]] =
        getObservationsForTargets(which)
          .flatMap: map =>
            getWorkflows(map.values.toList.flatten, commitHash, itcClient, ptc)
              .map: res =>
                res.flatMap: wfs =>
                  map.toList.foldLeft(Result(Nil)):
                    case (accum, (tid, oids)) =>
                      oids.traverse(wfs.get) match
                        case None => Result.internalError("Unpossible: query returned one or more bogus oids")
                        case Some(wfs) =>
                          if wfs.forall(_.isCompatibleWith(states)) then accum.map(tid :: _)
                          else accum.withProblems:
                            val msg = s"Target $tid is not eligible for this operation due to the workflow state of one or more associated observations."
                            OdbError.InvalidTarget(tid, msg.some)
                              .asProblemNec

  }

  object Statements {

    def ObservationValidationInfosWithoutAsterisms[A <: NonEmptyList[Observation.Id]](enc: Encoder[A]): Query[A, ObservationValidationInfo] =
      sql"""
        SELECT
          o.c_program_id,
          p.c_program_type,
          o.c_observation_id,
          o.c_instrument,
          o.c_explicit_ra,
          o.c_explicit_dec,
          o.c_calibration_role,
          o.c_workflow_user_state,
          o.c_declared_complete,
          p.c_proposal_status,
          x.c_cfp_id,
          o.c_science_band
        FROM t_observation o
        JOIN t_program p on p.c_program_id = o.c_program_id
        LEFT JOIN t_proposal x ON o.c_program_id = x.c_program_id
        WHERE c_observation_id IN ($enc)
      """
      .query(program_id *: program_type *: observation_id *: instrument.opt *: right_ascension.opt *: declination.opt *: calibration_role.opt *: user_state.opt *: bool *: tag *: cfp_id.opt *: science_band.opt)
      .map:
        case (pid, tpe, oid, inst, ra, dec, cal, state, dc, tag, cfp, sci) =>
          ObservationValidationInfo(pid, tpe, oid, inst, None, (ra, dec).mapN(Coordinates.apply), cal, state, dc, tag, cfp, sci, Nil)

    def ProgramAllocations[A <: NonEmptyList[Program.Id]](enc: Encoder[A]): Query[A, (Program.Id, ScienceBand)] =
      sql"""
        SELECT DISTINCT
          c_program_id,
          c_science_band
        FROM
          t_allocation
        WHERE
          c_program_id IN ($enc)
      """.query(program_id *: science_band)

    def CfpInfos[A <: NonEmptyList[CallForProposals.Id]](enc: Encoder[A]): Query[A, (CfpInfo, Option[Instrument])] =
      sql"""
        SELECT
          c.c_cfp_id,
          c.c_north_ra_start,
          c.c_north_ra_end,
          c.c_north_dec_start,
          c.c_north_dec_end,
          c.c_south_ra_start,
          c.c_south_ra_end,
          c.c_south_dec_start,
          c.c_south_dec_end,
          c.c_active_start,
          c.c_active_end,
          i.c_instrument
        FROM t_cfp c
        LEFT JOIN t_cfp_instrument i
        ON c.c_cfp_id = i.c_cfp_id
        WHERE c.c_cfp_id in ($enc)
      """.query(cfp_id *: right_ascension *: right_ascension *: declination *: declination *: right_ascension *: right_ascension *: declination *: declination *: date_interval *: instrument.opt)
        .map:
          case (id, n_ra_s, n_ra_e, n_dec_s, n_dec_e, s_ra_s, s_ra_e, s_dec_s, s_dec_e, active, oinst) =>
            (CfpInfo(id, CallCoordinatesLimits(lucuma.core.model.SiteCoordinatesLimits.apply(n_ra_s, n_ra_e, n_dec_s, n_dec_e), SiteCoordinatesLimits(s_ra_s, s_ra_e, s_dec_s, s_dec_e)), active, Nil), oinst)

    val UpdateUserState: Command[(Option[UserState], Observation.Id)] =
      sql"""
        UPDATE t_observation
        SET c_workflow_user_state = ${user_state.opt}
        WHERE c_observation_id = $observation_id
      """.command

    val UpdateDeclaredCompletion: Command[(Boolean, Observation.Id)] =
      sql"""
        UPDATE t_observation
        SET c_declared_complete = $bool
        WHERE c_observation_id = $observation_id
      """.command

    val selectObservationIds: Query[Program.Id, Observation.Id] =
      sql"""
        SELECT c_observation_id
        FROM t_observation
        WHERE c_existence = 'present'
        AND c_program_id = $program_id
      """.query(observation_id)

    /** An applied fragment returning (Target.Id, Option[Observation.Id]) */
    def selectObservationsForTargets(whichTargets: AppliedFragment): AppliedFragment =
      void"""
        SELECT t.c_target_id, a.c_observation_id
        FROM t_target t
        LEFT JOIN t_asterism_target a
        ON t.c_target_id = a.c_target_id
        WHERE t.c_target_id IN (""" |+| whichTargets |+| void""")
        """

  }

}
