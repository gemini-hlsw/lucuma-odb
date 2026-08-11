// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.effect.Async
import cats.implicits.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.Band
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.DeclaredExecutionState
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.ExecutionState as CoreExecutionState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.Observatory
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TooActivation
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.model.Access
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.Program
import lucuma.core.model.StandardRole.*
import lucuma.core.model.Target
import lucuma.core.syntax.string.*
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInputDerivation
import lucuma.odb.sequence.data.MissingParamSet
import lucuma.odb.service.GeneratorParamsService.Error as GenParamsError
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.*
import skunk.implicits.*

import Services.Syntax.*
import workflow.*

sealed trait ObservationWorkflowService[F[_]] {

  def getWorkflowsModesAndRoles(
    oids: List[Observation.Id]
  )(using NoTransaction[F], SuperUserAccess): F[Result[Map[Observation.Id, (ObservationWorkflow, Option[ObservingModeType], Option[CalibrationRole])]]]

  def getWorkflows(
    oids: List[Observation.Id]
  )(using NoTransaction[F], SuperUserAccess): F[Result[Map[Observation.Id, ObservationWorkflow]]]

  def getWorkflow(
    oid: Observation.Id
  )(using NoTransaction[F], SuperUserAccess): F[Result[ObservationWorkflow]]

  def getWorkflows(
    pid: Program.Id
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
    itc:  Option[Itc],
    exec: Option[CoreExecutionState]
  )(using Transaction[F]): F[Result[ObservationWorkflow]]

  def setWorkflowState(
    input: AccessControl.CheckedWithId[(Option[ObservingModeType], Option[CalibrationRole], ObservationWorkflow, ObservationWorkflowState), Observation.Id]
  )(using NoTransaction[F]): F[Result[ObservationWorkflow]]

  def filterState(
    oids: List[Observation.Id],
    states: Set[ObservationWorkflowState]
  )(using NoTransaction[F], SuperUserAccess): F[Result[List[Observation.Id]]]

  def filterState(
    which: AppliedFragment,
    states: Set[ObservationWorkflowState]
  )(using NoTransaction[F], SuperUserAccess): F[Result[List[Observation.Id]]]

  def filterTargets(
    which: AppliedFragment,
    states: Set[ObservationWorkflowState]
  )(using NoTransaction[F], SuperUserAccess): F[Result[List[Target.Id]]]

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

    def tooActivationExceedsCeiling(obs: TooActivation, ceiling: TooActivation): String =
      s"Target of Opportunity activation ${obs.tag.toScreamingSnakeCase} exceeds the maximum " +
      s"${ceiling.tag.toScreamingSnakeCase} allowed by the proposal."

    val OpportunityTargetRequiresActivation =
      "An observation with a Target of Opportunity placeholder must set a ToO activation other than NONE."

    val OpportunityTargetNotResolved =
      "Replace the Target of Opportunity placeholder with the actual target coordinates."

    def exchangeObservatoryMismatch(modeObs: Observatory, cfpObs: Observatory): String =
      s"Exchange observation requires a $modeObs Call for Proposals, but the proposal's observatory is $cfpObs."

    def invalidExchangeInstrument(instr: String): String =
      s"Instrument $instr is not part of the Call for Proposals."

    val MissingVMagnitude = "Please add a V magnitude."
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
      case CoreExecutionState.DeclaredOngoing  => Ongoing.some
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
  def instantiate[F[_]: Async](using Services[F]): ObservationWorkflowService[F] =
    new ObservationWorkflowService[F] {

      private def lookupCachedItcResults(
        input:      Map[Observation.Id, ObservationValidationInfo],
      )(using Transaction[F], SuperUserAccess): F[Map[Observation.Id, Itc]] =
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

      private def executionStates(
        infos: Map[Observation.Id, ObservationValidationInfo]
      )(using NoTransaction[F], SuperUserAccess): Map[Observation.Id, ExecutionState] =
        infos
          .view
          .mapValues[Option[ExecutionState]]: info =>
            val a: Option[ExecutionState] = info.declaredExecutionState.flatMap(_.workflowExecutionState)
            val b: Option[ExecutionState] = info
              .generatorParams
              .flatMap(_.toOption)
              .flatMap(_.executionState.workflowExecutionState)
            a.orElse(b)
          .collect[(Observation.Id, ExecutionState)]:
            case (oid, Some(es)) => oid -> es
          .toMap

      // Compute the observation status, as well as a list of legal transitions,
      private def workflowStateAndTransitions(
        info:           ObservationValidationInfo,
        executionState: Option[ExecutionState],
        codes:          List[ObservationValidationCode]
      ): (ObservationWorkflowState, List[ObservationWorkflowState]) =
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
            case ObservationValidationCode.TooActivationUnapproved => 8

        val validationStatus: ValidationState =
          if info.calibrationRole.isDefined then Defined // Calibrations are immediately Defined
          else codes.minOption.fold(Defined):
            case ObservationValidationCode.CallForProposalsError             |
                  ObservationValidationCode.ConfigurationError               |
                  ObservationValidationCode.ItcError                         => Undefined
            case ObservationValidationCode.ConfigurationRequestUnavailable   |
                  ObservationValidationCode.ConfigurationRequestNotRequested |
                  ObservationValidationCode.ConfigurationRequestDenied       |
                  ObservationValidationCode.ConfigurationRequestPending      |
                  ObservationValidationCode.TooActivationUnapproved          => Unapproved

        def userStatus(validationStatus: ValidationState): Option[UserState] =
          info.effectiveUserState.flatMap:
            case Inactive => Some(Inactive)       // Inactive overrides validation errors
            case Ready    =>
              validationStatus match              // Validation errors override Ready
                case Undefined  => None
                case Unapproved => None
                case Defined    => Some(Ready)

        // Our final state is the execution state (if any), else the user state (if any), else the validation state,
        val state: ObservationWorkflowState =
          (executionState, userStatus(validationStatus)) match
            case (None, None)     => validationStatus
            case (None, Some(us)) => us
            case (Some(es), _)    => es

        val canUpdateExecutionState: Boolean =
          info.isVisitor && user.role.access >= Access.Staff

        val allowedTransitions: List[ObservationWorkflowState] =
          if info.calibrationRole.contains(CalibrationRole.Telluric) && state <= Ready then
            // A telluric may be declined (set to Inactive)
            state match
              case Inactive =>
                if info.userState.contains(Inactive)
                then List(info.associatedUserState.getOrElse(validationStatus))
                else Nil
              case _ =>
                List(Inactive)
          else if (info.calibrationRole.exists(ObsExtract.PerObservationCalibrationRoles.contains) && state <= Ready) then Nil
          else state match
            case Inactive   => List(executionState.getOrElse(validationStatus))
            case Undefined  => List(Inactive)
            case Unapproved => List(Inactive)
            case Defined    =>
              // Exchange observations run at Keck/Subaru, not Gemini; they have no
              // Ready/Ongoing/Completed lifecycle, so Inactive is the only transition.
              List(Inactive) ++
                Option.when((!info.isExchange) && (!info.hasTooTarget) && (info.isAccepted || !info.tpe.hasProposal))(Ready)
            case Ready      => List(Inactive, validationStatus) ++ Option.when(canUpdateExecutionState)(Ongoing)
            case Ongoing    => List(Completed) ++ Option.when(canUpdateExecutionState)(Ready)
            case Completed  => if info.isDeclaredComplete then List(Ongoing) else Nil

        (state, allowedTransitions)

      private def validateObsDefinition(
        infos:  Map[Observation.Id, ObservationValidationInfo],
        itcFor: Observation.Id => Option[Itc]
      )(using Transaction[F]): ResultT[F, Map[Observation.Id, ObservationValidationMap]] = {

        type Validator = ObservationValidationInfo => ObservationValidationMap

        val (cals, other)         = infos.partition(_._2.calibrationRole.isDefined)
        val (nonScience, science) = other.partition(!_._2.tpe.hasProposal)

        // Here are our simple validators

        val generatorValidator: Validator = info =>
          if info.isVisitor || info.isExchange then ObservationValidationMap.empty
          else info.generatorParams.foldMap:
            case Left(error)                                                         => ObservationValidationMap.singleton(error.toObsValidation)
            case Right(GeneratorParams(itcInput = ItcInputDerivation.Incomplete(m))) => ObservationValidationMap.singleton(m.toObsValidation)
            case Right(ps)                                                           => ObservationValidationMap.empty

        val cfpInstrumentValidator: Validator = info =>
          info.cfpInfo.foldMap: cfp =>
            if cfp.instruments.isEmpty then ObservationValidationMap.empty // weird but original logic does this
            else info.instrument.foldMap: inst =>
              if cfp.instruments.contains(inst) then ObservationValidationMap.empty
              else ObservationValidationMap.singleton(ObservationValidation.callForProposals(Messages.invalidInstrument(inst)))

        // Exchange observations must match the proposal's observatory, and (when
        // the call restricts instruments) use one of its allowed exchange instruments.
        val exchangeValidator: Validator = info =>
          info.observingMode match
            case Some(e: ExchangeObservingModeType) =>
              info.cfpInfo.foldMap: cfp =>
                if cfp.observatory =!= e.observatory then
                  ObservationValidationMap.singleton(ObservationValidation.callForProposals(Messages.exchangeObservatoryMismatch(e.observatory, cfp.observatory)))
                else e match
                  case ExchangeObservingModeType.ExchangeKeck =>
                    if cfp.keckInstruments.isEmpty then ObservationValidationMap.empty
                    else info.keckInstrument.foldMap: inst =>
                      if cfp.keckInstruments.contains(inst) then ObservationValidationMap.empty
                      else ObservationValidationMap.singleton(ObservationValidation.callForProposals(Messages.invalidExchangeInstrument(inst.tag)))
                  case ExchangeObservingModeType.ExchangeSubaru =>
                    if cfp.subaruInstruments.isEmpty then ObservationValidationMap.empty
                    else info.subaruInstrument.foldMap: inst =>
                      if cfp.subaruInstruments.contains(inst) then ObservationValidationMap.empty
                      else ObservationValidationMap.singleton(ObservationValidation.callForProposals(Messages.invalidExchangeInstrument(inst.tag)))
            case _ => ObservationValidationMap.empty

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

        // The Target-of-Opportunity ceiling.  This is an authorization failure
        // rather than a misconfiguration, so it maps to Unapproved -- the
        // observation cannot advance to Ready until the activation is lowered or
        // the proposal's ceiling is raised.
        val tooActivationValidator: Validator = info =>
          if !info.exceedsTooCeiling then ObservationValidationMap.empty
          else
            ObservationValidationMap.singleton:
              ObservationValidation.tooActivationUnapproved:
                Messages.tooActivationExceedsCeiling(info.tooActivation, info.tooCeiling.get)

        // An opportunity target is a placeholder standing in for a target that
        // has not been found yet, so it makes sense only in an observation that
        // declares itself a Target of Opportunity, and only until the alert
        // arrives.  Either way the observation is internally inconsistent rather
        // than merely unapproved, so this is a ConfigurationError -- Undefined,
        // which also suppresses a stored Ready.
        //
        // The second case is a backstop.  A placeholder already blocks the
        // Defined -> Ready transition, so a trigger cannot be requested for one,
        // but the asterism can still be edited afterwards (Ready is in
        // preExecutionSet) and a Ready ToO with no coordinates is worse than a
        // loud error.
        val opportunityTargetValidator: Validator = info =>
          if !info.hasTooTarget then ObservationValidationMap.empty
          else if !info.isTooObservation then
            ObservationValidationMap.singleton(ObservationValidation.configuration(Messages.OpportunityTargetRequiresActivation))
          else if info.effectiveUserState.contains(Ready) then
            ObservationValidationMap.singleton(ObservationValidation.configuration(Messages.OpportunityTargetNotResolved))
          else ObservationValidationMap.empty

        val itcValidator: Validator = info =>
          if itcFor(info.oid).isDefined || info.isVisitor || info.isExchange then ObservationValidationMap.empty
          else ObservationValidationMap.singleton(ObservationValidation.itc("ITC results are not present."))

        // An acquisition-capable mode whose acquisition ITC could not be produced
        // (a cached deterministic failure) carries an ItcError.  Pre-execution this
        // maps to Undefined and blocks Ready; during execution the frozen snapshot
        // is present and execution-state dominance keeps the observation Ongoing,
        // so it is a non-blocking standing error there.
        val acquisitionValidator: Validator = info =>
          if info.isVisitor then ObservationValidationMap.empty
          else itcFor(info.oid).foldMap:
            _.acquisition match
              case ItcAcquisition.Failed(msg) => ObservationValidationMap.singleton(ObservationValidation.itc(msg))
              case _                          => ObservationValidationMap.empty

        // V magnitudes are used by Observe to set the GHOST slit viewing
        // camera exposure time, so every target in a GHOST observation needs one.
        val ghostVMagnitudeValidator: Validator = info =>
          if info.observingMode.contains(ObservingModeType.GhostIfu) && info.asterism.exists(!_.sourceProfile.hasBand(Band.V)) then
            ObservationValidationMap.singleton(ObservationValidation.configuration(Messages.MissingVMagnitude))
          else ObservationValidationMap.empty

        val otherConfigErrors: Validator = info =>
          NonEmptyChain.fromSeq(info.otherConfigErrors) match
            case None       => ObservationValidationMap.empty
            case Some(errs) => ObservationValidationMap.singleton(ObservationValidation(ObservationValidationCode.ConfigurationError, errs))

        // Here are our composed validators

        val calibrationValidator, engValidator: Validator = _ =>
          ObservationValidationMap.empty

        val scienceValidator1: Validator =
          generatorValidator         |+|
          cfpInstrumentValidator     |+|
          exchangeValidator          |+|
          cfpRaDecValidator          |+|
          bandValidator              |+|
          ghostVMagnitudeValidator   |+|
          tooActivationValidator     |+|
          opportunityTargetValidator |+|
          otherConfigErrors

        val scienceValidator2: Validator =
          itcValidator |+| acquisitionValidator

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
            info.isAccepted && !info.isExchange && prelimV.get(info.oid).forall(_.isEmpty)

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
      ): Map[Observation.Id, ObservationWorkflow] =
        infos
          .toList
          .map: (oid, info) =>
            val obsErrors = errs.get(oid).toList.flatMap(_.toList)
            val (s, ss) =workflowStateAndTransitions(info, execs.get(oid), obsErrors.map(_.code))
              // .map: (s, ss) =>
            oid -> ObservationWorkflow(s, ss, obsErrors)
          .toMap

      override def getWorkflows(
        oids: List[Observation.Id]
      )(using NoTransaction[F], SuperUserAccess): F[Result[Map[Observation.Id, ObservationWorkflow]]] =
        ResultT(getWorkflowsModesAndRoles(oids))
          .map: m =>
            m.view.mapValues(p => p._1).toMap
          .value

      override def getWorkflowsModesAndRoles(
        oids: List[Observation.Id]
      )(using NoTransaction[F], SuperUserAccess): F[Result[Map[Observation.Id, (ObservationWorkflow, Option[ObservingModeType], Option[CalibrationRole])]]] =

        // Data obtained from the database, requiring a transaction.
        val select: F[Result[(
          Map[Observation.Id, ObservationValidationInfo],
          Map[Observation.Id, ObservationValidationMap],
          Map[Observation.Id, Itc]
        )]] =
          services.transactionally:
            (
              for
                infos  <- ResultT.liftF(ObservationValidationInfo.fetch(oids))         // Map[Observation.Id, ObsDefinition]
                itcRes <- ResultT.liftF(lookupCachedItcResults(infos))      // Map[Observation.Id, ItcService.AsterismResults]
                errs   <- validateObsDefinition(infos, itcRes.get)          // Map[Observation.Id, ObservationValidationMap]
              yield (infos, errs, itcRes)
            ).value

        (for
          (infos, errs, itcRes) <- ResultT(select)
          errorFree              = infos.view.filterKeys(oid => errs.get(oid).forall(_.isEmpty)).toMap
          execs                  = executionStates(errorFree)
          workflows              = computeWorkflows(infos, errs, execs)
          withModes =
            workflows.map:
              case (oid, wf) =>
                oid -> (wf, infos.get(oid).flatMap(_.observingMode), infos.get(oid).flatMap(_.calibrationRole))
        yield withModes).value

      override def getWorkflow(
        oid: Observation.Id
      )(using NoTransaction[F], SuperUserAccess): F[Result[ObservationWorkflow]] =
        getWorkflows(List(oid)).map: result =>
          result.flatMap: map =>
            map.get(oid) match
              case Some(wf) => Result(wf)
              case None     => OdbError.InvalidObservation(oid, Some(s"Could not compute workflow for $oid.")).asFailure

      override def getCalculatedWorkflow(
        oid:  Observation.Id,
        itc:  Option[Itc],
        exec0: Option[CoreExecutionState]
      )(using Transaction[F]): F[Result[ObservationWorkflow]] =
        (for
          infos <- ResultT.liftF(ObservationValidationInfo.fetch(List(oid)))
          errs  <- validateObsDefinition(infos, _ => itc)
          exec = exec0.filter:
            case a: DeclaredExecutionState => true // always ok
            case _ => !infos.get(oid).exists(i => i.isVisitor || i.isExchange) // otherwise discard the state if it's a visitor or exchange
          wfExec = exec.flatMap[ExecutionState](ces => ces.workflowExecutionState)
          execs  = wfExec.fold[Map[Observation.Id, ExecutionState]](Map.empty)(es => Map(oid -> es))
          wfs    = computeWorkflows(infos, errs, execs)
          res   <- ResultT.fromResult(Result.fromOption(wfs.get(oid), s"Invalid observation: $oid"))
        yield res).value

      override def getWorkflows(
        pid: Program.Id
      )(using NoTransaction[F], SuperUserAccess): F[Result[Map[Observation.Id, ObservationWorkflow]]] =
        services
          .transactionally:
            session.prepareR(Statements.selectObservationIds).use: pq =>
              pq.stream(pid, 1024).compile.toList
          .flatMap(getWorkflows)

      extension (self: ObservingModeType) def isVisitorMode: Boolean =
        self match
          case _: VisitorObservingModeType => true
          case _ => false

      override def setWorkflowState(
        input: AccessControl.CheckedWithId[(Option[ObservingModeType], Option[CalibrationRole], ObservationWorkflow, ObservationWorkflowState), Observation.Id]
      )(using NoTransaction[F]): F[Result[ObservationWorkflow]] =
        input.foldWithId(OdbError.InvalidArgument().asFailureF):
          case ((mode, calibrationRole, w, state), oid) =>
            (
              if w.state === state then ResultT.success(w)
              else ResultT:
                services.transactionally:

                  def updateUserState(oid: Observation.Id, state: Option[UserState]): F[Unit] =
                    session.prepareR(Statements.UpdateUserState).use: pc =>
                      pc.execute(state, oid).void

                  def updateDeclaredState(oid: Observation.Id, state: Option[ExecutionState]): F[Unit] =
                    session.prepareR(Statements.UpdateDeclaredState).use: pc =>
                      val es: Option[DeclaredExecutionState] = state.map:
                        case Ongoing   => CoreExecutionState.DeclaredOngoing
                        case Completed => CoreExecutionState.DeclaredComplete
                      pc.execute(es, oid).void

                  (w.state, state) match

                    // Only for visitors
                    case (Ready, Ongoing) =>
                      updateUserState(oid, None) >>
                      updateDeclaredState(oid, Some(Ongoing))
                        .as(Result(w.copy(state = state)))

                    // Only for visitors
                    case (Ongoing, Ready)     =>
                      updateUserState(oid, Some(Ready)) >>
                      updateDeclaredState(oid, None)
                        .as(Result(w.copy(state = state)))

                    // Everyone, but logic differs for visitors
                    case (Completed, Ongoing) =>
                      val ds: Option[ExecutionState] =
                        if mode.exists(_.isVisitorMode) then Some(Ongoing) else None
                      updateDeclaredState(oid, ds)
                        .as(Result(w.copy(state = state)))

                    // Same for everyone
                    case (Ongoing, Completed) =>
                      updateDeclaredState(oid, Some(Completed))
                        .as(Result(w.copy(state = state)))

                    // Reinstating a declined telluric clears its override so it
                    // resumes inheriting its science observation's state.
                    case (Inactive, Ready) if calibrationRole.contains(CalibrationRole.Telluric) =>
                      updateUserState(oid, None)
                        .as(Result(w.copy(state = state)))

                    // Same for everyone; note that this needs to be the last case
                    case (a, b) if a.isUserState || b.isUserState =>
                      updateUserState(oid, b.asUserState)
                        .as(Result(w.copy(state = state)))

                    case _ =>
                      Result.internalError(s"Transition from ${w.state} to $state was not expected.").pure[F]

            ).value

      extension (wf: ObservationWorkflow) def isCompatibleWith(states: Set[ObservationWorkflowState]): Boolean =
        // An allowed transition from ongoing to completed [via declared completion] shouldn't prevent editing,
        // even though editing will be disabled if the transition is taken.
        (wf.state :: wf.validTransitions.filterNot(_ === ObservationWorkflowState.Completed)).forall(states.contains)

      override def filterState(
        oids: List[Observation.Id],
        states: Set[ObservationWorkflowState]
      )(using NoTransaction[F], SuperUserAccess): F[Result[List[Observation.Id]]] =
        getWorkflows(oids)
          .map: res =>
            res.flatMap: wfs =>
              oids.foldLeft(Result(Nil)): (r, oid) =>
                wfs.get(oid) match
                  case None => r.withProblems(OdbError.InvalidObservation(oid).asProblemNec)
                  case Some(wf) =>
                    if wf.isCompatibleWith(states) then r.map(oid :: _)
                    else r.withProblems:
                      val prefix = s"Observation $oid is ineligible for this operation due to its workflow state (${wf.state}"
                      val suffix = if wf.validTransitions.isEmpty then ")." else s" with allowed transition to ${wf.validTransitions.mkString("/")})."
                      OdbError.InvalidObservation(oid, (prefix + suffix).some)
                        .asProblemNec

      override def filterState(
        which: AppliedFragment,
        states: Set[ObservationWorkflowState]
      )(using NoTransaction[F], SuperUserAccess): F[Result[List[Observation.Id]]] =
        services
          .transactionally:
            session.prepareR(which.fragment.query(observation_id)).use: pq =>
              pq.stream(which.argument, chunkSize = 1024).compile.toList
          .flatMap: oids =>
            filterState(oids, states)

      private def getObservationsForTargets(whichTargets: AppliedFragment)(using NoTransaction[F]): F[Map[Target.Id, List[Observation.Id]]] =
        services.transactionally:
          val af = Statements.selectObservationsForTargets(whichTargets)
          session.prepareR(af.fragment.query(target_id *: observation_id.opt)).use: pq =>
            pq.stream(af.argument, 1024).compile.toList.map: list =>
              list.groupMap(_._1)(_._2).view.mapValues(_.flatten).toMap

      override def filterTargets(
        which: AppliedFragment,
        states: Set[ObservationWorkflowState]
      )(using NoTransaction[F], SuperUserAccess): F[Result[List[Target.Id]]] =
        getObservationsForTargets(which)
          .flatMap: map =>
            getWorkflows(map.values.toList.flatten)
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

    val UpdateUserState: Command[(Option[UserState], Observation.Id)] =
      sql"""
        UPDATE t_observation
        SET c_workflow_user_state = ${user_state.opt}
        WHERE c_observation_id = $observation_id
      """.command

    val UpdateDeclaredState: Command[(Option[DeclaredExecutionState], Observation.Id)] =
      sql"""
        UPDATE t_observation
        SET c_declared_state = ${declared_execution_state.opt}
        WHERE c_observation_id = $observation_id
      """.command

    val UpdateDeclaredOngoing: Command[(Boolean, Observation.Id)] =
      sql"""
        UPDATE t_observation
        SET c_declared_ongoing = $bool
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
