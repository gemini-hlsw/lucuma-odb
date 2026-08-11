// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service
package workflow

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.implicits.*
import grackle.ResultT
import lucuma.core.enums.Band
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.Observatory
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TooActivation
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.StandardRole.*
import lucuma.core.syntax.string.*
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInputDerivation
import lucuma.odb.sequence.data.MissingParamSet
import lucuma.odb.service.GeneratorParamsService.Error as GenParamsError
import ObservationWorkflowState.*

import Services.Syntax.*
import cats.Applicative

object ObservationValidator:

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

  extension (mp: MissingParamSet)
    private def toObsValidation: ObservationValidation =
      ObservationValidation.configuration(s"Missing ${mp.params.map(_.name).toList.intercalate(", ")}")

  extension (ge: GeneratorParamsService.Error)
    private def toObsValidation: ObservationValidation = ge match
      case GenParamsError.MissingData(p) => p.toObsValidation
      case _                             => ObservationValidation.configuration(ge.format)

  def validate[F[_]: Applicative](
    infos:  Map[Observation.Id, ObservationValidationInfo],
    itcFor: Observation.Id => Option[Itc]
  )(using Services[F]): ResultT[F, Map[Observation.Id, ObservationValidationMap]] = {

    type Validator = ObservationValidationInfo => ObservationValidationMap

    val (cals, other)         = infos.partition(_._2.calibrationRole.isDefined)
    val (nonScience, science) = other.partition(!_._2.tpe.hasProposal)

    def validateConfigurations(infos: NonEmptyList[ObservationValidationInfo]): ResultT[F, Map[Observation.Id, ObservationValidationMap]] =
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
