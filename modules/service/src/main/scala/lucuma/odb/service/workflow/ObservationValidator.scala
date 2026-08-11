// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service
package workflow

import cats.Applicative
import cats.Monoid
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.implicits.*
import grackle.ResultT
import lucuma.core.enums.Band
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.TooActivation
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.StandardRole.*
import lucuma.core.syntax.string.*
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ObservationValidationMap

import ObservationWorkflowState.*
import Services.Syntax.*

trait ObservationValidator extends (ObservationValidationInfo => ObservationValidationMap):
  def apply(info: ObservationValidationInfo): ObservationValidationMap

object ObservationValidator:

  given Monoid[ObservationValidator]:
    def empty = _ => ObservationValidationMap.empty
    def combine(x: ObservationValidator, y: ObservationValidator): ObservationValidator = a => x(a) |+| y(a)

  /* Validation Messages */
  object Messages {
    def tooActivationExceedsCeiling(obs: TooActivation, ceiling: TooActivation): String =
      s"Target of Opportunity activation ${obs.tag.toScreamingSnakeCase} exceeds the maximum " +
      s"${ceiling.tag.toScreamingSnakeCase} allowed by the proposal."

    val OpportunityTargetRequiresActivation =
      "An observation with a Target of Opportunity placeholder must set a ToO activation other than NONE."

    val OpportunityTargetNotResolved =
      "Replace the Target of Opportunity placeholder with the actual target coordinates."

    val MissingVMagnitude = "Please add a V magnitude."
  }

  def validate[F[_]: Applicative](
    infos:  Map[Observation.Id, ObservationValidationInfo],
    itcFor: Observation.Id => Option[Itc]
  )(using Services[F]): ResultT[F, Map[Observation.Id, ObservationValidationMap]] = {

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
    import validator.*


    // The Target-of-Opportunity ceiling.  This is an authorization failure
    // rather than a misconfiguration, so it maps to Unapproved -- the
    // observation cannot advance to Ready until the activation is lowered or
    // the proposal's ceiling is raised.
    val tooActivationValidator: ObservationValidator = info =>
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
    val opportunityTargetValidator: ObservationValidator = info =>
      if !info.hasTooTarget then ObservationValidationMap.empty
      else if !info.isTooObservation then
        ObservationValidationMap.singleton(ObservationValidation.configuration(Messages.OpportunityTargetRequiresActivation))
      else if info.effectiveUserState.contains(Ready) then
        ObservationValidationMap.singleton(ObservationValidation.configuration(Messages.OpportunityTargetNotResolved))
      else ObservationValidationMap.empty

    val itcValidator: ObservationValidator = info =>
      if itcFor(info.oid).isDefined || info.isVisitor || info.isExchange then ObservationValidationMap.empty
      else ObservationValidationMap.singleton(ObservationValidation.itc("ITC results are not present."))

    // An acquisition-capable mode whose acquisition ITC could not be produced
    // (a cached deterministic failure) carries an ItcError.  Pre-execution this
    // maps to Undefined and blocks Ready; during execution the frozen snapshot
    // is present and execution-state dominance keeps the observation Ongoing,
    // so it is a non-blocking standing error there.
    val acquisitionValidator: ObservationValidator = info =>
      if info.isVisitor then ObservationValidationMap.empty
      else itcFor(info.oid).foldMap:
        _.acquisition match
          case ItcAcquisition.Failed(msg) => ObservationValidationMap.singleton(ObservationValidation.itc(msg))
          case _                          => ObservationValidationMap.empty

    // V magnitudes are used by Observe to set the GHOST slit viewing
    // camera exposure time, so every target in a GHOST observation needs one.
    val ghostVMagnitudeValidator: ObservationValidator = info =>
      if info.observingMode.contains(ObservingModeType.GhostIfu) && info.asterism.exists(!_.sourceProfile.hasBand(Band.V)) then
        ObservationValidationMap.singleton(ObservationValidation.configuration(Messages.MissingVMagnitude))
      else ObservationValidationMap.empty

    val otherConfigErrors: ObservationValidator = info =>
      NonEmptyChain.fromSeq(info.otherConfigErrors) match
        case None       => ObservationValidationMap.empty
        case Some(errs) => ObservationValidationMap.singleton(ObservationValidation(ObservationValidationCode.ConfigurationError, errs))

    // Here are our composed validators

    val calibrationValidator, engValidator: ObservationValidator = _ =>
      ObservationValidationMap.empty

    val scienceValidator1: ObservationValidator =
      GeneratorValidator         |+|
      CfpInstrumentValidator     |+|
      ExchangeValidator          |+|
      CfpRaDecValidator          |+|
      BandValidator              |+|
      ghostVMagnitudeValidator   |+|
      tooActivationValidator     |+|
      opportunityTargetValidator |+|
      otherConfigErrors

    val scienceValidator2: ObservationValidator =
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
