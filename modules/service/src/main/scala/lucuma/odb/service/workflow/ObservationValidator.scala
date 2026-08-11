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
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.StandardRole.*
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ObservationValidationMap

import Services.Syntax.*

trait ObservationValidator extends (ObservationValidationInfo => ObservationValidationMap):
  def apply(info: ObservationValidationInfo): ObservationValidationMap

object ObservationValidator:

  given Monoid[ObservationValidator]:
    def empty = _ => ObservationValidationMap.empty
    def combine(x: ObservationValidator, y: ObservationValidator): ObservationValidator = a => x(a) |+| y(a)

  /* Validation Messages */
  object Messages {


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
      TooActivationValidator     |+|
      OpportunityTargetValidator |+|
      otherConfigErrors

    val scienceValidator2: ObservationValidator =
      ItcValidator(itcFor) |+| acquisitionValidator

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
