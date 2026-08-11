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
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.StandardRole.*
import lucuma.odb.data.Itc
import lucuma.odb.data.ObservationValidationMap

import Services.Syntax.*

trait ObservationValidator extends (ObservationValidationInfo => ObservationValidationMap):
  def apply(info: ObservationValidationInfo): ObservationValidationMap

object ObservationValidator:

  given Monoid[ObservationValidator]:
    def empty = _ => ObservationValidationMap.empty
    def combine(x: ObservationValidator, y: ObservationValidator): ObservationValidator = a => x(a) |+| y(a)

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
      GhostVMagnitudeValidator   |+|
      TooActivationValidator     |+|
      OpportunityTargetValidator |+|
      otherConfigErrors

    val scienceValidator2: ObservationValidator =
      ItcValidator(itcFor) |+| AcquisitionValidator(itcFor)

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
