// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service
package workflow

import cats.Applicative
import cats.Functor
import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.ResultT
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.Itc
import lucuma.odb.data.ObservationValidationMap
import skunk.Transaction

import Services.Syntax.*

type ObservationValidator = ObservationValidationInfo => ObservationValidationMap

object ObservationValidator:

  def validate[F[_]: Applicative](
    infos:  Map[Observation.Id, ObservationValidationInfo],
    itcFor: Observation.Id => Option[Itc]
  )(using Transaction[F], Services[F]): ResultT[F, Map[Observation.Id, ObservationValidationMap]] = {

    // Partition observations into cals, engineering, and science
    val (cals, other)  = infos.partition(_._2.role.isDefined)
    val (eng, science) = other.partition(!_._2.tpe.hasProposal)

    // Here are our many simple validators
    import validator.*

    // Here are our composed validators

    // For now calibration and engineering obs have no validators
    val calibrationValidator, engValidator: ObservationValidator = _ =>
      ObservationValidationMap.empty

    // Science is broken into two stages. Any errors here mean there's no point
    // validating ITC and acquisition.
    val scienceValidator1: ObservationValidator =
      generatorValidator       |+|
      cfpInstrumentValidator   |+|
      exchangeValidator        |+|
      cfpRaDecValidator        |+|
      bandValidator            |+|
      ghostVMagnitudeValidator |+|
      otherConfigErrorValidator

    // Only checked if stage 1 above succeeds.
    val scienceValidator2: ObservationValidator =
      itcValidator(itcFor) |+| acquisitionValidator(itcFor)

    // And our validation results

    val engResults: Map[Observation.Id, ObservationValidationMap] =
      eng.view.mapValues(engValidator).toMap

    val calibrationResults: Map[Observation.Id, ObservationValidationMap] =
      cals.view.mapValues(calibrationValidator).toMap

    val scienceResults1: Map[Observation.Id, ObservationValidationMap] =
      science.view.mapValues(scienceValidator1).toMap

    val scienceResults2: Map[Observation.Id, ObservationValidationMap] =
      science
        .view
        .filterKeys(k => scienceResults1.get(k).forall(_.nonFatal)) // ensure there are no errors in stage 1
        .mapValues(scienceValidator2)
        .toMap

    val prelimV: Map[Observation.Id, ObservationValidationMap] =
      calibrationResults |+| engResults |+| scienceResults1 |+| scienceResults2

    val toCheck: List[ObservationValidationInfo] =
      science.values.toList.filter: info =>
        info.isAccepted && !info.isExchange && prelimV.get(info.oid).forall(_.nonFatal)

    val configValidations: ResultT[F, Map[Observation.Id, ObservationValidationMap]] =
      NonEmptyList
        .fromList(toCheck)
        .fold(ResultT.pure(Map.empty[Observation.Id, ObservationValidationMap]))(validateConfigurations)

    configValidations.map(prelimV |+| _)

  }

  @annotation.nowarn("msg=unused implicit parameter")
  private def validateConfigurations[F[_]: Functor](infos: NonEmptyList[ObservationValidationInfo])(using Transaction[F], Services[F]): ResultT[F, Map[Observation.Id, ObservationValidationMap]] =
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

