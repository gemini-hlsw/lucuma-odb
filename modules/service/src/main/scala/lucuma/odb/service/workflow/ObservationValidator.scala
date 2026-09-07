// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service
package workflow

import cats.Applicative
import cats.Functor
import cats.Monoid
import cats.data.NonEmptyList
import cats.implicits.*
import grackle.ResultT
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.StandardRole.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Itc
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.service.ConfigurationService.AvailabilityShortfall
import lucuma.odb.service.ConfigurationService.RequestCoverage

import Services.Syntax.*

trait ObservationValidator extends (ObservationValidationInfo => ObservationValidationMap):
  def apply(info: ObservationValidationInfo): ObservationValidationMap

object ObservationValidator:

  given Monoid[ObservationValidator]:
    def empty = _ => ObservationValidationMap.empty
    def combine(x: ObservationValidator, y: ObservationValidator): ObservationValidator = a => x(a) |+| y(a)

  private def validateConfigurations[F[_]: Functor](
    infos: NonEmptyList[ObservationValidationInfo]
  )(using Services[F]): ResultT[F, Map[Observation.Id, ObservationValidationMap]] =
    ResultT(configurationService.selectRequestCoverage(infos.toList.map(i => (i.pid, i.oid)))).map: rs =>
      rs.view
        .map:
          case ((_, oid), RequestCoverage(lst, shortfall)) =>
            oid -> {
              val m = ObservationValidationMap.empty
              if lst.isEmpty then m.add(notRequested(shortfall))
              else if lst.exists(_.status === ConfigurationRequestStatus.Approved) then m
              else if lst.forall(_.status === ConfigurationRequestStatus.Denied) then m.add(ObservationValidation.configurationRequestDenied)
              else m.add(ObservationValidation.configurationRequestPending)
            }
        .toMap

  /**
   * Nothing subsumes the observation, so approval has not been requested for the
   * configuration it now has.  That is true whichever dimension moved, but the
   * stock message names no cause, and availability is the one dimension a PI can
   * trip without touching anything they would recognize as the science: trim a
   * timing window and the observation quietly stops being approved.  So when
   * availability is the only thing standing in the way, say so, with the bar and
   * what was offered.
   */
  private def notRequested(shortfall: Option[AvailabilityShortfall]): ObservationValidation =
    shortfall.fold(ObservationValidation.configurationRequestNotRequested): s =>
      ObservationValidation.fromMsgs(
        ObservationValidationCode.ConfigurationRequestNotRequested,
        ObservationValidationCode.ConfigurationRequestNotRequested.description,
        s"Less available than approved: open for ${describe(s.actual)}, where ${describe(s.required)} is needed."
      )

  /**
   * Availability runs from hours to months, so say it the way a proposal would
   * rather than as an ISO-8601 duration.
   */
  private def describe(ts: TimeSpan): String =
    val totalHours = ts.toHours.toLong
    val days       = totalHours / 24
    val hours      = totalHours % 24
    def plural(n: Long, unit: String) = s"$n $unit${if n === 1L then "" else "s"}"
    (Option.when(days > 0)(plural(days, "day")).toList ++ Option.when(hours > 0)(plural(hours, "hour")).toList) match
      case Nil   => "less than an hour"
      case parts => parts.mkString(" ")

  def validate[F[_]: Applicative](
    infos:  Map[Observation.Id, ObservationValidationInfo],
    itcFor: Observation.Id => Option[Itc]
  )(using Services[F]): ResultT[F, Map[Observation.Id, ObservationValidationMap]] = {

    val (cals, other)         = infos.partition(_._2.calibrationRole.isDefined)
    val (nonScience, science) = other.partition(!_._2.tpe.hasProposal)

    // Here are our simple validators
    import validator.*

    // Here are our composed validators

    val calibrationValidator, engValidator: ObservationValidator = _ =>
      ObservationValidationMap.empty

    val scienceValidator1: ObservationValidator =
      GeneratorValidator             |+|
      CfpInstrumentValidator         |+|
      ExchangeValidator              |+|
      CfpRaDecValidator              |+|
      BandValidator                  |+|
      GhostVMagnitudeValidator       |+|
      TooActivationValidator         |+|
      OpportunityTargetValidator     |+|
      OtherConfigErrorValidator      |+|
      ConditionsProbabilityValidator

    val scienceValidator2: ObservationValidator =
      ItcValidator(itcFor)                |+| 
      AcquisitionValidator(itcFor)        |+| 
      TotalSignalToNoiseValidator(itcFor)

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
