// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import cats.Order.catsKernelOrderingForOrder
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Problem
import grackle.Result
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ExposureTimeModeType
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*

trait ToExposureTimeModeOps:

  extension (etm: ExposureTimeMode)
    def modeType: ExposureTimeModeType =
      etm match
        case ExposureTimeMode.SignalToNoiseMode(_, _)   => ExposureTimeModeType.SignalToNoiseMode
        case ExposureTimeMode.TimeAndCountMode(_, _, _) => ExposureTimeModeType.TimeAndCountMode

    def signalToNoise: Option[SignalToNoise] =
      ExposureTimeMode
        .signalToNoise
        .andThen(ExposureTimeMode.SignalToNoiseMode.value)
        .getOption(etm)

    def exposureTime: Option[TimeSpan] =
      ExposureTimeMode
        .timeAndCount
        .andThen(ExposureTimeMode.TimeAndCountMode.time)
        .getOption(etm)

    def exposureCount: Option[PosInt] =
      ExposureTimeMode
        .timeAndCount
        .andThen(ExposureTimeMode.TimeAndCountMode.count)
        .getOption(etm)

  extension ($: ExposureTimeMode.type)
    def forAcquisition(at: Wavelength): ExposureTimeMode =
      ExposureTimeMode.SignalToNoiseMode(
        SignalToNoise.FromBigDecimalExact.getOption(10).get,
        at
      )

    def forSingleScienceObservingModes(
      observingModeName:   String,
      explicitAcquisition: Option[ExposureTimeMode],
      explicitScience:     Option[ExposureTimeMode],
      newRequirement:      Option[ExposureTimeMode],
      curRequirements:     Map[Observation.Id, ExposureTimeMode],
      which:               List[Observation.Id]
    ): Result[Map[Observation.Id, (ExposureTimeMode, ExposureTimeMode)]] =
      val sci =
        which
          .fproduct: oid =>
            explicitScience orElse newRequirement orElse curRequirements.get(oid)
          .collect:
            case (oid, Some(etm)) => oid -> etm
          .toMap

      val missing = which.toSet -- sci.keySet

      def success: Map[Observation.Id, (ExposureTimeMode, ExposureTimeMode)] =
        sci.fproductLeft: etm =>
          explicitAcquisition.getOrElse(forAcquisition(etm.at))

      def error: Problem =
        val oids = missing.toList.sorted.mkString(", ")
        val msg  = s"${observingModeName} requires a science exposure time mode.  Missing in observations: $oids"
        OdbError.InvalidArgument(msg.some).asProblem

      Result.fromOption(Option.when(missing.isEmpty)(success), error)

object exposureTimeMode extends ToExposureTimeModeOps