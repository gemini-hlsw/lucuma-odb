// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

/**
 * A telluric calibration's science exposure time mode is derived from its science
 * observation unless a user overrides it.  Clearing the override marks the row derived
 * again and queues the science observation's calibrations, whose resync rewrites it.
 */
object TelluricScienceExposureTimeMode:

  /** The S/N a derived telluric row falls back to until the resync rewrites it. */
  val DerivedSignalToNoise: SignalToNoise =
    SignalToNoise.fromInt(100).get

  val NotATelluric: OdbError =
    OdbError.InvalidArgument(
      "A null 'exposureTimeMode' is only valid when editing a telluric calibration.".some
    )

  /** Queues a recalculation of the science observation behind each telluric. */
  def requeue[F[_]: {Concurrent, Services}](
    tellurics: List[Observation.Id]
  )(using Transaction[F]): F[Unit] =
    calibrationCalcService.telluricScience(tellurics).flatMap: science =>
      science.values.toList.distinct.traverse_((s, p) => calibrationCalcService.invalidate(s, p))

  /** Reverts every science row of the given observations, which must all be tellurics. */
  def revert[F[_]: {Concurrent, Services}](
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]] =
    calibrationCalcService.telluricScience(which).flatMap: tellurics =>
      if which.exists(oid => !tellurics.contains(oid)) then NotATelluric.asFailureF
      else
        val science = tellurics.values.toList.distinct
        exposureTimeModeService
          .setDerived(tellurics.keys.toList, ExposureTimeModeRole.Science, DerivedSignalToNoise) *>
          science.traverse_((s, p) => calibrationCalcService.invalidate(s, p)).as(Result.unit)
