// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.SignalToNoise
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.TelluricExposureTimeModeEdit
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

  def notATelluric(oids: List[Observation.Id]): OdbError =
    OdbError.InvalidArgument(
      (TelluricExposureTimeModeEdit.NullOnlyOnTelluric +
        s" Not a telluric: ${oids.mkString(", ")}.").some
    )

  /** Queues a recalculation of the science observation behind each telluric. */
  def requeue[F[_]: {Concurrent, Services}](
    tellurics: List[Observation.Id]
  )(using Transaction[F]): F[Unit] =
    calibrationCalcService.telluricScience(tellurics).flatMap(requeueScience)

  private def requeueScience[F[_]: {Concurrent, Services}](
    tellurics: Map[Observation.Id, (Observation.Id, Program.Id)]
  )(using Transaction[F]): F[Unit] =
    tellurics.values.toList.distinct.traverse_((s, p) => calibrationCalcService.invalidate(s, p))

  /**
   * Applies a science exposure time mode edit: a value sets the rows explicit, null reverts
   * the tellurics to derived, and an absent edit does nothing.
   */
  def updateScience[F[_]: {Concurrent, Services}](
    which: List[Observation.Id],
    etm:   Nullable[ExposureTimeMode]
  )(using Transaction[F]): F[Result[Unit]] =
    etm.fold(
      revert(which),
      Result.unit.pure[F],
      e => exposureTimeModeService.updateMany(which, ExposureTimeModeRole.Science, e).as(Result.unit)
    )

  /** Reverts every science row of the given observations, which must all be tellurics. */
  def revert[F[_]: {Concurrent, Services}](
    which: List[Observation.Id]
  )(using Transaction[F]): F[Result[Unit]] =
    calibrationCalcService.telluricScience(which).flatMap: tellurics =>
      val others = which.filterNot(tellurics.contains)
      if others.nonEmpty then notATelluric(others).asFailureF
      else
        exposureTimeModeService
          .setDerived(tellurics.keys.toList, ExposureTimeModeRole.Science, DerivedSignalToNoise) *>
          requeueScience(tellurics).as(Result.unit)
