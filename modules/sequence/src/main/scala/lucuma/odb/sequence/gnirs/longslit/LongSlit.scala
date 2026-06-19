// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs.longslit

import cats.Monad
import cats.data.EitherT
import fs2.Pure
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.odb.data.Itc
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.gnirs.InitialConfigs

import java.util.UUID

object LongSlit:

  def staticFrom(config: Config): GnirsStaticConfig =
    InitialConfigs.staticFrom(config)

  def instantiate[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    namespace:     UUID,
    expander:      SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig],
    config:        Config,
    itc:           Either[OdbError, Itc.Spectroscopy],
    calRole:       Option[CalibrationRole]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, GnirsStaticConfig, GnirsDynamicConfig]]] =
    val static = staticFrom(config)
    // Daytime pinhole flats have no acquisition sequence.
    val acquisition: Either[OdbError, SequenceGenerator[GnirsDynamicConfig]] =
      if calRole.contains(CalibrationRole.DaytimePinhole) then Right(SequenceGenerator.empty[GnirsDynamicConfig])
      else Acquisition.instantiate(
             observationId, estimator, static, namespace, config,
             itc.map(_.acquisition.focus.value)
           )
    (for
      a <- EitherT.fromEither(acquisition)
      s <- EitherT(Science.instantiate(
             observationId, estimator, static, namespace, expander, config,
             itc.map(_.science.focus.value), calRole
           ))
    yield StreamingExecutionConfig(static, a.generate, s.generate)).value
