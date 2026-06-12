// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package imaging

import cats.data.EitherT
import cats.effect.Sync
import fs2.Pure
import lucuma.core.enums.ImagingVariantType
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.odb.data.Itc.Flamingos2Imaging
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

object Imaging:

  private def instantiate[F[_]: Sync](
    static:  Flamingos2StaticConfig,
    science: F[Either[OdbError, SequenceGenerator[Flamingos2DynamicConfig]]]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, Flamingos2StaticConfig, Flamingos2DynamicConfig]]] =
    EitherT(science)
      // Note acquisition is empty for imaging
      .map(s => StreamingExecutionConfig(static, SequenceGenerator.empty.generate, s.generate))
      .value

  private val BaseStatic: Flamingos2StaticConfig =
    Flamingos2StaticConfig(
      mosPreImaging           = MosPreImaging.IsNotMosPreImaging,
      useElectronicOffsetting = false
    )

  def flamingos2[F[_]: Sync](
    estimator:  StepTimeEstimateCalculator[Flamingos2StaticConfig, Flamingos2DynamicConfig],
    namespace:  UUID,
    config:     Config,
    scienceItc: Either[OdbError, Flamingos2Imaging]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, Flamingos2StaticConfig, Flamingos2DynamicConfig]]] =
    val static = config.variant.variantType match
      case ImagingVariantType.PreImaging => BaseStatic.copy(mosPreImaging = MosPreImaging.IsMosPreImaging)
      case _                             => BaseStatic

    instantiate(
      static,
      Science.flamingos2(estimator, static, namespace, config, scienceItc)
    )
