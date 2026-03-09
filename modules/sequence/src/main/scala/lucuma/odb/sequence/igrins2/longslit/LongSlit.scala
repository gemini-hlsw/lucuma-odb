// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package igrins2.longslit

import cats.Monad
import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2SVCImages
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

object LongSlit:

  val ABBACycleTitle: NonEmptyString =
    NonEmptyString.unsafeFrom("ABBA Cycle")

  val NodQ: Offset.Q =
    Offset.Q(lucuma.core.math.Angle.fromBigDecimalArcseconds(1.25))

  def staticConfig(config: Config): Igrins2StaticConfig =
    Igrins2StaticConfig(
      saveSVCImages = if config.saveSVCImages then Igrins2SVCImages.Save else Igrins2SVCImages.DontSave,
      offsetMode    = config.offsetMode
    )

  def instantiate[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[Igrins2StaticConfig, Igrins2DynamicConfig],
    namespace:     UUID,
    config:        Config,
    itc:           Either[OdbError, IntegrationTime]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, Igrins2StaticConfig, Igrins2DynamicConfig]]] =

    val static = staticConfig(config)

    val science: Either[OdbError, SequenceGenerator[Igrins2DynamicConfig]] =
      itc.flatMap: time =>
        Either.cond(
          time.exposureTime.toNonNegMicroseconds.value > 0,
          Science.generator(estimator, static, namespace, time),
          OdbError.SequenceUnavailable(
            observationId,
            s"IGRINS-2 Long Slit requires a positive exposure time.".some
          )
        )

    Monad[F].pure:
      science.map: gen =>
        StreamingExecutionConfig(
          static,
          Stream.empty,
          gen.generate
        )
