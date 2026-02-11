// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2.longslit

import cats.Monad
import cats.data.EitherT
import fs2.Pure
import lucuma.core.enums.MosPreImaging
import lucuma.core.model.Observation
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2Dynamic
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig as F2Static
import lucuma.core.util.Timestamp
import lucuma.odb.data.Itc.Spectroscopy
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

object LongSlit:

  val Static: F2Static =
    F2Static(
      mosPreImaging          = MosPreImaging.IsNotMosPreImaging,
      useElectronicOffsetting = false
    )

  def instantiate[F[_]: Monad](
    observationId:  Observation.Id,
    estimator:      TimeEstimateCalculator[F2Static, F2Dynamic],
    namespace:      UUID,
    expander:       SmartGcalExpander[F, F2Dynamic],
    config:         Config,
    itc:            Either[OdbError, Spectroscopy],
    lastAcqReset:   Option[Timestamp]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, F2Static, F2Dynamic]]] =
    (for
       a <- EitherT.fromEither(Acquisition.instantiate(observationId, estimator, Static, namespace, config, itc.map(_.acquisition.focus.value), lastAcqReset))
       s <- EitherT(Science.instantiate(observationId, estimator, Static, namespace, expander, config, itc.map(_.science.focus.value)))
    yield StreamingExecutionConfig(Static, a.generate, s.generate)).value
