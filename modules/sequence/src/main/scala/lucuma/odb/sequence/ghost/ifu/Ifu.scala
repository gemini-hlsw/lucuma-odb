// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package ghost.ifu

import cats.Monad
import cats.data.EitherT
import fs2.Pure
import fs2.Stream
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.odb.data.Itc.GhostIfu
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

object Ifu:

  def instantiate[F[_]: Monad](
    estimator: StepTimeEstimateCalculator[GhostStaticConfig, GhostDynamicConfig],
    static:    GhostStaticConfig,
    namespace: UUID,
    config:    Config,
    itc:       Either[OdbError, GhostIfu]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, GhostStaticConfig, GhostDynamicConfig]]] =
    (for
      g <- EitherT.fromEither(itc)
      r  = g.red.focus.value
      b  = g.blue.focus.value
      s <- EitherT(Science.instantiate(estimator, static, namespace, config, r, b))
    yield StreamingExecutionConfig(static, Stream.empty, s.generate)).value