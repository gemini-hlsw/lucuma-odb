// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package flamingos2
package longslit

import cats.Monad
import cats.data.EitherT
import fs2.Pure
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2Dynamic
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig as F2Static
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.StreamingExecutionConfig

import java.util.UUID

object LongSlit:

  /** Maximum time that may pass between "Nighttime Calibrations" atoms. */
  val MaxSciencePeriod: TimeSpan =
    90.minuteTimeSpan

  def instantiate[F[_]: Monad](
    observationId:  Observation.Id,
    estimator:      StepTimeEstimateCalculator[F2Static, F2Dynamic],
    namespace:      UUID,
    expander:       SmartGcalExpander[F, F2Static, F2Dynamic],
    config:         Config,
    acquisitionItc: Either[OdbError, IntegrationTime],
    scienceItc:     Either[OdbError, IntegrationTime],
    calRole:        Option[CalibrationRole]
  ): F[Either[OdbError, StreamingExecutionConfig[Pure, F2Static, F2Dynamic]]] =
    (for
       a <- EitherT.fromEither(Acquisition.instantiate(observationId, estimator, Static, namespace, config, acquisitionItc))
       s <- EitherT(spectroscopy.Science.instantiate(observationId, estimator, Static, namespace, expander, ObservingMode.Flamingos2LongSlitName, MaxSciencePeriod, config, scienceItc, calRole))
    yield StreamingExecutionConfig(Static, a.generate, s.generate)).value
