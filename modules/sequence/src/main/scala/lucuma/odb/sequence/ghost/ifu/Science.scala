// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package ghost
package ifu

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.option.*
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * GHOST IFU sequence generation.
 */
object Science:

  def instantiate[F[_]: Monad](
    estimator:     StepTimeEstimateCalculator[GhostStaticConfig, GhostDynamicConfig],
    static:        GhostStaticConfig,
    namespace:     UUID,
    @annotation.unused expander: SmartGcalExpander[F, GhostStaticConfig, GhostDynamicConfig],  // not yet used
    config:        Config,
    red:           IntegrationTime,
    blue:          IntegrationTime
  ): F[Either[OdbError, SequenceGenerator[GhostDynamicConfig]]] =
    val generator = new SequenceGenerator[GhostDynamicConfig]:
      def generate: Stream[Pure, Atom[GhostDynamicConfig]] =

        val redDetector = GhostDetector(
          red.exposureTime,
          red.exposureCount,
          config.red.value.binning,
          config.red.value.readMode
        ).asRed

        val blueDetector = GhostDetector(
          blue.exposureTime,
          blue.exposureCount,
          config.blue.value.binning,
          config.blue.value.readMode
        ).asBlue

        val instrumentConfig = GhostDynamicConfig(
          redDetector,
          blueDetector,
          config.ifu1Agitator,
          config.ifu2Agitator
        )

        val protoAtom = ProtoAtom(
          none,
          NonEmptyList.one:
            ProtoStep(
              instrumentConfig,
              StepConfig.Science,
              TelescopeConfig(Offset.Zero, StepGuideState.Enabled),
              ObserveClass.Science
            )
        )

        AtomBuilder
          .instantiate(estimator, static, namespace, SequenceType.Science)
          .buildStream(Stream.emits(List.fill(config.stepCount.value)(protoAtom)))

    generator.asRight[OdbError].pure[F]