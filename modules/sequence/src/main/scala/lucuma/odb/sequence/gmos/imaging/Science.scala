// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package imaging

import cats.Applicative
import cats.data.NonEmptyMap
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import fs2.Pure
import fs2.Stream
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.data.Zipper
import lucuma.core.util.Timestamp
import lucuma.odb.data.Itc.GmosNorthImaging
import lucuma.odb.data.Itc.GmosSouthImaging
import lucuma.odb.data.Itc.Result
import lucuma.odb.data.OdbError

import java.util.UUID

object Science:

  private case class ScienceGenerator[S, D, A](
      observationId: Observation.Id,
      estimator:     TimeEstimateCalculator[S, D],
      static:        S,
      namespace:     UUID,
      config:        Config[A],
      time:          Either[OdbError, NonEmptyMap[A, Zipper[Result]]]
  ) extends SequenceGenerator.Base[D]:

    override def generate(timestamp: Timestamp): Stream[Pure, Atom[D]] =
      config.variant match {
        case Variant.Grouped(order, offsets, skyCount, skyOffsets)  => ???
        case Variant.Interleaved(offsets, skyCount, skyOffsets)     => ???
        case Variant.PreImaging(offset1, offset2, offset3, offset4) => ???
      }


  object ScienceGenerator:
    def instantiate[F[_]: Applicative, S, D, A](
      observationId: Observation.Id,
      estimator:     TimeEstimateCalculator[S, D],
      static:        S,
      namespace:     UUID,
      config:        Config[A],
      time:          Either[OdbError, NonEmptyMap[A, Zipper[Result]]]
    ): F[Either[OdbError, SequenceGenerator[D]]] =
//       TODO: maybe this doesn't need an effect or an error case ??

      val cnt = time.fold(
        _ => NonNegInt.MinValue,
        m => NonNegInt.unsafeFrom(m.toNel.toList.map(_._2.focus._2.exposureCount.value).max)
      )

      config.variant match {
        case Variant.Grouped(_, offsets, skyCount, skyOffsets)      => ???
        case Variant.Interleaved(offsets, skyCount, skyOffsets)     => ???
        case Variant.PreImaging(offset1, offset2, offset3, offset4) => ???
      }

      Right(ScienceGenerator(observationId, estimator, static, namespace, config, time)).pure[F]

  def gmosNorth[F[_]: Applicative](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosNorth, GmosNorth],
    static:        StaticConfig.GmosNorth,
    namespace:     UUID,
    config:        Config.GmosNorth,
    time:          Either[OdbError, GmosNorthImaging]
  ): F[Either[OdbError, SequenceGenerator[GmosNorth]]] =
    ScienceGenerator.instantiate(observationId, estimator, static, namespace, config, time.map(_.science))

  def gmosSouth[F[_]: Applicative](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosSouth, GmosSouth],
    static:        StaticConfig.GmosSouth,
    namespace:     UUID,
    config:        Config.GmosSouth,
    time:          Either[OdbError, GmosSouthImaging]
  ): F[Either[OdbError, SequenceGenerator[GmosSouth]]] =
    ScienceGenerator.instantiate(observationId, estimator, static, namespace, config, time.map(_.science))
