// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.service.SmartGcalService
import lucuma.odb.smartgcal.data.Flamingos2
import lucuma.odb.smartgcal.data.Gmos

object SmartGcalImplementation {

  type Cache[K, D] = Map[(K, SmartGcalType), Either[String, NonEmptyList[(D => D, Gcal)]]]

  private def emptyCache[K, D]: Cache[K, D] =
      Map.empty

  def fromService[F[_]: Concurrent](
    service: SmartGcalService[F]
  ): ForInstrument[F] =
    new ForInstrument[F](service)

  private class Expander[F[_]: Concurrent, K, D](
    toKey:     D => K,
    formatKey: K => String,
    select:    (K, SmartGcalType) => F[List[(D => D, Gcal)]]
  ) extends SmartGcalExpander[F, D] {

    private def expandAtomWithCache(
      cache: Cache[K, D],
      atom:  ProtoAtom[ProtoStep[D]]
    ): F[(Cache[K, D], Either[String, ProtoAtom[ProtoStep[D]]])] =
      atom
        .steps
        .mapAccumulateM(cache)(expandStepWithCache)
        .map(_.map(_.flatSequence.map(steps => ProtoAtom(atom.description, steps))))

    private def expandStepWithCache(
      cache: Cache[K, D],
      step:  ProtoStep[D]
    ): F[(Cache[K, D], Either[String, NonEmptyList[ProtoStep[D]]])] =
      step match {
        case ProtoStep(d, StepConfig.SmartGcal(sgt), t, o, b) =>
          val key = toKey(d)

          def toStep(tup: (D => D, Gcal)): ProtoStep[D] = {
            val (update, gcal) = tup
            ProtoStep(update(d), gcal, t, o, b)
          }

          cache.get((key, sgt)).fold(
            select(key, sgt).map {
              case Nil    =>
                val error = s"missing Smart GCAL mapping: ${formatKey(key)}"
                (cache.updated((key, sgt), error.asLeft), error.asLeft)
              case h :: t =>
               (
                 cache.updated((key, sgt), NonEmptyList(h, t).asRight),
                 NonEmptyList(toStep(h), t.map(toStep)).asRight
               )
            }
          ) { e => (cache, e.map(_.map(toStep))).pure[F] }

        case ps@ProtoStep(_, _, _, _, _) =>
          (cache, NonEmptyList.one(ps).asRight).pure[F]
      }

    override def expandStep(
      step: ProtoStep[D]
    ): F[Either[String, NonEmptyList[ProtoStep[D]]]] =
      expandStepWithCache(emptyCache[K, D], step).map(_._2)

    override def expandAtom(
      atom: ProtoAtom[ProtoStep[D]]
    ): F[Either[String, ProtoAtom[ProtoStep[D]]]] =
      expandAtomWithCache(emptyCache[K, D], atom).map(_._2)

  }

  class ForInstrument[F[_]: Concurrent](service: SmartGcalService[F]) {

    val flamingos2: SmartGcalExpander[F, Flamingos2DynamicConfig] =
      new Expander[F, Flamingos2.TableKey, Flamingos2DynamicConfig](
        Flamingos2.TableKey.fromDynamicConfig,
        _.format,
        service.selectFlamingos2
      )

    val gmosNorth: SmartGcalExpander[F, GmosNorth] =
      new Expander[F, Gmos.SearchKey.North, GmosNorth](
        Gmos.SearchKey.North.fromDynamicConfig,
        _.format,
        service.selectGmosNorth
      )

    val gmosSouth: SmartGcalExpander[F, GmosSouth] =
      new Expander[F, Gmos.SearchKey.South, GmosSouth](
        Gmos.SearchKey.South.fromDynamicConfig,
        _.format,
        service.selectGmosSouth
      )
  }

}
