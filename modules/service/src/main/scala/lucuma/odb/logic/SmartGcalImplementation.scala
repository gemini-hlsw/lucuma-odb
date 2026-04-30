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
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth as GmosNorthDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth as GmosSouthDynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth as GmosNorthStaticConfig
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth as GmosSouthStaticConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.service.SmartGcalService
import lucuma.odb.smartgcal.data.Flamingos2
import lucuma.odb.smartgcal.data.Ghost
import lucuma.odb.smartgcal.data.Gmos
import lucuma.odb.smartgcal.data.Igrins2

object SmartGcalImplementation {

  type Cache[K, D] = Map[(K, SmartGcalType), Either[String, NonEmptyList[(D => D, Gcal)]]]

  private def emptyCache[K, D]: Cache[K, D] =
      Map.empty

  def fromService[F[_]: Concurrent](
    service: SmartGcalService[F]
  ): ForInstrument[F] =
    new ForInstrument[F](service)

  private class Expander[F[_]: Concurrent, K, S, D](
    toKey:     (S, D) => K,
    formatKey: K => String,
    select:    (K, SmartGcalType) => F[List[(D => D, Gcal)]]
  ) extends SmartGcalExpander[F, S, D] {

    private def expandAtomWithCache(
      cache:  Cache[K, D],
      static: S,
      atom:   ProtoAtom[ProtoStep[D]]
    ): F[(Cache[K, D], Either[String, ProtoAtom[ProtoStep[D]]])] =
      atom
        .steps
        .mapAccumulateM(cache)(expandStepWithCache(_, static, _))
        .map(_.map(_.flatSequence.map(steps => ProtoAtom(atom.description, steps))))

    private def expandStepWithCache(
      cache:  Cache[K, D],
      static: S,
      step:   ProtoStep[D]
    ): F[(Cache[K, D], Either[String, NonEmptyList[ProtoStep[D]]])] =
      step match {
        case ProtoStep(d, StepConfig.SmartGcal(sgt), t, o, b) =>
          val key = toKey(static, d)

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
      static: S,
      step:   ProtoStep[D]
    ): F[Either[String, NonEmptyList[ProtoStep[D]]]] =
      expandStepWithCache(emptyCache[K, D], static, step).map(_._2)

    override def expandAtom(
      static: S,
      atom:   ProtoAtom[ProtoStep[D]]
    ): F[Either[String, ProtoAtom[ProtoStep[D]]]] =
      expandAtomWithCache(emptyCache[K, D], static, atom).map(_._2)

  }

  class ForInstrument[F[_]: Concurrent](service: SmartGcalService[F]) {

    val flamingos2: SmartGcalExpander[F, Flamingos2StaticConfig, Flamingos2DynamicConfig] =
      new Expander[F, Flamingos2.TableKey, Flamingos2StaticConfig, Flamingos2DynamicConfig](
        (_, d) => Flamingos2.TableKey.fromDynamicConfig(d),
        _.format,
        service.selectFlamingos2
      )

    val ghost: SmartGcalExpander[F, GhostStaticConfig, GhostDynamicConfig] =
      new Expander[F, Ghost.SearchKey, GhostStaticConfig, GhostDynamicConfig](
        (s, d) => Ghost.SearchKey.forConfig(s, d),
        _.format,
        service.selectGhost
      )

    val gmosNorth: SmartGcalExpander[F, GmosNorthStaticConfig, GmosNorthDynamicConfig] =
      new Expander[F, Gmos.SearchKey.North, GmosNorthStaticConfig, GmosNorthDynamicConfig](
        (_, d) => Gmos.SearchKey.North.fromDynamicConfig(d),
        _.format,
        service.selectGmosNorth
      )

    val gmosSouth: SmartGcalExpander[F, GmosSouthStaticConfig, GmosSouthDynamicConfig] =
      new Expander[F, Gmos.SearchKey.South, GmosSouthStaticConfig, GmosSouthDynamicConfig](
        (_, d) => Gmos.SearchKey.South.fromDynamicConfig(d),
        _.format,
        service.selectGmosSouth
      )

    val igrins2: SmartGcalExpander[F, Igrins2StaticConfig, Igrins2DynamicConfig] =
      new Expander[F, Igrins2.TableKey.type, Igrins2StaticConfig, Igrins2DynamicConfig](
        (_, _) => Igrins2.TableKey,
        _.format,
        service.selectIgrins2
      )
  }

}
