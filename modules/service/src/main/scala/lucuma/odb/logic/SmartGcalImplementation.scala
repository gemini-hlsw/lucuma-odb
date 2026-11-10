// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.functor.*
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
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.SmartGcalExpander.Missing
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.service.SmartGcalService
import lucuma.odb.smartgcal.SmartGcalRules
import lucuma.odb.smartgcal.data.Flamingos2
import lucuma.odb.smartgcal.data.Ghost
import lucuma.odb.smartgcal.data.Gmos
import lucuma.odb.smartgcal.data.Gnirs
import lucuma.odb.smartgcal.data.Igrins2

object SmartGcalImplementation {

  def fromService[F[_]: Concurrent](
    service: SmartGcalService[F]
  ): ForInstrument[F] =
    new ForInstrument[F](service)

  // The instrument's rules are applied here so that they hold for every
  // expansion: the lookup is keyed on `rules.lookupConfig` of the step (or
  // fails with its error), and the configuration of each resulting step is
  // completed by `rules.calibrationConfig`.
  private class Expander[F[_]: Concurrent, K, S, D](
    rules:     SmartGcalRules[D],
    toKey:     (S, D) => K,
    formatKey: K => String,
    select:    (K, SmartGcalType) => F[Option[List[(D => D, Gcal)]]]
  ) extends SmartGcalExpander[F, S, D] {

    override def expandStep(
      static: S,
      step:   ProtoStep[D]
    ): F[Either[Missing, List[ProtoStep[D]]]] =
      step match {
        case ProtoStep(d, StepConfig.SmartGcal(sgt), t, o, b) =>

          def toStep(tup: (D => D, Gcal)): ProtoStep[D] = {
            val (update, gcal) = tup
            ProtoStep(rules.calibrationConfig(update(d)), gcal, t, o, b)
          }

          def lookup(key: K): F[Either[Missing, List[ProtoStep[D]]]] =
            select(key, sgt).map:
              _.map(_.map(toStep)).toRight(Missing.NoMapping(formatKey(key)))

          rules
            .lookupConfig(d)
            .fold(
              error => Missing.NoKey(error).asLeft[List[ProtoStep[D]]].pure[F],
              c     => lookup(toKey(static, c))
            )

        case ps@ProtoStep(_, _, _, _, _) =>
          List(ps).asRight[Missing].pure[F]
      }

  }

  class ForInstrument[F[_]: Concurrent](service: SmartGcalService[F]) {

    val flamingos2: SmartGcalExpander[F, Flamingos2StaticConfig, Flamingos2DynamicConfig] =
      new Expander[F, Flamingos2.TableKey, Flamingos2StaticConfig, Flamingos2DynamicConfig](
        SmartGcalRules.flamingos2,
        (_, d) => Flamingos2.TableKey.fromDynamicConfig(d),
        _.format,
        service.selectFlamingos2
      )

    val ghost: SmartGcalExpander[F, GhostStaticConfig, GhostDynamicConfig] =
      new Expander[F, Ghost.SearchKey, GhostStaticConfig, GhostDynamicConfig](
        SmartGcalRules.ghost,
        (s, d) => Ghost.SearchKey.forConfig(s, d),
        _.format,
        service.selectGhost
      )

    val gmosNorth: SmartGcalExpander[F, GmosNorthStaticConfig, GmosNorthDynamicConfig] =
      new Expander[F, Gmos.SearchKey.North, GmosNorthStaticConfig, GmosNorthDynamicConfig](
        SmartGcalRules.gmosNorth,
        (_, d) => Gmos.SearchKey.North.fromDynamicConfig(d),
        _.format,
        service.selectGmosNorth
      )

    val gmosSouth: SmartGcalExpander[F, GmosSouthStaticConfig, GmosSouthDynamicConfig] =
      new Expander[F, Gmos.SearchKey.South, GmosSouthStaticConfig, GmosSouthDynamicConfig](
        SmartGcalRules.gmosSouth,
        (_, d) => Gmos.SearchKey.South.fromDynamicConfig(d),
        _.format,
        service.selectGmosSouth
      )

    val igrins2: SmartGcalExpander[F, Igrins2StaticConfig, Igrins2DynamicConfig] =
      new Expander[F, Igrins2.TableKey.type, Igrins2StaticConfig, Igrins2DynamicConfig](
        SmartGcalRules.igrins2,
        (_, _) => Igrins2.TableKey,
        _.format,
        service.selectIgrins2
      )

    val gnirs: SmartGcalExpander[F, GnirsStaticConfig, GnirsDynamicConfig] =
      new Expander[F, Gnirs.SearchKey, GnirsStaticConfig, GnirsDynamicConfig](
        SmartGcalRules.gnirs,
        (s, d) => Gnirs.SearchKey.fromConfig(s, d),
        _.format,
        service.selectGnirs
      )
  }

}
