// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoSequence
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.service.SmartGcalService
import lucuma.odb.smartgcal.data.Gmos

trait SmartGcalExpander[F[_], K, D] {

  def expand(
    s: ProtoSequence[ProtoStep[D]]
  ): F[Either[K, ProtoSequence[ProtoStep[D]]]]

}

object SmartGcalExpander {

  def fromService[F[_]: Monad](
    service: SmartGcalService[F]
  ): ForInstrument[F] =
    new ForInstrument[F](service)

  private class Expander[F[_]: Monad, K, D](
    toKey:  D => K,
    select: (K, SmartGcalType) => F[List[(D => D, Gcal)]]
  ) extends SmartGcalExpander[F, K, D] {

    private type Cache = Map[(K, SmartGcalType), NonEmptyList[(D => D, Gcal)]]

    private def emptyCache: Cache =
      Map.empty

    def expand(
      s: ProtoSequence[ProtoStep[D]]
    ): F[Either[K, ProtoSequence[ProtoStep[D]]]] =
      s.atoms
       .mapAccumulateM(emptyCache)(expandAtom)
       .map { case (_, atoms) => ProtoSequence(atoms) }
       .value

    private def expandAtom(
      cache: Cache,
      atom:  ProtoAtom[ProtoStep[D]]
    ): EitherT[F, K, (Cache, ProtoAtom[ProtoStep[D]])] =
      atom
        .steps
        .mapAccumulateM(cache)(expandStep)
        .map { _.map { nelnel => ProtoAtom(atom.description, nelnel.flatten) } }

    private def expandStep(
      cache: Cache,
      step:  ProtoStep[D],
    ): EitherT[F, K, (Cache, NonEmptyList[ProtoStep[D]])] =
      step match {
        case ProtoStep(d, StepConfig.SmartGcal(sgt), o, b) =>
          val key = toKey(d)

          def toStep(tup: (D => D, Gcal)): ProtoStep[D] = {
            val (update, gcal) = tup
            ProtoStep(update(d), gcal, o, b)
          }

          cache.get((key, sgt)).fold(
            EitherT(
              select(key, sgt).map {
                case Nil    => key.asLeft // sorry, no mapping for d
                case h :: t => (
                  cache.updated((key, sgt), NonEmptyList(h, t)),
                  NonEmptyList(toStep(h), t.map(toStep))
                ).asRight
              }
            )
          ) { nel => EitherT.rightT[F, K]((cache, nel.map(toStep))) }

        case ps@ProtoStep(_, _, _, _) =>
          EitherT.rightT[F, K]((cache, NonEmptyList.one(ps)))
      }
  }

  class ForInstrument[F[_]: Monad](service: SmartGcalService[F]) {

    val gmosNorth: SmartGcalExpander[F, Gmos.SearchKey.North, GmosNorth] =
      new Expander[F, Gmos.SearchKey.North, GmosNorth](
        Gmos.SearchKey.North.fromDynamicConfig,
        service.selectGmosNorth
      )

    val gmosSouth: SmartGcalExpander[F, Gmos.SearchKey.South, GmosSouth] =
      new Expander[F, Gmos.SearchKey.South, GmosSouth](
        Gmos.SearchKey.South.fromDynamicConfig,
        service.selectGmosSouth
      )
  }

  def expandExecutionConfig[F[_]: Monad, K, S, D](
    c: ProtoExecutionConfig[S, ProtoStep[D]],
    x: SmartGcalExpander[F, K, D]
  ): F[Either[K, ProtoExecutionConfig[S, ProtoStep[D]]]] =
    (for {
      a <- EitherT(x.expand(c.acquisition))
      s <- EitherT(x.expand(c.science))
    } yield ProtoExecutionConfig(c.static, a, s)).value

}
