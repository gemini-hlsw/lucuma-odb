// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Monad
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.StaticConfig
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecution
import lucuma.odb.sequence.data.ProtoSequence
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.service.SmartGcalService
import lucuma.odb.smartgcal.data.Gmos.SearchKey.{ North => GmosNorthSearchKey }

import scala.collection.mutable.ListBuffer


trait SmartGcalExpander[F[_]] {

  /**
   * Expands any SmartGcal steps found in the execution into their matching
   * Gcal steps by matching on the dynamic instrument config in the step where
   * they occur.
   *
   * @return a Left `GmosNorthSearchKey` if no smart gcal configuration can be
   *         found for a particular step, a Right `ProtoExecution` if successful
   *         with all SmartGcal steps replaced by one or more Gcal steps
   */
  def expandGmosNorth(
    exec: ProtoExecution[StaticConfig.GmosNorth, GmosNorth]
  ): F[Either[GmosNorthSearchKey, ProtoExecution[StaticConfig.GmosNorth, GmosNorth]]]

}

object SmartGcalExpander {

  def fromService[F[_]: Monad](
    service: SmartGcalService[F]
  ): SmartGcalExpander[F] =

    new SmartGcalExpander[F] {

      type Cache[K, D] = Map[(K, SmartGcalType), NonEmptyList[(D => D, Gcal)]]

      def emptyCache[K, D]: Cache[K, D] =
        Map.empty

      override def expandGmosNorth(
        exec: ProtoExecution[StaticConfig.GmosNorth, GmosNorth]
      ): F[Either[GmosNorthSearchKey, ProtoExecution[StaticConfig.GmosNorth, GmosNorth]]] =
        expand(GmosNorthSearchKey.fromDynamicConfig, service.selectGmosNorth, exec)

      def expand[K, S, D](
        toKey:  D => K,
        select: (K, SmartGcalType) => F[List[(D => D, Gcal)]],
        exec:   ProtoExecution[S, D]
      ): F[Either[K, ProtoExecution[S, D]]] =
        expandSequence(toKey, select)(emptyCache, exec.acquisition).flatMap { (c, acq) =>
          expandSequence(toKey, select)(c, exec.science).map { (_, sci) =>
            ProtoExecution(exec.static, acq, sci)
          }
        }.value

      private def mapAccumulateM[G[_]: Monad, A, B, S](
        as: List[A],
        s0: S
      )(
        f: (S, A) => G[(S, B)]
      ): G[(S, List[B])] =
        as.foldLeft((s0, ListBuffer.empty[B]).pure[G]) { case (accG, a) =>
          accG.flatMap { case (s, bs) =>
            f(s, a).map { case (s2, b) => (s2, bs.addOne(b)) }
          }
        }.map(_.map(_.toList))

      private def mapAccumulateNelM[G[_]: Monad, A, B, S](
        as: NonEmptyList[A],
        s0: S
      )(
        f: (S, A) => G[(S, B)]
      ): G[(S, NonEmptyList[B])] =
        f(s0, as.head).flatMap { (s1, b) =>
          mapAccumulateM(as.tail, s1)(f).map(_.map(NonEmptyList(b, _)))
        }

      private def expandSequence[K, D](
        toKey:    D => K,
        select:   (K, SmartGcalType) => F[List[(D => D, Gcal)]]
      )(
        cache:    Cache[K, D],
        sequence: ProtoSequence[D]
      ): EitherT[F, K, (Cache[K, D], ProtoSequence[D])] =
        mapAccumulateNelM(sequence.atoms, cache)(expandAtom(toKey, select)).map { _.map(ProtoSequence(_)) }

      private def expandAtom[K, D](
        toKey:  D => K,
        select: (K, SmartGcalType) => F[List[(D => D, Gcal)]]
      )(
        cache:  Cache[K, D],
        atom:   ProtoAtom[D]
      ): EitherT[F, K, (Cache[K, D], ProtoAtom[D])] =
        mapAccumulateNelM(atom.steps, cache)(expandStep(toKey, select)).map { _.map(nel => ProtoAtom(nel.reduce)) }

      private def expandStep[K, D](
        toKey:  D => K,
        select: (K, SmartGcalType) => F[List[(D => D, Gcal)]]
      )(
        cache:  Cache[K, D],
        step:   ProtoStep[D]
      ): EitherT[F, K, (Cache[K, D], NonEmptyList[ProtoStep[D]])] =
        step match {
          case ProtoStep(d, StepConfig.SmartGcal(sgt)) =>
            val key = toKey(d)

            def toStep(tup: (D => D, Gcal)): ProtoStep[D] = {
              val (update, gcal) = tup
              ProtoStep(update(d), gcal)
            }

            cache.get((key, sgt)).fold(
              EitherT(
                select(key, sgt).map {
                  case Nil    => key.asLeft  // sorry, no mapping for d
                  case h :: t => (
                    cache.updated((key, sgt), NonEmptyList(h, t)),
                    NonEmptyList(toStep(h), t.map(toStep))
                  ).asRight
                }
              )
            ) { nel => EitherT.rightT[F, K]((cache, nel.map(toStep))) }

          case ps@ProtoStep(_, _) =>
            EitherT.rightT[F, K](cache, NonEmptyList.one(ps))
        }

    }

}
