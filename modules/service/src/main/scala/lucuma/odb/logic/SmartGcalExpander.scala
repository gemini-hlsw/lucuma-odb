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

import scala.collection.mutable.ListBuffer


trait SmartGcalExpander[F[_]] {

  def expand[S, D](
    select: (D, SmartGcalType) => F[List[(D, Gcal)]],
    exec:   ProtoExecution[S, D]
  ): F[Either[D, ProtoExecution[S, D]]]

  def expandGmosNorth(
    exec: ProtoExecution[StaticConfig.GmosNorth, GmosNorth]
  ): F[Either[GmosNorth, ProtoExecution[StaticConfig.GmosNorth, GmosNorth]]]

}

object SmartGcalExpander {

  def fromService[F[_]: Monad](
    service: SmartGcalService[F]
  ): SmartGcalExpander[F] =

    new SmartGcalExpander[F] {

      type Cache[D] = Map[(D, SmartGcalType), NonEmptyList[(D, Gcal)]]

      def emptyCache[D]: Cache[D] =
        Map.empty

      override def expand[S, D](
        select: (D, SmartGcalType) => F[List[(D, Gcal)]],
        exec:   ProtoExecution[S, D]
      ): F[Either[D, ProtoExecution[S, D]]] =
        expandSequence(select)(emptyCache, exec.acquisition).flatMap { (c, acq) =>
          expandSequence(select)(c, exec.science).map { (_, sci) =>
            ProtoExecution(exec.static, acq, sci)
          }
        }.value

      override def expandGmosNorth(
        exec: ProtoExecution[StaticConfig.GmosNorth, GmosNorth]
      ): F[Either[GmosNorth, ProtoExecution[StaticConfig.GmosNorth, GmosNorth]]] =
        expand(service.selectGmosNorth, exec)

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


      private def expandSequence[D](
        select:   (D, SmartGcalType) => F[List[(D, Gcal)]]
      )(
        cache:    Cache[D],
        sequence: ProtoSequence[D]
      ): EitherT[F, D, (Cache[D], ProtoSequence[D])] =
        mapAccumulateNelM(sequence.atoms, cache)(expandAtom(select)).map { _.map(ProtoSequence(_)) }

      private def expandAtom[D](
        select: (D, SmartGcalType) => F[List[(D, Gcal)]]
      )(
        cache:  Cache[D],
        atom:   ProtoAtom[D]
      ): EitherT[F, D, (Cache[D], ProtoAtom[D])] =
        mapAccumulateNelM(atom.steps, cache)(expandStep(select)).map { _.map(nel => ProtoAtom(nel.reduce)) }

      private def expandStep[D](
        select: (D, SmartGcalType) => F[List[(D, Gcal)]]
      )(
        cache:  Cache[D],
        step:   ProtoStep[D]
      ): EitherT[F, D, (Cache[D], NonEmptyList[ProtoStep[D]])] =
        step match {
          case ProtoStep(d, StepConfig.SmartGcal(sgt)) =>
            cache.get((d, sgt)).fold(
              EitherT(
                select(d, sgt).map {
                  case Nil    =>
                    d.asLeft  // sorry, no mapping for d
                  case h :: t => (
                    cache.updated((d, sgt), NonEmptyList(h, t)),
                    NonEmptyList(ProtoStep(h._1, h._2), t.map(ProtoStep.apply))
                  ).asRight
                }
              )
            ) { nel => EitherT.rightT[F, D]((cache, nel.map(ProtoStep.apply))) }

          case ps@ProtoStep(_, _) =>
            EitherT.rightT[F, D](cache, NonEmptyList.one(ps))
        }

    }

}
