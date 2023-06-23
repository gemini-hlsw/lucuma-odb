// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.Pipe
import fs2.Stream
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.service.SmartGcalService
import lucuma.odb.smartgcal.data.Gmos

import SmartGcalExpander.Cache

trait SmartGcalExpander[F[_], K, D] {

  /**
   * Computes a cache of all unique instrument configurations that require a
   * smart gcal definition lookup and their associated gcal configurations (or
   * an error describing the missing definition if not possible).  The cache can
   * be passed to `expandWithCache`` to avoid additional trips to the database
   * for smart gcal.
   *
   * @param s the sequence to validate
   *
   * @return cache of smart gcal lookups or an error message
   */
  def validate(s: Stream[F, ProtoAtom[ProtoStep[D]]]): F[Either[String, Cache[K, D]]]

  /**
   * Expands the given sequence replacing smart gcal steps with normal gcal
   * steps.  Note that a `MappingException` will be thrown if any smart gcal
   * step cannot be expanded.  To avoid the exception, use `validate` before
   * `expand`.
   */
  def expand: Pipe[F, ProtoAtom[ProtoStep[D]], ProtoAtom[ProtoStep[D]]]

  /**
   * Expands the given sequence replacing smart gcal steps with normal gcal
   * steps.  Note that a `MappingException` will be thrown if any smart gcal
   * step cannot be expanded.  To avoid the exception, use `validate` before
   * `expandWithCache`.  To avoid further trips to the database, pass the cache
   * obtained from `validate`.
   */
  def expandWithCache(c: Cache[K, D]): Pipe[F, ProtoAtom[ProtoStep[D]], ProtoAtom[ProtoStep[D]]]

}

object SmartGcalExpander {

  type Cache[K, D] = Map[(K, SmartGcalType), NonEmptyList[(D => D, Gcal)]]

  def emptyCache[K, D]: Cache[K, D] =
      Map.empty

  final class MappingException(val keyDescription: String) extends RuntimeException(
    s"Missing Smart GCal mapping for $keyDescription"
  )

  def fromService[F[_]: Concurrent](
    service: SmartGcalService[F]
  ): ForInstrument[F] =
    new ForInstrument[F](service)

  private class Expander[F[_]: Concurrent, K, D](
    toKey:     D => K,
    formatKey: K => String,
    select:    (K, SmartGcalType) => F[List[(D => D, Gcal)]]
  ) extends SmartGcalExpander[F, K, D] {

    val expand: Pipe[F, ProtoAtom[ProtoStep[D]], ProtoAtom[ProtoStep[D]]] =
      expandWithCache(emptyCache[K, D])

    def expandWithCache(c: Cache[K, D]): Pipe[F, ProtoAtom[ProtoStep[D]], ProtoAtom[ProtoStep[D]]] =
      _.evalMapAccumulate(c)(expandAtom).map(_._2)

    private def expandAtom(
      cache: Cache[K, D],
      atom:  ProtoAtom[ProtoStep[D]]
    ): F[(Cache[K, D], ProtoAtom[ProtoStep[D]])] =
      atom
        .steps
        .mapAccumulateM(cache)(expandStep)
        .map { _.map { nelnel => ProtoAtom(atom.description, nelnel.flatten) } }

    private def expandStep(
      cache: Cache[K, D],
      step:  ProtoStep[D]
    ): F[(Cache[K, D], NonEmptyList[ProtoStep[D]])] =
      step match {
        case ProtoStep(d, StepConfig.SmartGcal(sgt), o, b) =>
          val key = toKey(d)

          def toStep(tup: (D => D, Gcal)): ProtoStep[D] = {
            val (update, gcal) = tup
            ProtoStep(update(d), gcal, o, b)
          }

          cache.get((key, sgt)).fold(
            select(key, sgt).map {
              case Nil    => throw new MappingException(formatKey(key))
              case h :: t => (
                cache.updated((key, sgt), NonEmptyList(h, t)),
                NonEmptyList(toStep(h), t.map(toStep))
              )
            }
          ) { nel => (cache, nel.map(toStep)).pure[F] }

        case ps@ProtoStep(_, _, _, _) =>
          (cache, NonEmptyList.one(ps)).pure[F]
      }

    def validate(s: Stream[F, ProtoAtom[ProtoStep[D]]]): F[Either[String, Cache[K, D]]] =
      s.fold(EitherT.rightT[F, K](emptyCache[K, D])) { (e, atom) =>
        e.flatMap { c => validateAtom(c, atom) }
      }.evalMap(_.value)
       .map(_.leftMap(k => formatKey(k)))
       .compile
       .onlyOrError

    private def validateAtom(
      cache: Cache[K, D],
      atom:  ProtoAtom[ProtoStep[D]]
    ): EitherT[F, K, Cache[K, D]] =
      atom
        .steps
        .foldLeft(EitherT.rightT(cache)) { (e, step) =>
          e.flatMap { c => validateStep(c, step) }
        }

    private def validateStep(
      cache: Cache[K, D],
      step:  ProtoStep[D]
    ): EitherT[F, K, Cache[K, D]] =
      step match {
        case ProtoStep(d, StepConfig.SmartGcal(sgt), _, _) =>
          val key = toKey(d)
          cache.get((key, sgt)).fold(
            EitherT(select(key, sgt).map {
              case Nil    => key.asLeft
              case h :: t => cache.updated((key, sgt), NonEmptyList(h, t)).asRight
            })
          ) { _ => EitherT.rightT(cache) }

        case _ =>
          EitherT.rightT(cache)
      }
  }

  class ForInstrument[F[_]: Concurrent](service: SmartGcalService[F]) {

    val gmosNorth: SmartGcalExpander[F, Gmos.SearchKey.North, GmosNorth] =
      new Expander[F, Gmos.SearchKey.North, GmosNorth](
        Gmos.SearchKey.North.fromDynamicConfig,
        _.format,
        service.selectGmosNorth
      )

    val gmosSouth: SmartGcalExpander[F, Gmos.SearchKey.South, GmosSouth] =
      new Expander[F, Gmos.SearchKey.South, GmosSouth](
        Gmos.SearchKey.South.fromDynamicConfig,
        _.format,
        service.selectGmosSouth
      )
  }

}
