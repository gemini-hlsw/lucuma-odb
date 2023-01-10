// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.client

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.effect.MonadCancel
import cats.effect.Resource
import cats.syntax.applicative.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.order.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.data.Zipper
import lucuma.core.model.NonNegDuration
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.itc.client.ItcClient
import lucuma.itc.client.ItcResult
import lucuma.itc.client.SpectroscopyModeInput
import lucuma.odb.service.ItcInputService
import lucuma.odb.util.NonEmptyListExtensions.*

/**
 * The ITC client combines the input parameter lookup and the actual remote
 * call to the ITC service in a single method.
 */
sealed trait Itc[F[_]] {
  import Itc.ResultSet

  def queryOne(
    programId:     Program.Id,
    observationId: Observation.Id,
    useCache:      Boolean
  ): F[Option[ResultSet]]

  def queryAll(
    programId:      Program.Id,
    observationIds: List[Observation.Id],
    useCache:       Boolean
  ): F[List[ResultSet]]

}


object Itc {

  opaque type Param = String

  extension (p: Param) {
    def stringValue: String =
      p
  }

  /**
   * Result of an ITC lookup for a single target of an observation, ignoring
   * any other targets that an observation might have.
   */
  enum Result {

    def fold[A](
      mf: Missing      => A,
      ef: ServiceError => A,
      sf: Success      => A
    ): A =
      this match {
        case m @ Missing(_, _)          => mf(m)
        case e @ ServiceError(_, _, _)  => ef(e)
        case s @ Success(_, _, _, _, _) => sf(s)
      }

    val missing: Option[Result.Missing] =
      fold(_.some, _ => none, _ => none)

    val serviceError: Option[Result.ServiceError] =
      fold(_ => none, _.some, _ => none)

    val success: Option[Result.Success] =
      fold(_ => none, _ => none, _.some)

    /**
     * One or more required parameters are missing, preventing the ITC service
     * from being consulted.
     */
    case Missing(
      targetId: Option[Target.Id],
      params:   NonEmptySet[Param]
    )

    /**
     * The ITC service was called but did not return a successful result.  The
     * `message` field may contain more information.
     */
    case ServiceError(
      targetId: Target.Id,
      input:    SpectroscopyModeInput,
      message:  String
    )

    /**
     * Successful ITC service call results.
     */
    case Success(
      targetId:      Target.Id,
      input:         SpectroscopyModeInput,
      exposureTime:  NonNegDuration,
      exposures:     NonNegInt,
      signalToNoise: PosBigDecimal
    )
  }

  object Result {

    /**
     * An `Order` definition used to select a single Result for an observation
     * when it has multiple targets.  In general, we want the result in which
     * calls for the longest observation.  // TODO: verify
     *
     * N.B., not implicit because this isn't a total ordering.
     */
    val SelectionOrder: Order[Result] =
      Order.from {
        case (Missing(_, _), _)                                   => -1
        case (_, Missing(_, _))                                   =>  1
        case (ServiceError(_, _, _), _)                           => -1
        case (_, ServiceError(_, _, _))                           =>  1
        case (Success(_, _, at, ac, _), Success(_, _, bt, bc, _)) =>
          at.value
            .multipliedBy(ac.value)
            .compareTo(bt.value.multipliedBy(bc.value))
      }

  }

  /**
   * Results for all the targets in an observation's asterism.  The summary or
   * selected result for the set as a whole is the focus of the `value` zipper.
   */
  final case class ResultSet(
    programId:     Program.Id,
    observationId: Observation.Id,
    value:         Zipper[Result]
  )

  object ResultSet {

    def missing(
      pid: Program.Id,
      oid: Observation.Id,
      ps:  NonEmptyList[(Option[Target.Id], Param)]
    ): ResultSet =
      fromResults(
        pid,
        oid,
        ps.groupMapReduceNem(_._1) { case (_, p) => NonEmptySet.one(p) }
          .toNel
          .map { case (tid, ps) => Result.Missing(tid, ps) }
      )

    def fromResults(
      pid: Program.Id,
      oid: Observation.Id,
      rs:  NonEmptyList[Result]
    ): ResultSet =
      ResultSet(pid, oid, rs.focusMax(Result.SelectionOrder))
  }

  def fromClientAndService[F[_]](
    client:  ItcClient[F],
    service: Resource[F, ItcInputService[F]]
  )(implicit ev: MonadCancel[F, Throwable]): Itc[F] =
    new Itc[F] {

      def queryOne(
        programId:     Program.Id,
        observationId: Observation.Id,
        useCache:      Boolean
      ): F[Option[ResultSet]] =
        queryAll(programId, List(observationId), useCache)
          .map(_.find(_.observationId === observationId))

      def queryAll(
        programId:      Program.Id,
        observationIds: List[Observation.Id],
        useCache:       Boolean
      ): F[List[ResultSet]] =

        service.use { s =>
          s.selectSpectroscopyInput(programId, observationIds).flatMap { m =>
             m.toList.traverse { case (oid, e) =>
               callForObservation(programId, oid, e, useCache)
             }
          }
        }

      def callForObservation(
        pid:      Program.Id,
        oid:      Observation.Id,
        e:        EitherNel[(Option[Target.Id], String), NonEmptyList[(Target.Id, SpectroscopyModeInput)]],
        useCache: Boolean
      ): F[ResultSet] =

        e.fold(
          ps => ResultSet.missing(pid, oid, ps).pure[F],
          _.traverse { case (tid, si) =>
            callForTarget(tid, si, useCache)
          }.map(ResultSet.fromResults(pid, oid, _))
        )


      def callForTarget(
        tid:      Target.Id,
        input:    SpectroscopyModeInput,
        useCache: Boolean
      ): F[Result] =
        client.spectroscopy(input, useCache).map { sr =>
          sr.result.fold(Result.ServiceError(tid, input, "ITC Service returned nothing.")) {
            case ItcResult.Error(msg)       => Result.ServiceError(tid, input, msg)
            case ItcResult.Success(t, c, s) => Result.Success(tid, input, t, c, s)
          }
        }

    }

}
