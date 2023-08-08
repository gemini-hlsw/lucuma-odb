// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.PlannedTime
import lucuma.core.model.sequence.PlannedTimeRange
import lucuma.itc.client.ItcClient
import lucuma.odb.data.GroupTree
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.ItcService
import lucuma.odb.service.ItcService.AsterismResult
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

/**
 * A service that can estimate the PlannedTimeRange for a program.
 */
sealed trait PlannedTimeRangeService[F[_]] {

  /**
   * Estimates the program planned time for observations that are well defined.
   */
  def estimateProgram(
    programId: Program.Id
  )(using NoTransaction[F]): F[Option[PlannedTimeRange]]

}

object PlannedTimeRangeService {

  private case class ObservationData(
    generatorParams: GeneratorParams,
    asterismResult:  Option[AsterismResult],
    executionDigest: Option[ExecutionDigest]
  ) {

    def cachedFullPlannedTime: Option[PlannedTime] =
      executionDigest.map(_.fullPlannedTime)

  }

  private object ObservationData {

    def load[F[_]: Concurrent](
      pid:       Program.Id,
      itcClient: ItcClient[F]
    )(using Services[F], Transaction[F]): F[Map[Observation.Id, ObservationData]] =
      for {
        p <- generatorParamsService.selectAll(pid)
        pʹ = p.collect { case (oid, Right(gp)) => (oid, gp) }
        i <- itcService(itcClient).selectAll(pid, pʹ)
        d <- executionDigestService.selectAll(pid)
        dʹ = d.map { case (oid, (_, digest)) => (oid, digest) }
      } yield pʹ.map { case (oid, gp) => (oid, ObservationData(gp, i.get(oid), dʹ.get(oid))) }

  }

  def instantiate[F[_]: Concurrent](
    commitHash:   CommitHash,
    itcClient:    ItcClient[F],
    calculator:   PlannedTimeCalculator.ForInstrumentMode,
  )(using Services[F]): PlannedTimeRangeService[F] =
    new PlannedTimeRangeService[F] {

      // PlannedTime Ordering that sorts longest to shortest.
      val longestToShortest: Ordering[PlannedTime] =
        catsKernelOrderingForOrder(Order.reverse(Order[PlannedTime]))

      def combine(minRequired: Int, children: List[PlannedTimeRange]): Option[PlannedTimeRange] =
        Option.when(children.size >= minRequired) {
          PlannedTimeRange.from(
            // Combines the first minRequired elements after sorting by ascending min PlannedTime
            children.map(_.min).sorted.take(minRequired).combineAllOption.getOrElse(PlannedTime.Zero),
            // Combines the first minRequired elements after sorting by descending max PlannedTime
            children.map(_.max).sorted(longestToShortest).take(minRequired).combineAllOption.getOrElse(PlannedTime.Zero)
          )
        }

      def leafRange(
        pid: Program.Id,
        oid: Observation.Id,
        m:   Map[Observation.Id, ObservationData]
      ): F[Option[PlannedTimeRange]] =
        OptionT.fromOption(m.get(oid)).flatMap { data =>
          OptionT
            .fromOption(data.cachedFullPlannedTime)  // try the cache first
            .orElse {
              // ExecutionDigest not in the cache, we'll need to calculate it.
              // For that we need the ITC results, which may be cached.  Use the
              // cached ITC AsterismResult if available, else call remote ITC.
              val asterismResult = OptionT.fromOption(data.asterismResult).orElseF {
                 itcService(itcClient)
                   .callRemote(pid, oid, data.generatorParams)
                   .map(_.toOption)
              }

              // Calculate planned time using provided ITC result and params.
              asterismResult.flatMapF { ar =>
                generator(commitHash, itcClient, calculator)
                  .calculateDigest(pid, oid, ar, data.generatorParams)
                  .map(_.toOption.map(_.fullPlannedTime))
              }
            }
            .map(PlannedTimeRange.single)
        }.value


      // If no minRequired, assume _all_ children are required.
      def parentRange(
        pid:         Program.Id,
        minRequired: Option[NonNegShort],
        children:    List[GroupTree.Child],
        m:           Map[Observation.Id, ObservationData]
      ): F[Option[PlannedTimeRange]] =
        children
          .traverse(plannedTimeRange(pid, _, m))
          // combine after skipping any elements for which we cannot compute the planned time
          .map(lst => combine(minRequired.fold(lst.size)(_.value.toInt), lst.flatMap(_.toList)))

      def plannedTimeRange(
        pid:  Program.Id,
        root: GroupTree,
        m:    Map[Observation.Id, ObservationData]
      ): F[Option[PlannedTimeRange]] =
        root match {
          case GroupTree.Leaf(oid)                               => leafRange(pid, oid, m)
          case GroupTree.Branch(_, min, _, children, _, _, _, _) => parentRange(pid, min, children, m)
          case GroupTree.Root(_, children)                       => parentRange(pid, None, children, m)
        }

      override def estimateProgram(
        pid: Program.Id
      )(using NoTransaction[F]): F[Option[PlannedTimeRange]] =
        services.transactionally {
          (groupService.selectGroups(pid), ObservationData.load(pid, itcClient)).tupled
        }.flatMap { case (tree, dataMap) =>
          plannedTimeRange(pid, tree, dataMap)
        }
  }
}
