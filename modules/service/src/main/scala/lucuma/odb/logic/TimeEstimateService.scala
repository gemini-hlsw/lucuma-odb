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
import cats.syntax.functorFilter.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.itc.client.ItcClient
import lucuma.odb.data.GroupTree
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.ItcService
import lucuma.odb.service.ItcService.AsterismResults
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

/**
 * A service that can estimate the time for a program.
 */
sealed trait TimeEstimateService[F[_]] {

  /**
   * Estimates the remaining time for all observations in the program that are
   * well defined.
   */
  def estimateProgram(
    programId: Program.Id
  )(using NoTransaction[F]): F[Option[CategorizedTimeRange]]

  /**
   * Estimates the remaining time for all observations in the group that are
   * well defined.
   */
  def estimateGroup(
    groupId: Group.Id
  )(using NoTransaction[F]): F[Option[CategorizedTimeRange]]

}

object TimeEstimateService {

  private case class ObservationData(
    generatorParams: GeneratorParams,
    asterismResults: Option[AsterismResults],
    executionDigest: Option[ExecutionDigest]
  ) {

    def cachedFullTimeEstimate: Option[CategorizedTime] =
      executionDigest.map(_.fullTimeEstimate)

  }

  private object ObservationData {

    def load[F[_]: Concurrent](
      pid:       Program.Id,
      itcClient: ItcClient[F]
    )(using Services[F], Transaction[F]): F[Map[Observation.Id, ObservationData]] =
      for {
        p <- generatorParamsService.selectAll(pid, ObsStatus.Included)
        pʹ = p.collect { case (oid, Right(gp)) => (oid, gp) }
        i <- itcService(itcClient).selectAll(pid, pʹ)
        d <- executionDigestService.selectAll(pid)
        dʹ = d.map { case (oid, (_, digest)) => (oid, digest) }
      } yield pʹ.map { case (oid, gp) => (oid, ObservationData(gp, i.get(oid), dʹ.get(oid))) }

  }

  def instantiate[F[_]: Concurrent](
    commitHash:   CommitHash,
    itcClient:    ItcClient[F],
    calculator:   TimeEstimateCalculatorImplementation.ForInstrumentMode,
  )(using Services[F]): TimeEstimateService[F] =
    new TimeEstimateService[F] {

      // CategorizedTime Ordering that sorts longest to shortest.
      val longestToShortest: Ordering[CategorizedTime] =
        catsKernelOrderingForOrder(Order.reverse(Order[CategorizedTime]))

      def combine(minRequired: Int, children: List[CategorizedTimeRange]): Option[CategorizedTimeRange] =
        Option.when(children.size >= minRequired) {
          CategorizedTimeRange.from(
            // Combines the first minRequired elements after sorting by ascending min CategorizedTime
            children.map(_.min).sorted.take(minRequired).combineAllOption.getOrElse(CategorizedTime.Zero),
            // Combines the first minRequired elements after sorting by descending max CategorizedTime
            children.map(_.max).sorted(longestToShortest).take(minRequired).combineAllOption.getOrElse(CategorizedTime.Zero)
          )
        }

      def leafRange(
        pid: Program.Id,
        oid: Observation.Id,
        m:   Map[Observation.Id, ObservationData]
      ): F[Option[CategorizedTimeRange]] =
        OptionT.fromOption(m.get(oid)).flatMap { data =>
          OptionT
            .fromOption(data.cachedFullTimeEstimate)  // try the cache first
            .orElse {
              // ExecutionDigest not in the cache, we'll need to calculate it.
              // For that we need the ITC results, which may be cached.  Use the
              // cached ITC AsterismResults if available, else call remote ITC.
              val asterismResults = OptionT.fromOption(data.asterismResults).orElseF {
                 itcService(itcClient)
                   .callRemote(pid, oid, data.generatorParams)
                   .map(_.toOption)
              }

              // Calculate time estimate using provided ITC result and params.
              asterismResults.flatMapF { ar =>
                generator(commitHash, itcClient, calculator)
                  .calculateDigest(pid, oid, ar, data.generatorParams)
                  .map(_.toOption.map(_.fullTimeEstimate))
              }
            }
            .map(CategorizedTimeRange.single)
        }.value


      def parentRange(
        pid:         Program.Id,
        minRequired: Option[NonNegShort],
        children:    List[GroupTree.Child],
        m:           Map[Observation.Id, ObservationData]
      ): F[Option[CategorizedTimeRange]] =
        children
          .traverse(timeEstimateRange(pid, _, m))
          // combine after skipping any elements for which we cannot compute the
          // time estimate
          .map { lst =>
            val valid = lst.flattenOption
            // If no expicit `minRequired` is set, only count the complete and
            // Approved (i.e., valid) observations.
            combine(minRequired.fold(valid.size)(_.value.toInt), valid)
          }

      def timeEstimateRange(
        pid:  Program.Id,
        root: GroupTree,
        m:    Map[Observation.Id, ObservationData]
      ): F[Option[CategorizedTimeRange]] =
        root match {
          case GroupTree.Leaf(oid)                                  => leafRange(pid, oid, m)
          case GroupTree.Branch(_, min, _, children, _, _, _, _, _) => parentRange(pid, min, children, m)
          case GroupTree.Root(_, children)                          => parentRange(pid, None, children, m)
        }

      private def load(pid: Program.Id): F[(GroupTree, Map[Observation.Id, ObservationData])] =
        services.transactionally {
          (groupService.selectGroups(pid), ObservationData.load(pid, itcClient)).tupled
        }

      override def estimateProgram(
        pid: Program.Id
      )(using NoTransaction[F]): F[Option[CategorizedTimeRange]] =
        load(pid).flatMap { case (tree, dataMap) =>
          timeEstimateRange(pid, tree, dataMap)
        }

      override def estimateGroup(
        groupId: Group.Id
      )(using NoTransaction[F]): F[Option[CategorizedTimeRange]] =
        (for {
          p <- OptionT(groupService.selectPid(groupId))
          d <- OptionT.liftF(load(p))
          (tree, dataMap) = d
          t <- OptionT.fromOption(tree.findGroup(groupId))
          r <- OptionT(timeEstimateRange(p, t, dataMap))
        } yield r).value

  }
}
