// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.monoid.*
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.util.CalculationState
import lucuma.core.util.CalculatedValue
import lucuma.itc.client.ItcClient
import lucuma.odb.data.GroupTree
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.ObscalcService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

/**
 * A service that can estimate the time for a program.
 */
sealed trait TimeEstimateService2[F[_]]:

  /**
   * Estimates the remaining time for all observations in the program that are
   * well defined.
   */
  def estimateProgramRange(
    programId: Program.Id
  )(using Transaction[F]): F[Option[CalculatedValue[CategorizedTimeRange]]]

  /**
   * Estimates the remaining time for all observations in the group that are
   * well defined.
   */
  def estimateGroupRange(
    groupId: Group.Id
  )(using Transaction[F]): F[Option[CalculatedValue[CategorizedTimeRange]]]

  /**
   * Estimates the remaining time for all observations in the program by band,
   * ignoring `minRequired` in groups.
   */
  def estimateProgramBanded(
    programId: Program.Id
  )(using Transaction[F]): F[Map[Option[ScienceBand], CalculatedValue[CategorizedTime]]]

  /**
   * Estimates the remaining time for all observations in the group by band,
   * ignoring `minRequired`.
   */
  def estimateGroupBanded(
    groupId: Group.Id
  )(using Transaction[F]): F[Map[Option[ScienceBand], CalculatedValue[CategorizedTime]]]


object TimeEstimateService2:

  def instantiate[F[_]: Concurrent](
    commitHash:   CommitHash,
    itcClient:    ItcClient[F],
    calculator:   TimeEstimateCalculatorImplementation.ForInstrumentMode,
  )(using Services[F]): TimeEstimateService2[F] =
    lazy val obscalcService: ObscalcService[F] =
      services.obscalcService(commitHash, itcClient, calculator)

    new TimeEstimateService2[F]:

      // CategorizedTime Ordering that sorts longest to shortest.
      val longestToShortest: Ordering[CategorizedTime] =
        catsKernelOrderingForOrder(Order.reverse(Order[CategorizedTime]))

      def combine(
        minRequired: Int,
        children:    List[CalculatedValue[CategorizedTimeRange]]
      ): Option[CalculatedValue[CategorizedTimeRange]] =
        Option.when(children.size >= minRequired):
          CalculatedValue(
            children.foldMap(_.state),  // all children considered for calc state since changing one time calc could change sorting
            CategorizedTimeRange.from(
              // Combines the first minRequired elements after sorting by ascending min CategorizedTime
              children.map(_.value.min).sorted.take(minRequired).combineAllOption.getOrElse(CategorizedTime.Zero),
              // Combines the first minRequired elements after sorting by descending max CategorizedTime
              children.map(_.value.max).sorted(longestToShortest).take(minRequired).combineAllOption.getOrElse(CategorizedTime.Zero)
            )
          )

      def leafRange(
        pid: Program.Id,
        oid: Observation.Id,
        m:   Map[Observation.Id, CalculatedValue[CategorizedTime]]
      ): Option[CalculatedValue[CategorizedTimeRange]] =
        m.get(oid).map: cv =>
          CalculatedValue(cv.state, CategorizedTimeRange.single(cv.value))

      def parentRange(
        pid:         Program.Id,
        minRequired: Option[NonNegShort],
        children:    List[GroupTree.Child],
        m:           Map[Observation.Id, CalculatedValue[CategorizedTime]]
      ): Option[CalculatedValue[CategorizedTimeRange]] =
        // combine after skipping any elements for which we cannot compute the
        // time estimate
        val valid = children.map(timeEstimateRange(pid, _, m)).flattenOption

        // If no expicit `minRequired` is set, only count the complete and
        // Approved (i.e., valid) observations.
        combine(minRequired.fold(valid.size)(_.value.toInt), valid)

      def timeEstimateRange(
        pid:  Program.Id,
        root: GroupTree,
        m:    Map[Observation.Id, CalculatedValue[CategorizedTime]]
      ): Option[CalculatedValue[CategorizedTimeRange]] =
        root match
          case GroupTree.Leaf(oid)                                  => leafRange(pid, oid, m)
          case GroupTree.Branch(_, min, _, children, _, _, _, _, _) => parentRange(pid, min, children, m)
          case GroupTree.Root(_, children)                          => parentRange(pid, None, children, m)

      private def load(
        pid: Program.Id
      )(using Transaction[F]): F[(GroupTree, Map[Observation.Id, CalculatedValue[CategorizedTime]])] =
        (groupService.selectGroups(pid), obscalcService.selectProgramCategorizedTime(pid)).tupled

      override def estimateProgramRange(
        pid: Program.Id
      )(using Transaction[F]): F[Option[CalculatedValue[CategorizedTimeRange]]] =
        load(pid).map:
          case (tree, valueMap) => timeEstimateRange(pid, tree, valueMap)

      override def estimateGroupRange(
        gid: Group.Id
      )(using Transaction[F]): F[Option[CalculatedValue[CategorizedTimeRange]]] =
        (for
          p <- OptionT(groupService.selectPid(gid))
          d <- OptionT.liftF(load(p))
          (tree, valueMap) = d
          t <- OptionT.fromOption(tree.findGroup(gid))
          r <- OptionT.fromOption(timeEstimateRange(p, t, valueMap))
        yield r).value

      override def estimateProgramBanded(
        pid: Program.Id
      )(using Transaction[F]): F[Map[Option[ScienceBand], CalculatedValue[CategorizedTime]]] =
        (observationService.selectBands(pid),
         obscalcService.selectProgramCategorizedTime(pid)
        ).tupled
         .map: (bandMap, valueMap) =>
           bandMap
             .toList
             .groupMapReduce((_, band) => band)((oid, _) => valueMap.getOrElse(oid, CalculatedValue.empty[CategorizedTime]))(_ |+| _)

      override def estimateGroupBanded(
        gid: Group.Id
      )(using Transaction[F]): F[Map[Option[ScienceBand], CalculatedValue[CategorizedTime]]] =
        def containedObs(root: GroupTree): Set[Observation.Id] =
          root match
            case GroupTree.Leaf(oid)                                => Set(oid)
            case GroupTree.Branch(_, _, _, children, _, _, _, _, _) => children.map(containedObs).combineAll
            case GroupTree.Root(_, children)                        => children.map(containedObs).combineAll

        val res = (for
          p <- OptionT(groupService.selectPid(gid))
          t <- OptionT.liftF(groupService.selectGroups(p))
          os = t.findGroup(gid).map(containedObs).combineAll
          b <- OptionT.liftF(observationService.selectBands(p))
          c <- OptionT.liftF(obscalcService.selectProgramCategorizedTime(p))
        yield
          b.view
           .filterKeys(os.contains)
           .toList
           .groupMapReduce((_, band) => band)((oid, _) => c.getOrElse(oid, CalculatedValue.empty[CategorizedTime]))(_ |+| _))

        res.value.map(_.getOrElse(Map.empty))