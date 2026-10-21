// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.Semigroup
import cats.effect.Concurrent
import cats.kernel.CommutativeMonoid
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.util.CalculatedValue
import lucuma.core.util.CalculationState
import lucuma.odb.data.GroupTree
import lucuma.odb.service.GroupService
import lucuma.odb.service.ObscalcService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

/**
 * A service that can estimate the time required to finish the unexecuted
 * remainder of a program.
 */
sealed trait TimeEstimateService[F[_]]:

  /**
   * Estimates the remaining time for all observations in the program that are
   * well defined.
   */
  def estimateProgramRange(
    programId: Program.Id
  )(using Transaction[F]): F[Option[CalculatedValue[CategorizedTimeRange]]]

  /**
   * Estimates the remaining time for all observations in the program by band,
   * ignoring `minRequired` in groups.
   */
  def estimateProgramBanded(
    programId: Program.Id
  )(using Transaction[F]): F[Map[Option[ScienceBand], CalculatedValue[CategorizedTime]]]

  /**
   * Estimates the remaining time for each group: the range for its well
   * defined observations (as `estimateProgramRange` does for a program) and,
   * when `includeBanded` is set, the time by band ignoring `minRequired` (as
   * `estimateProgramBanded` does).  Each program's group tree is given
   * (`GroupService.selectProgramTrees`), so that callers computing other
   * things over the same groups load it only once.  The result has an entry
   * for every group in `trees`.
   */
  def estimateGroups(
    trees:         List[GroupService.ProgramTree],
    includeBanded: Boolean
  )(using Transaction[F]): F[Map[Group.Id, TimeEstimateService.GroupEstimate]]


object TimeEstimateService:

  /**
   * A group's remaining time range, and its remaining time by band.  The
   * banded time is empty unless it was requested.
   */
  case class GroupEstimate(
    range:  Option[CalculatedValue[CategorizedTimeRange]],
    banded: Map[Option[ScienceBand], CalculatedValue[CategorizedTime]]
  )

  object GroupEstimate:
    val Empty: GroupEstimate = GroupEstimate(None, Map.empty)

  def instantiate[F[_]: Concurrent: Services]: TimeEstimateService[F] =
    lazy val obscalcService: ObscalcService[F] =
      services.obscalcService

    new TimeEstimateService[F]:

      // OBSCALC TODO: In core, need to fiddle with `NotGiven` in `CalculatedValue`
      // to avoid the need to do this.
      given Semigroup[CalculatedValue[CategorizedTime]] =
        CommutativeMonoid[CalculatedValue[CategorizedTime]]

      // CategorizedTime Ordering that sorts longest to shortest.
      val longestToShortest: Ordering[CategorizedTime] =
        catsKernelOrderingForOrder(using Order.reverse(Order[CategorizedTime]))

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
              children.map(_.value.max).sorted(using longestToShortest).take(minRequired).combineAllOption.getOrElse(CategorizedTime.Zero)
            )
          )

      def leafRange(
        oid: Observation.Id,
        m:   Map[Observation.Id, CalculatedValue[CategorizedTime]]
      ): Option[CalculatedValue[CategorizedTimeRange]] =
        m.get(oid).map: cv =>
          CalculatedValue(cv.state, CategorizedTimeRange.single(cv.value))

      def parentRange(
        minRequired: Option[NonNegShort],
        children:    List[Option[CalculatedValue[CategorizedTimeRange]]]
      ): Option[CalculatedValue[CategorizedTimeRange]] =
        // combine after skipping any elements for which we cannot compute the
        // time estimate
        val valid = children.flattenOption

        // If no expicit `minRequired` is set, only count the complete and
        // Approved (i.e., valid) observations.
        combine(minRequired.fold(valid.size)(_.value.toInt), valid)

      // The range for `root`, along with the range of every group within it,
      // computed in a single bottom-up pass.
      def timeEstimateRanges(
        root: GroupTree,
        m:    Map[Observation.Id, CalculatedValue[CategorizedTime]]
      ): (Option[CalculatedValue[CategorizedTimeRange]], Map[Group.Id, Option[CalculatedValue[CategorizedTimeRange]]]) =
        val groups = Map.newBuilder[Group.Id, Option[CalculatedValue[CategorizedTimeRange]]]

        def go(t: GroupTree): Option[CalculatedValue[CategorizedTimeRange]] =
          t match
            case GroupTree.Leaf(oid)                                                     =>
              leafRange(oid, m)
            case GroupTree.Branch(groupId = gid, minRequired = min, children = children) =>
              val r = parentRange(min, children.map(go))
              groups += gid -> r
              r
            case GroupTree.Root(_, children)                                             =>
              parentRange(None, children.map(go))

        val r = go(root)
        (r, groups.result())

      // The observations in every group within `root`, computed in a single
      // bottom-up pass.
      def groupObservations(root: GroupTree): Map[Group.Id, Set[Observation.Id]] =
        val groups = Map.newBuilder[Group.Id, Set[Observation.Id]]

        def go(t: GroupTree): Set[Observation.Id] =
          t match
            case GroupTree.Leaf(oid)                                => Set(oid)
            case GroupTree.Branch(groupId = gid, children = children) =>
              val os = children.foldLeft(Set.empty[Observation.Id])(_ ++ go(_))
              groups += gid -> os
              os
            case GroupTree.Root(_, children)                        =>
              children.foldLeft(Set.empty[Observation.Id])(_ ++ go(_))

        go(root)
        groups.result()

      // Time for the given observations, summed by band.
      def bandedTime(
        oids:    Iterable[Observation.Id],
        bandMap: Map[Observation.Id, Option[ScienceBand]],
        m:       Map[Observation.Id, CalculatedValue[CategorizedTime]]
      ): Map[Option[ScienceBand], CalculatedValue[CategorizedTime]] =
        oids
          .toList
          .flatMap(oid => bandMap.get(oid).map(_ -> m.getOrElse(oid, CalculatedValue.empty[CategorizedTime])))
          .groupMapReduce(_._1)(_._2)(_ |+| _)

      private def load(
        pid: Program.Id
      )(using Transaction[F]): F[(GroupTree, Map[Observation.Id, CalculatedValue[CategorizedTime]])] =
        (groupService.selectGroups(pid), obscalcService.selectProgramCategorizedTime(pid)).tupled

      override def estimateProgramRange(
        pid: Program.Id
      )(using Transaction[F]): F[Option[CalculatedValue[CategorizedTimeRange]]] =
        load(pid).map:
          case (tree, valueMap) => timeEstimateRanges(tree, valueMap)._1

      override def estimateProgramBanded(
        pid: Program.Id
      )(using Transaction[F]): F[Map[Option[ScienceBand], CalculatedValue[CategorizedTime]]] =
        (observationService.selectBands(pid),
         obscalcService.selectProgramCategorizedTime(pid)
        ).tupled
         .map: (bandMap, valueMap) =>
           bandedTime(bandMap.keys, bandMap, valueMap)

      override def estimateGroups(
        trees:         List[GroupService.ProgramTree],
        includeBanded: Boolean
      )(using Transaction[F]): F[Map[Group.Id, GroupEstimate]] =
        trees
          .flatTraverse: pt =>
            (obscalcService.selectProgramCategorizedTime(pt.programId),
             if includeBanded then observationService.selectBands(pt.programId)
             else Map.empty[Observation.Id, Option[ScienceBand]].pure[F]
            ).mapN: (valueMap, bandMap) =>
              val ranges = timeEstimateRanges(pt.tree, valueMap)._2
              val obs    = if includeBanded then groupObservations(pt.tree) else Map.empty[Group.Id, Set[Observation.Id]]
              pt.groupIds.map: gid =>
                gid -> GroupEstimate(
                  ranges.get(gid).flatten,
                  bandedTime(obs.getOrElse(gid, Set.empty), bandMap, valueMap)
                )
          .map: found =>
            val m = found.toMap
            trees.flatMap(_.groupIds).map(gid => gid -> m.getOrElse(gid, GroupEstimate.Empty)).toMap
