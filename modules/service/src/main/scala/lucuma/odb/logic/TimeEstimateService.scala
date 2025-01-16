// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.BandedTime
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.itc.client.ItcClient
import lucuma.odb.data.GroupTree
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.GeneratorParamsService
import lucuma.odb.service.ItcService
import lucuma.odb.service.ItcService.AsterismResults
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

/**
 * A service that can estimate the time for a program.
 */
sealed trait TimeEstimateService[F[_]]:

  /**
   * Estimates the remaining time for all observations in the program that are
   * well defined.
   */
  def estimateProgramRange(
    programId: Program.Id
  )(using NoTransaction[F]): F[Option[CategorizedTimeRange]]

  /**
   * Estimates the remaining time for all observations in the group that are
   * well defined.
   */
  def estimateGroupRange(
    groupId: Group.Id
  )(using NoTransaction[F]): F[Option[CategorizedTimeRange]]

  /**
   * Estimates the remaining time for all observations in the program by band,
   * ignoring `minRequired` in groups.
   */
  def estimateProgramBanded(
    programId: Program.Id
  )(using NoTransaction[F]): F[Option[Map[Option[ScienceBand], CategorizedTime]]]

  /**
   * Estimates the remaining time for all observations in the group by band,
   * ignoring `minRequired`.
   */
  def estimateGroupBanded(
    groupId: Group.Id
  )(using NoTransaction[F]): F[Option[Map[Option[ScienceBand], CategorizedTime]]]


object TimeEstimateService:

  private case class ObservationData(
    generatorParams: GeneratorParams,
    asterismResults: Option[AsterismResults],
    executionDigest: Option[ExecutionDigest]
  ):
    def cachedFullTimeEstimate: Option[BandedTime] =
      executionDigest.map(dig => BandedTime(generatorParams.scienceBand, dig.fullTimeEstimate))

  private object ObservationData:
    def load[F[_]: Concurrent](
      pid:       Program.Id,
      itcClient: ItcClient[F]
    )(using Services[F], Transaction[F]): F[Map[Observation.Id, ObservationData]] =
      for
        p <- generatorParamsService.selectAll(pid /*, ObsStatus.Included*/)
        pʹ = p.collect { case (oid, Right(gp)) => (oid, gp) }
        i <- itcService(itcClient).selectAll(pid, pʹ)
        d <- executionDigestService.selectAll(pid)
        dʹ = d.map { case (oid, (_, digest)) => (oid, digest) }
      yield pʹ.map { case (oid, gp) => (oid, ObservationData(gp, i.get(oid), dʹ.get(oid))) }

  def instantiate[F[_]: Concurrent](
    commitHash:   CommitHash,
    itcClient:    ItcClient[F],
    calculator:   TimeEstimateCalculatorImplementation.ForInstrumentMode,
  )(using Services[F]): TimeEstimateService[F] =
    new TimeEstimateService[F]:

      // CategorizedTime Ordering that sorts longest to shortest.
      val longestToShortest: Ordering[CategorizedTime] =
        catsKernelOrderingForOrder(Order.reverse(Order[CategorizedTime]))

      def observationTime(
        pid: Program.Id,
        oid: Observation.Id,
        m:   Map[Observation.Id, ObservationData]
      ): F[Option[BandedTime]] =
        OptionT.fromOption(m.get(oid)).flatMap: data =>
          OptionT
            .fromOption(data.cachedFullTimeEstimate)  // try the cache first
            .orElse:
              // ExecutionDigest not in the cache, we'll need to calculate it.
              // For that we need the ITC results, which may be cached.  Use the
              // cached ITC AsterismResults if available, else call remote ITC.
              val asterismResults = OptionT.fromOption(data.asterismResults).map(_.asRight).orElseF(
                itcService(itcClient)
                  .callRemote(pid, oid, data.generatorParams)
                  .map:
                    case Left(ItcService.Error.ObservationDefinitionError(GeneratorParamsService.Error.MissingData(p))) =>
                      p.asLeft[AsterismResults].some
                    case Left(_)   => none
                    case Right(ar) => ar.asRight.some
              )

              // Calculate time estimate using provided ITC result and params.
              asterismResults.flatMapF: ar =>
                generator(commitHash, itcClient, calculator)
                  .calculateDigest(pid, oid, ar, data.generatorParams)
                  .map(_.toOption.map(dig => BandedTime(data.generatorParams.scienceBand, dig.fullTimeEstimate)))
        .value

      def combine(minRequired: Int, children: List[CategorizedTimeRange]): Option[CategorizedTimeRange] =
        Option.when(children.size >= minRequired):
          CategorizedTimeRange.from(
            // Combines the first minRequired elements after sorting by ascending min CategorizedTime
            children.map(_.min).sorted.take(minRequired).combineAllOption.getOrElse(CategorizedTime.Zero),
            // Combines the first minRequired elements after sorting by descending max CategorizedTime
            children.map(_.max).sorted(longestToShortest).take(minRequired).combineAllOption.getOrElse(CategorizedTime.Zero)
          )

      def leafRange(
        pid: Program.Id,
        oid: Observation.Id,
        m:   Map[Observation.Id, ObservationData]
      ): F[Option[CategorizedTimeRange]] =
        observationTime(pid, oid, m).map(_.map(bt => CategorizedTimeRange.single(bt.time)))

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
        root match
          case GroupTree.Leaf(oid)                                  => leafRange(pid, oid, m)
          case GroupTree.Branch(_, min, _, children, _, _, _, _, _) => parentRange(pid, min, children, m)
          case GroupTree.Root(_, children)                          => parentRange(pid, None, children, m)

      private def load(pid: Program.Id): F[(GroupTree, Map[Observation.Id, ObservationData])] =
        services.transactionally:
          (groupService.selectGroups(pid), ObservationData.load(pid, itcClient)).tupled

      override def estimateProgramRange(
        pid: Program.Id
      )(using NoTransaction[F]): F[Option[CategorizedTimeRange]] =
        load(pid).flatMap:
          case (tree, dataMap) => timeEstimateRange(pid, tree, dataMap)

      override def estimateGroupRange(
        gid: Group.Id
      )(using NoTransaction[F]): F[Option[CategorizedTimeRange]] =
        (for
          p <- OptionT(groupService.selectPid(gid))
          d <- OptionT.liftF(load(p))
          (tree, dataMap) = d
          t <- OptionT.fromOption(tree.findGroup(gid))
          r <- OptionT(timeEstimateRange(p, t, dataMap))
        yield r).value

      def bandedTimeEstimate(
        pid:  Program.Id,
        root: GroupTree,
        m:    Map[Observation.Id, ObservationData]
      ): F[Map[Option[ScienceBand], CategorizedTime]] =

        val empty: Map[Option[ScienceBand], CategorizedTime] = Map.empty

        def leaf(oid: Observation.Id): F[Map[Option[ScienceBand], CategorizedTime]] =
          observationTime(pid, oid, m).map: obt =>
            obt.fold(empty)(bt => Map(bt.band -> bt.time))

        def parent(cs: List[GroupTree.Child]): F[Map[Option[ScienceBand], CategorizedTime]] =
          cs.traverse(bandedTimeEstimate(pid, _, m)).map(_.combineAll)

        root match
          case GroupTree.Leaf(oid)                                => leaf(oid)
          case GroupTree.Branch(_, _, _, children, _, _, _, _, _) => parent(children)
          case GroupTree.Root(_, children)                        => parent(children)

      override def estimateProgramBanded(
        pid: Program.Id
      )(using NoTransaction[F]): F[Option[Map[Option[ScienceBand], CategorizedTime]]] =
        load(pid).flatMap:
          case (tree, dataMap) => bandedTimeEstimate(pid, tree, dataMap).map(_.some)

      override def estimateGroupBanded(
        gid: Group.Id
      )(using NoTransaction[F]): F[Option[Map[Option[ScienceBand], CategorizedTime]]] =
        (for
          p <- OptionT(groupService.selectPid(gid))
          d <- OptionT.liftF(load(p))
          (tree, dataMap) = d
          t <- OptionT.fromOption(tree.findGroup(gid))
          r <- OptionT.liftF(bandedTimeEstimate(p, t, dataMap))
        yield r).value