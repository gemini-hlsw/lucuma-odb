// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import grackle.Result
import grackle.ResultT
import lucuma.core.model.Access
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.Config
import lucuma.odb.data.Existence
import lucuma.odb.data.GroupTree
import lucuma.odb.data.GroupTree.Branch
import lucuma.odb.data.GroupTree.Leaf
import lucuma.odb.data.GroupTree.Root
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.CloneGroupInput
import lucuma.odb.graphql.input.CreateGroupInput
import lucuma.odb.graphql.input.GroupPropertiesInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.util.Codecs.*
import org.http4s.client.Client
import skunk.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

trait GroupService[F[_]] {
  def createGroup(input: CreateGroupInput, system: Boolean = false)(using Transaction[F]): F[Result[Group.Id]]
  def updateGroups(SET: GroupPropertiesInput.Edit, which: AppliedFragment)(using Transaction[F]): F[Result[List[Group.Id]]]
  def selectGroups(programId: Program.Id)(using Transaction[F]): F[GroupTree]
  def selectPid(groupId: Group.Id)(using Transaction[F]): F[Option[Program.Id]]
  def cloneGroup(input: CloneGroupInput)(using Transaction[F]): F[Result[Group.Id]]

  def deleteSystemGroup(pid: Program.Id, groupId: Group.Id)(using Transaction[F], ServiceAccess): F[Result[Unit]]
  
}

object GroupService {

  object GroupElement:
    type Id = Either[Group.Id, Observation.Id]

  // TODO: check access control

  def instantiate[F[_]: Concurrent](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): GroupService[F] =
    new GroupService[F] {

      private def createGroupImpl(pid: Program.Id, SET: GroupPropertiesInput.Create, initialContents: List[Either[Group.Id, Observation.Id]], system: Boolean)(using Transaction[F]): F[Group.Id] =
        for {
          _ <- session.execute(sql"SET CONSTRAINTS ALL DEFERRED".command)
          i <- openHole(pid, SET.parentGroupId, SET.parentGroupIndex)
          g <- session.prepareR(Statements.InsertGroup).use(_.unique(((pid, SET), i), system))
          _ <- initialContents.traverse:
            case Left(c)  => moveGroupToEnd(c, g)
            case Right(o) => moveObservationToEnd(o, g)
        } yield g

      private def moveGroupToEnd(child: Group.Id, parent: Group.Id): F[Unit] =
        moveGroups(Nullable.NonNull(parent), None, sql"$group_id"(child), AppliedFragment.empty).void

      private def moveObservationToEnd(child: Observation.Id, parent: Group.Id)(using Transaction[F]): F[Unit] =
        observationService.updateObservations(
          Services.asSuperUser:
            AccessControl.unchecked(
              ObservationPropertiesInput.Edit.Empty.copy(group = Nullable.NonNull(parent)), 
              List(child),
              observation_id
            )
        ).void

      override def createGroup(input: CreateGroupInput, system: Boolean)(using Transaction[F]): F[Result[Group.Id]] =
        programService(emailConfig, httpClient).resolvePid(input.programId, input.proposalReference, input.programReference).flatMap: r =>
          r.traverse(createGroupImpl(_, input.SET, input.initialContents, system))
      
      // This saves a bit of annoyance below
      extension [A](self: List[A]) private def traverseNel_[F[_]: Applicative, B](f: NonEmptyList[A] => F[B]): F[Unit] =
        NonEmptyList.fromList(self).traverse(f).void

      // I don't know why this isn't defined already
      extension [F[_]: Monad, A](self: ResultT[F,A]) def >>[B](fb: ResultT[F, B]): ResultT[F, B] =
        self.flatMap(_ => fb)

      override def deleteSystemGroup(pid: Program.Id, groupId: Group.Id)(using Transaction[F], ServiceAccess): F[Result[Unit]] =
        session.execute(sql"set constraints all deferred".command) *>
        selectGroups(pid).flatMap: root =>
          root.findGroup(groupId) match
            case None    => OdbError.InvalidArgument(s"No such group $groupId in $pid.".some).asFailureF
            case Some(g) => 
              ResultT.fromResult(toDelete(g))
                .flatMap: (gs, os) =>                  
                  os.traverseNel_(nel => ResultT(observationService.deleteCalibrationObservations(nel))) >>
                  gs.traverseNel_(nel => deleteEmptySystemGroups(nel))
                .value
                .flatTap: r =>
                  transaction.rollback.whenA(r.isFailure)

      private def deleteEmptySystemGroups(gs: NonEmptyList[Group.Id]): ResultT[F, Unit] =
        ResultT:
          val enc: Encoder[gs.type] = group_id.nel(gs)
          moveGroups(Nullable.Null, None, sql"$enc"(gs), AppliedFragment.empty) *>
          session.prepareR(Statements.deleteSystemGroups(enc)).use: pq =>
            pq.stream(gs, 1024)
              .compile
              .toList
              .flatMap: deleted =>
                if deleted.toSet == gs.toList.toSet then Result.unit.pure[F]
                else OdbError.InvalidArgument(s"Cannot delete non-system groups.".some).asFailureF

      private def toDelete(tree: GroupTree): Result[(List[Group.Id], List[Observation.Id])] =
        tree match
          case Root(programId, children) => 
            OdbError.InvalidArgument(s"Cannot delete root group of $programId.".some).asFailure
          case Branch(groupId, _, _, children, _, _, _, _, false) =>
              OdbError.InvalidArgument(s"Cannot delete non-sytem group $groupId.".some).asFailure
          case Leaf(observationId) => 
            Result((Nil, List(observationId))) // calibration-ness is checked by the obs service    
          case Branch(groupId, _, _, children, _, _, _, _, true) =>
              children
                .traverse(toDelete)
                .map(_.combineAll)
                .map: (gs, os) => 
                  (groupId :: gs, os)            

      // Clone `oid` into `dest`, at the end.
      private def cloneObservationInto(oid: Observation.Id, dest: Option[Group.Id])(using Transaction[F]): ResultT[F, Observation.Id] =
        ResultT(
          observationService.cloneObservation(
            Services.asSuperUser:
              AccessControl.unchecked(
                Some(
                  ObservationPropertiesInput.Edit.Empty.copy(
                    group = Nullable.orNull(dest)
                  )
                ),
                oid,
                observation_id
              )
          )
        ).map(_.cloneId)

      // Clone `gid` into `dest`, at the end, as an empty group.
      private def cloneAsEmptyGroupInto(gid: Group.Id, dest: Option[Group.Id])(using Transaction[F]): ResultT[F, Group.Id] =
        selectGroupAsInput(gid).flatMap: input => 
          ResultT(createGroup(input.copy(SET = input.SET.copy(parentGroupId = dest, parentGroupIndex = None))))
        
      // Clone `gid` into `dest`, at the end, and clone its contents too.
      private def cloneGroupInto(gid: Group.Id, dest: Option[Group.Id])(using Transaction[F]): ResultT[F, Group.Id] =
        for
          gid0 <- cloneAsEmptyGroupInto(gid, dest)
          es   <- selectGroupElements(gid) // find the children of the source group
          _    <- es.traverse(cloneGroupElementInto(_, Some(gid0))) // clone the children, recursively, into the new group
        yield gid

      // Clone the `elem` into `dest`, at the end.
      private def cloneGroupElementInto(elem: GroupElement.Id, dest: Option[Group.Id])(using Transaction[F]): ResultT[F, GroupElement.Id] =
        elem match
          case Left(gid)  => cloneGroupInto(gid, dest).map(_.asLeft)
          case Right(oid) => cloneObservationInto(oid, dest).map(_.asRight)

      /** Construct a CreateGroupInput that would clone `gid`. */
      private def selectGroupAsInput(gid: Group.Id): ResultT[F, CreateGroupInput] =
        ResultT:
          session.prepareR(Statements.SelectGroupAsInput).use: pq =>
            pq.unique(gid).map:
              case (cgi, false) => Result.success(cgi)
              case (_, true)    => OdbError.UpdateFailed(Some("System groups cannot be cloned.")).asFailure

      // Select the elements of `gid`, in order.
      private def selectGroupElements(gid: Group.Id): ResultT[F, List[GroupElement.Id]] =
        ResultT:
          session.prepareR(Statements.SelectGroupElements).use: pq =>
            pq.stream(gid, 1024).compile.toList.map(Result.success)

      // Clone `gid` as a sibling.
      private def cloneGroupImpl(input: CloneGroupInput)(using Transaction[F]): ResultT[F, Group.Id] =
        for
          cgi   <- selectGroupAsInput(input.groupId)
          cgiʹ   = cgi.copy(SET = input.SET.foldLeft(cgi.SET)(_.withEdit(_)))
          clone <- ResultT(createGroup(cgiʹ, false))
          elems <- selectGroupElements(input.groupId)
          _     <- elems.traverse(cloneGroupElementInto(_, Some(clone)))
        yield clone

      def cloneGroup(input: CloneGroupInput)(using Transaction[F]): F[Result[Group.Id]] =
        cloneGroupImpl(input).value

      // Applying the same move to a list of groups will put them all together in the
      // destination group (or at the top level) in no particular order. Returns the ids of
      // directly affected groups, but not those who were shuffled as a side-effect.
      def moveGroups(
        groupId: Nullable[Group.Id],
        groupIndex: Option[NonNegShort],
        which: AppliedFragment,
        accessPredicate: AppliedFragment
      ): F[List[Group.Id]] =
        (groupId, groupIndex) match
          case (Nullable.Absent, None) => Nil.pure[F] // do nothing if neither is specified
          case (gid, index) =>
            val af = Statements.moveGroups(gid.toOption, index, which, accessPredicate)
            session.prepareR(af.fragment.query(group_id *: void)).use(pq => pq.stream(af.argument, 512).map(_._1).compile.toList)

      def updateGroups(SET: GroupPropertiesInput.Edit, which: AppliedFragment)(using Transaction[F]): F[Result[List[Group.Id]]] =
        val accessPredicate = user.role.access match
            case Access.Service => void""
            case _              => void" AND c_system = FALSE" // Non service can't update system groups
        session.execute(sql"SET CONSTRAINTS ALL DEFERRED".command) >>
        moveGroups(SET.parentGroupId, SET.parentGroupIndex, which, accessPredicate).flatMap { ids =>
          Statements.updateGroups(SET, which, accessPredicate).traverse { af =>
            session.prepareR(af.fragment.query(group_id)).use { pq => pq.stream(af.argument, 512).compile.toList }
          }
          .map(moreIds => Result(moreIds.foldLeft(ids)((a, b) => (a ++ b).distinct)))
          .recoverWith { case SqlState.CheckViolation(ex) =>  Result.failure("Minimum interval must be less than or equal maximum interval.").pure}
        }

      @annotation.nowarn("msg=unused implicit parameter")
      def openHole(pid: Program.Id, gid: Option[Group.Id], index: Option[NonNegShort])(using Transaction[F]): F[NonNegShort] =
        session.prepareR(Statements.OpenHole).use(_.unique(pid, gid, index))

      def selectGroups(programId: Program.Id)(using Transaction[F]): F[GroupTree] = {

        def mkTree(m: Map[Option[Group.Id], List[GroupTree.Child]]): GroupTree = {

          def mapChildren(children: List[GroupTree.Child]): List[GroupTree.Child] =
            children.map {
              case l@GroupTree.Leaf(_)                           => l
              case b@GroupTree.Branch(_, _, _, _, _, _, _, _, _) => mapBranch(b)
            }

          def mapBranch(p: GroupTree.Branch): GroupTree.Branch =
            p.copy(children = mapChildren(m.get(p.groupId.some).toList.flatten))

          GroupTree.Root(programId, mapChildren(m.get(none[Group.Id]).toList.flatten))
        }

        for {
          gs <- session.execute(Statements.SelectGroups)(programId)
          os <- session.execute(Statements.SelectObservations)(programId)
        } yield mkTree((gs ++ os).groupBy(_._1).view.mapValues(_.sortBy(_._2.value).map(_._3)).toMap)

      }

      def selectPid(groupId: Group.Id)(using Transaction[F]): F[Option[Program.Id]] =
        session.option(Statements.SelectPid)(groupId)

    }

  object Statements {

    val InsertGroup: Query[Program.Id ~ GroupPropertiesInput.Create ~ NonNegShort ~ Boolean, Group.Id] =
      sql"""
      insert into t_group (
        c_program_id,
        c_parent_id,
        c_parent_index,
        c_name,
        c_description,
        c_min_required,
        c_ordered,
        c_min_interval,
        c_max_interval,
        c_existence,
        c_system
      ) values (
        $program_id,
        ${group_id.opt},
        ${int2_nonneg},
        ${text_nonempty.opt},
        ${text_nonempty.opt},
        ${int2_nonneg.opt},
        $bool,
        ${time_span.opt},
        ${time_span.opt},
        $existence,
        $bool
      ) returning c_group_id
      """.query(group_id)
         .contramap[Program.Id ~ GroupPropertiesInput.Create ~ NonNegShort ~ Boolean] { case (((pid, c), index), system) => (
          pid,
          c.parentGroupId,
          index,
          c.name,
          c.description,
          c.minimumRequired,
          c.ordered,
          c.minimumInterval,
          c.maximumInterval,
          c.existence,
          system
        )}

    val OpenHole: Query[(Program.Id, Option[Group.Id], Option[NonNegShort]), NonNegShort] =
      sql"select group_open_hole($program_id, ${group_id.opt}, ${int2_nonneg.opt})".query(int2_nonneg)

    def updateGroups(SET: GroupPropertiesInput.Edit, which: AppliedFragment, accessPredicate: AppliedFragment): Option[AppliedFragment] = {

      val fieldUpdates: List[AppliedFragment] =
        List(
          SET.name.toOptionOption.map(sql"c_name = ${text_nonempty.opt}"),
          SET.description.toOptionOption.map(sql"c_description = ${text_nonempty.opt}"),
          SET.ordered.map(sql"c_ordered = $bool"),
          SET.minimumRequired.toOptionOption.map(sql"c_min_required = ${int2_nonneg.opt}"),
          SET.minimumInterval.toOptionOption.map(sql"c_min_interval = ${time_span.opt}"),
          SET.maximumInterval.toOptionOption.map(sql"c_max_interval = ${time_span.opt}"),
          SET.existence.map(sql"c_existence = $existence"),
        ).flatten

      val idPredicate: AppliedFragment =
        void" c_group_id IN (" |+| which |+| void")";

      val coda: AppliedFragment =
        void" WHERE" |+| idPredicate |+| accessPredicate |+| void" RETURNING c_group_id"

      NonEmptyList.fromList(fieldUpdates).map { updates =>
        updates.foldSmash(
          void"UPDATE t_group SET ",
          void", ",
          coda
        )
      }

    }

    def moveGroups(gid: Option[Group.Id], index: Option[NonNegShort], which: AppliedFragment, access: AppliedFragment): AppliedFragment =
      sql"""
        SELECT c_group_id, group_move_group(c_group_id, ${group_id.opt}, ${int2_nonneg.opt})
        FROM t_group
        WHERE c_group_id IN (
      """.apply(gid, index) |+| which |+| access |+| void")"

    val branch: Decoder[GroupTree.Branch] =
      (group_id *: text_nonempty.opt *: text_nonempty.opt *: int2_nonneg.opt *: bool *:  time_span.opt *: time_span.opt *: bool).map {
        case (gid, name, description, minRequired, ordered, minInterval, maxInterval, system) =>
          GroupTree.Branch(gid, minRequired, ordered, Nil, name, description, minInterval, maxInterval, system)
      }

    val SelectGroups: Query[Program.Id, (Option[Group.Id], NonNegShort, GroupTree.Branch)] =
      sql"""
        SELECT
          c_parent_id,
          c_parent_index,
          c_group_id,
          c_name,
          c_description,
          c_min_required,
          c_ordered,
          c_min_interval,
          c_max_interval,
          c_system
        FROM
          t_group
        WHERE
          c_program_id = $program_id
      """.query(group_id.opt *: int2_nonneg *: branch)

    val SelectObservations: Query[Program.Id, (Option[Group.Id], NonNegShort, GroupTree.Leaf)] =
      sql"""
        SELECT
          c_group_id,
          c_group_index,
          c_observation_id
        FROM
          t_observation
        WHERE
          c_program_id = $program_id AND c_existence = 'present'
      """.query(group_id.opt *: int2_nonneg *: observation_id)
         .map { case (gid, index, oid) => (gid, index, GroupTree.Leaf(oid)) }

     val SelectPid: Query[Group.Id, Program.Id] =
       sql"""
         SELECT
           c_program_id
         FROM
           t_group
         WHERE
           c_group_id = $group_id
       """.query(program_id)

      /** Select group elements in a group, in order. */
      val SelectGroupElements: Query[Group.Id, GroupElement.Id] =
        sql"""
          SELECT c_group_id, c_observation_id
          FROM (
            SELECT null c_group_id, c_observation_id, c_group_index
            FROM t_observation WHERE c_group_id = $group_id
            UNION
            SELECT c_group_id, null, c_parent_index
            FROM t_group WHERE c_parent_id = $group_id
          ) sub ORDER BY c_group_index
        """
          .contramap[Group.Id](a => (a, a))
          .query(
            (group_id.opt ~ observation_id.opt).emap:
              case (None, Some(oid)) => Right(Right(oid))
              case (Some(gid), None) => Right(Left(gid))
              case (a, b)            => Left("SelectGroupElements: unpossible row: $a, $b")
          )

      /** Select a `CreateGroupInput` for a given `Group.Id` that can be used to create an empty clone. */
      val SelectGroupAsInput: Query[Group.Id, (CreateGroupInput, Boolean)] =
        sql"""
          SELECT
            c_program_id,
            c_parent_id,
            c_parent_index,
            c_name,
            c_description,
            c_min_required,
            c_ordered,
            c_min_interval,
            c_max_interval,
            c_existence,
            c_system
          FROM
            t_group
          WHERE
            c_group_id = $group_id
        """.query(
            program_id ~ 
            group_id.opt ~
            int2_nonneg ~
            text_nonempty.opt ~
            text_nonempty.opt ~
            int2_nonneg.opt ~
            bool ~
            time_span.opt ~
            time_span.opt ~
            existence ~
            bool
          ).map: 
            case pid ~ gid ~ gix ~ nam ~ des ~ mre ~ ord ~ min ~ max ~ exi ~ sys =>
              (CreateGroupInput(
                programId = Some(pid),
                proposalReference = None,
                programReference = None,
                SET = GroupPropertiesInput.Create(
                  name = nam,
                  description = des,
                  minimumRequired = mre,
                  ordered = ord,
                  minimumInterval = min,
                  maximumInterval = max,
                  parentGroupId = gid,
                  parentGroupIndex = Some(gix),
                  existence = exi,
                ),
                initialContents = Nil
              ), sys)

      def deleteSystemGroups[A <: NonEmptyList[Group.Id]](enc: Encoder[A]): Query[A, Group.Id] =
        sql"""
          DELETE FROM t_group
          WHERE c_group_id in ($enc)
          AND c_system = true
          RETURNING c_group_id
        """.query(group_id)

  }

}
