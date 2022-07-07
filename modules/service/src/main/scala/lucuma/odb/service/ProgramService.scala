// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.GuestRole
import lucuma.core.model.Program
import lucuma.core.model.ServiceRole
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.odb.data.Nullable.Absent
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.data._
import lucuma.odb.service.ProgramService.LinkUserRequest.PartnerSupport
import lucuma.odb.service.ProgramService.LinkUserRequest.StaffSupport
import lucuma.odb.util.Codecs._
import natchez.Trace
import skunk._
import skunk.data.Completion
import skunk.syntax.all._

trait ProgramService[F[_]] {

  /**
   * Insert a new program, where the calling user becomes PI (unless it's a Service user, in which
   * case the PI is left empty.
   */
  def insertProgram(name: Option[NonEmptyString]): F[Program.Id]

  /** Update a program, where `userId` is the current user. */
  def updateProgram(
    programId: Program.Id,
    ex: Option[Existence],
    name: Nullable[NonEmptyString],
  ): F[UpdateResult[Program.Id]]

  /** Delete a program with the given id, returning true if deleted, false if not found. */
  def deleteProgram(programId: Program.Id): F[Boolean]

  /**
   * Perform the requested program <-> user link, yielding the linked ids if successful, or None
   * if the user was not authorized to perform the action.
   */
  def linkUser(req: ProgramService.LinkUserRequest): F[ProgramService.LinkUserResponse]

}

object ProgramService {

  sealed abstract class LinkUserRequest(val role: ProgramUserRole, val supportType: Option[ProgramUserSupportType] = None, val supportPartner: Option[Tag] = None) {
    def programId: Program.Id
    def userId: User.Id
  }
  object LinkUserRequest {

    case class Coi(programId: Program.Id, userId: User.Id) extends LinkUserRequest(ProgramUserRole.Coi)
    case class Observer(programId: Program.Id, userId: User.Id) extends LinkUserRequest(ProgramUserRole.Observer)
    case class StaffSupport(programId: Program.Id, userId: User.Id) extends LinkUserRequest(ProgramUserRole.Support, Some(ProgramUserSupportType.Staff))
    case class PartnerSupport(programId: Program.Id, userId: User.Id, partnerTag: Tag) extends LinkUserRequest(ProgramUserRole.Support, Some(ProgramUserSupportType.Partner), Some(partnerTag))

    /** Construct a LinkedUserRequest from unvalidated inputs, if possible. */
    def validate(
      programId: Program.Id,
      userId: User.Id,
      role: ProgramUserRole,
      supportType: Option[ProgramUserSupportType],
      supportPartner: Option[Tag]
    ): Either[String, LinkUserRequest] =
      role match {
        case ProgramUserRole.Coi =>
          (supportType orElse supportPartner)
            .as("Support type/partner must not be specified for COI role.")
            .toLeft(Coi(programId, userId))
        case ProgramUserRole.Observer =>
          (supportType orElse supportPartner)
            .as("Support type/partner must not be specified for OBSERVER role.")
            .toLeft(Observer(programId, userId))
        case ProgramUserRole.Support =>
          (supportType, supportPartner) match {
            case (Some(ProgramUserSupportType.Staff), None)        => Right(StaffSupport(programId, userId))
            case (Some(ProgramUserSupportType.Staff), _)           => Left("Support partner must not be specified if support type is STAFF.")
            case (Some(ProgramUserSupportType.Partner), Some(tag)) => Right(PartnerSupport(programId, userId, tag))
            case (Some(ProgramUserSupportType.Partner), _)         => Left("Support partner must be specified if support type is PARTNER.")
            case (None, _)                                         => Left("Support type must be specifed if role is SUPPORT.")
          }
      }

  }

  sealed trait LinkUserResponse extends Product with Serializable
  object LinkUserResponse {
    case class NotAuthorized(user: User)                     extends LinkUserResponse
    case class AlreadyLinked(pid: Program.Id, user: User.Id) extends LinkUserResponse
    case class Success(pis: Program.Id, user: User.Id)       extends LinkUserResponse
    case class InvalidUser(user: User.Id)                    extends LinkUserResponse
  }

  /**
   * Construct a `ProgramService` using the specified `Session`, for the specified `User`. All
   * operations will be performed on behalf of `user`.
   */
  def fromSessionAndUser[F[_]: MonadCancelThrow: Trace](s: Session[F], user: User): ProgramService[F] =
    new ProgramService[F] {

      def fail[A](msg: String): F[A] =
        MonadCancelThrow[F].raiseError(new RuntimeException(msg))

      def insertProgram(name: Option[NonEmptyString]): F[Program.Id] =
        Trace[F].span("insertProgram") {
          s.prepare(Statements.InsertProgram).use(ps => ps.unique(name ~ user))
        }

      def updateProgram(
        programId: Program.Id,
        ex:        Option[Existence],
        name:      Nullable[NonEmptyString],
      ): F[UpdateResult[Program.Id]] =
        Trace[F].span("updateProgram") {
          Statements.updateProgram(programId, ex, name, user) match {
            case None => UpdateResult.NothingToBeDone.pure[F].widen
            case Some(af) =>
              s.prepare(af.fragment.command).use(_.execute(af.argument)).flatMap {
                case Completion.Update(0) => UpdateResult.NoSuchObject.pure[F].widen
                case Completion.Update(1) => UpdateResult.Success(programId).pure[F].widen
                case other => fail(s"Expected `Update(0)` or `Update(1)`, found $other.")
              }
          }
        }

      def deleteProgram(programId: Program.Id): F[Boolean] =
        Trace[F].span("deleteProgram") {
          s.prepare(Statements.DeleteProgram).use(pc => pc.execute(programId))
            .flatMap {
              case Completion.Delete(0) => false.pure[F]
              case Completion.Delete(1) => true.pure[F]
              case other => fail(s"Expected `Delete(0)` or `Delete(1)`, found $other.")
            }
          }

      def linkUser(req: ProgramService.LinkUserRequest): F[LinkUserResponse] = {
        val af: Option[AppliedFragment] =
          req match {
            case LinkUserRequest.Coi(programId, userId) => Statements.linkCoi(programId, userId, user)
            case LinkUserRequest.Observer(programId, userId) => Statements.linkObserver(programId, userId, user)
            case StaffSupport(programId, userId) => Statements.linkStaffSupport(programId, userId, user)
            case PartnerSupport(programId, userId, partnerTag) => Statements.linkPartnerSupport(programId, userId, user, partnerTag)
          }
        af match {
          case None     =>  Monad[F].pure(LinkUserResponse.NotAuthorized(user))
          case Some(af) =>
            val stmt = sql"${af.fragment} RETURNING c_program_id, c_user_id".query(program_id ~ user_id)
            s.prepare(stmt).use { pq =>
              pq.option(af.argument).map {
                case Some(pid ~ uid) => LinkUserResponse.Success(pid, uid)
                case None            => LinkUserResponse.NotAuthorized(user)
              } .recover {
                case SqlState.UniqueViolation(_) => LinkUserResponse.AlreadyLinked(req.programId, req.userId)
                case SqlState.ForeignKeyViolation(_) => LinkUserResponse.InvalidUser(req.userId)
              }
            }
        }
      }

  }


  object Statements {

    def existsUserAsPi(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      sql"""
        EXISTS (select c_program_id from t_program where c_program_id = $program_id and c_pi_user_id = $user_id)
      """.apply(programId ~ userId)

    def existsUserAsCoi(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      sql"""
        EXISTS (select c_role from t_program_user where  c_program_id = $program_id and c_user_id = $user_id and c_role = 'coi')
      """.apply(programId ~ userId)

    def existsAllocationForPartner(
      programId: Program.Id,
      partner: Tag
    ): AppliedFragment =
      sql"""
        EXISTS (select c_duration from t_allocation where c_program_id = $program_id and c_partner=$tag and c_duration > 'PT')
        """.apply(programId ~ partner)

    def updateProgram(
      programId: Program.Id,
      ex: Option[Existence],
      name: Nullable[NonEmptyString],
      user: User
    ): Option[AppliedFragment] = {

      val base = void"update t_program set "

      val upExistence = sql"c_existence = $existence"
      val upName      = sql"c_name = ${text_nonempty.opt}"

      val ups: List[AppliedFragment] =
        List(
          ex.map(upExistence),
          name match {
            case Nullable.Null => Some(upName(None))
            case Absent => None
            case NonNull(value) => Some(upName(Some(value)))
          }
        ).flatten

      NonEmptyList.fromList(ups).map { nel =>

        val up = nel.intercalate(void", ")

        import lucuma.core.model.Access._

        val where = user.role.access match {

          case Service | Admin | Staff =>
            sql"""
              where p.c_program_id = $program_id
            """.apply(programId)

          case Ngo     => ??? // TODO

          case Guest | Pi =>
            sql"""
              where c_program_id = $program_id
              and (
                c_pi_user_id = $user_id
                or
                exists(
                  select c_role
                  from   t_program_user
                  where  c_program_id = $program_id
                  and    c_user_id = $user_id
                  and    c_role = 'coi'
                )
              )
            """.apply(programId ~ user.id ~ programId ~ user.id)

        }

        base |+| up |+| where

      }
    }

    /** Insert a program, making the passed user PI if it's a non-service user. */
    val InsertProgram: Query[Option[NonEmptyString] ~ User, Program.Id] =
      sql"""
        INSERT INTO t_program (c_name, c_pi_user_id, c_pi_user_type)
        VALUES (${text_nonempty.opt}, ${(user_id ~ user_type).opt})
        RETURNING c_program_id
      """.query(program_id)
         .contramap {
            case oNes ~ ServiceUser(_, _) => oNes ~ None
            case oNes ~ nonServiceUser    => oNes ~ Some(nonServiceUser.id ~ UserType.fromUser(nonServiceUser))
         }


    val DeleteProgram: Command[Program.Id] =
      sql"""
        DELETE FROM t_program
        WHERE c_program_id = $program_id
      """.command

    /** Link a user to a program, without any access checking. */
    val LinkUser: Fragment[Program.Id ~ User.Id ~ ProgramUserRole ~ Option[ProgramUserSupportType] ~ Option[Tag]] =
      sql"""
         INSERT INTO t_program_user (c_program_id, c_user_id, c_user_type, c_role, c_support_type, c_support_partner)
         SELECT $program_id, $user_id, 'standard', $program_user_role, ${program_user_support_type.opt}, ${tag.opt}
        """

    /**
     * Link a co-investigator to a program.
     * - Guests cannot do this.
     * - Staff, Admin, and Service users can always do this.
     * - Standard user can only do this if they're the program's PI.
     */
    def linkCoi(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      user: User, // current user
    ): Option[AppliedFragment] = {
      val up = LinkUser(targetProgram ~ targetUser ~ ProgramUserRole.Coi ~ None ~ None)
      user.role match {
        case GuestRole                    => None
        case ServiceRole(_)               => Some(up)
        case StandardRole.Admin(_)        => Some(up)
        case StandardRole.Ngo(_, partner) => Some(up |+| void" WHERE " |+| existsAllocationForPartner(targetProgram, Tag(partner.tag)))
        case StandardRole.Pi(_)           => Some(up |+| void" WHERE " |+| existsUserAsPi(targetProgram, user.id))
        case StandardRole.Staff(_)        => Some(up)
      }
    }

    /**
     * Link an observer to a program.
     * - Guests cannot do this.
     * - Staff, Admin, and Service users can always do this.
     * - Standard user can only do this if they're the program's PI or Coi.
     */
    def linkObserver(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      user: User, // current user
    ): Option[AppliedFragment] = {
      val up = LinkUser(targetProgram ~ targetUser ~ ProgramUserRole.Observer ~ None ~ None)
      user.role match {
        case GuestRole                    => None
        case ServiceRole(_)               => Some(up)
        case StandardRole.Admin(_)        => Some(up)
        case StandardRole.Ngo(_, partner) => Some(up |+| void" WHERE " |+| existsAllocationForPartner(targetProgram, Tag(partner.tag)))
        case StandardRole.Staff(_)        => Some(up)
        case StandardRole.Pi(_)           =>
          Some(
            up |+| void" WHERE " |+| existsUserAsPi(targetProgram, user.id)  |+|
                   void" OR "    |+| existsUserAsCoi(targetProgram, user.id)
          )
      }
    }

    /**
     * Link staff support to a program.
     * - Staff, Admin, and Service users can always do this.
     * - Nobody else can do this.
     */
    def linkStaffSupport(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      user: User, // current user
    ): Option[AppliedFragment] = {
      import lucuma.core.model.Access._
      val up = LinkUser(targetProgram ~ targetUser ~ ProgramUserRole.Support ~ Some(ProgramUserSupportType.Staff) ~ None)
      user.role.access match {
        case Admin | Staff | Service => Some(up) // ok
        case _                       => None // nobody else can do this
      }
    }

    /**
     * Link partner support to a program.
     * - Staff, Admin, and Service users can always do this.
     * - Nobody else can do this.
     */
    def linkPartnerSupport(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      user: User, // current user
      partner: Tag, // partner
    ): Option[AppliedFragment] = {
      import lucuma.core.model.Access._
      val up = LinkUser(targetProgram ~ targetUser ~ ProgramUserRole.Support ~ Some(ProgramUserSupportType.Partner) ~ Some(partner))
      user.role.access match {
        case Admin | Staff | Service => Some(up) // ok
        case _                       => None // nobody else can do this
      }
    }

  }

}