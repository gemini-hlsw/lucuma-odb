// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Access
import lucuma.core.model.GuestRole
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.UserType
import lucuma.odb.graphql.input.AddProgramUserInput
import lucuma.odb.graphql.input.ProgramUserPropertiesInput
import lucuma.odb.graphql.input.UnlinkUserInput
import lucuma.odb.util.Codecs.educational_status
import lucuma.odb.util.Codecs.gender
import lucuma.odb.util.Codecs.partner
import lucuma.odb.util.Codecs.partner_link
import lucuma.odb.util.Codecs.partner_link_type
import lucuma.odb.util.Codecs.program_id
import lucuma.odb.util.Codecs.program_user_role
import lucuma.odb.util.Codecs.user_id
import lucuma.odb.util.Codecs.user_type
import lucuma.sso.client.SsoGraphQlClient
import natchez.Trace
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.text.varchar
import skunk.data.Completion
import skunk.syntax.all.*

import Services.Syntax.*

trait ProgramUserService[F[_]]:

  /**
   * Perform the requested program <-> user link, yielding the linked ids if successful, or None
   * if the user was not authorized to perform the action.
   */
  def linkUser(
    req: ProgramUserService.LinkUserRequest
  )(using Transaction[F]): F[Result[(Program.Id, User.Id)]]

  /** Unlink the requested user, yielding true if the user was unlinkes, false if no such link existed. */
  def unlinkUser(
    input: UnlinkUserInput
  )(using Transaction[F], Services.PiAccess): F[Result[Boolean]]

  def updateProgramUsers(
    SET:   ProgramUserPropertiesInput,
    which: AppliedFragment
  )(using Transaction[F]): F[Result[List[(Program.Id, User.Id)]]]

  def addProgramUser(
    sso:   SsoGraphQlClient[F],
    input: AddProgramUserInput
  )(using NoTransaction[F]): F[Result[(Program.Id, User.Id)]]

  /** Check to see if the user has access to the given program. */
  def userHasAccess(
    programId: Program.Id
  )(using Transaction[F]): F[Boolean]

object ProgramUserService:
  sealed trait LinkUserRequest:
    def programId: Program.Id
    def userId: User.Id

  object LinkUserRequest:
    case class Coi(programId: Program.Id, partnerLink: PartnerLink, userId: User.Id)   extends LinkUserRequest
    case class CoiRo(programId: Program.Id, partnerLink: PartnerLink, userId: User.Id) extends LinkUserRequest
    case class Support(programId: Program.Id, userId: User.Id, tpe: ProgramUserRole.SupportPrimary.type | ProgramUserRole.SupportSecondary.type) extends LinkUserRequest

  sealed trait LinkUserResponse extends Product with Serializable:
    def toResult: Result[(Program.Id, User.Id)] =
      this match
        case LinkUserResponse.NotAuthorized(user)     => OdbError.NotAuthorized(user.id).asFailure
        case LinkUserResponse.AlreadyLinked(pid, uid) => OdbError.NoAction(Some(s"User $uid is already linked to program $pid.")).asFailure
        case LinkUserResponse.InvalidUser(uid)        => OdbError.InvalidUser(uid, Some(s"User $uid does not exist or is of a nonstandard type.")).asFailure
        case LinkUserResponse.Success(pid, user)      => Result((pid, user))

  object LinkUserResponse:
    case class NotAuthorized(user: User)                     extends LinkUserResponse
    case class AlreadyLinked(pid: Program.Id, user: User.Id) extends LinkUserResponse
    case class InvalidUser(user: User.Id)                    extends LinkUserResponse
    case class Success(pid: Program.Id, user: User.Id)       extends LinkUserResponse


  def instantiate[F[_]: Concurrent: Trace](using Services[F]): ProgramUserService[F] =
    new ProgramUserService[F]:

      def linkUserImpl(
        req: LinkUserRequest
      )(using Transaction[F]): F[LinkUserResponse] =
        val af: Option[AppliedFragment] =
          req match
            case LinkUserRequest.Coi(programId, partnerLink, userId) => Statements.linkCoi(programId, userId, partnerLink, user)
            case LinkUserRequest.CoiRo(programId, partnerLink, userId) => Statements.linkCoiReadOnly(programId, userId, partnerLink, user)
            case LinkUserRequest.Support(programId, userId, tpe) => Statements.linkSupport(programId, userId, user, tpe)

        af match
          case None     => Monad[F].pure(LinkUserResponse.NotAuthorized(user))
          case Some(af) =>
            val stmt = sql"${af.fragment} RETURNING c_program_id, c_user_id".query(program_id ~ user_id)
            session.prepareR(stmt).use: pq =>
              pq.option(af.argument).map:
                case Some(pid ~ uid) => LinkUserResponse.Success(pid, uid)
                case None            => LinkUserResponse.NotAuthorized(user)
              .recover:
                case SqlState.UniqueViolation(_)     => LinkUserResponse.AlreadyLinked(req.programId, req.userId)
                case SqlState.ForeignKeyViolation(_) => LinkUserResponse.InvalidUser(req.userId)

      def linkUser(req: LinkUserRequest)(using Transaction[F]): F[Result[(Program.Id, User.Id)]] =
        linkUserImpl(req).map(_.toResult)

      // delete a link unconditionally
      private def unlinkUnconditionally(input: UnlinkUserInput): F[Result[Boolean]] =
        session.prepareR(Statements.UnlinkUnconditionally).use: pc =>
          pc.execute(input.programId, input.userId).map:
            case Completion.Delete(1) => Result(true)
            case other                => Result.internalError(s"unlinkUnconditionally: unexpected completion: $other")

      private def unlinkCoi(input: UnlinkUserInput)(using Transaction[F], Services.PiAccess): F[Result[Boolean]] =
        user.role.access match
          case Access.Guest   => Result.internalError("unlinkCoi: guest user seen; should be impossible").pure[F]
          case Access.Ngo     => OdbError.NotAuthorized(user.id).asFailureF // not allowed
          case Access.Staff   |
               Access.Admin   |
               Access.Service => unlinkUnconditionally(input)
          case Access.Pi      =>
            session.prepareR(Statements.UnlinkCoiAsPi).use: pc =>
              pc.execute(user.id, input.programId, input.userId).map:
                case Completion.Delete(1) => Result(true)
                case Completion.Delete(0) => OdbError.NotAuthorized(user.id).asFailure
                case other                => Result.internalError(s"unlinkCoi: unexpected completion: $other")

      private def unlinkObserver(input: UnlinkUserInput)(using Transaction[F], Services.PiAccess): F[Result[Boolean]] =
        user.role.access match
          case Access.Guest   => Result.internalError("unlinkObserver: guest user seen; should be impossible").pure[F]
          case Access.Ngo     => OdbError.NotAuthorized(user.id).asFailureF // not allowed
          case Access.Staff   |
               Access.Admin   |
               Access.Service => unlinkUnconditionally(input)
          case Access.Pi      =>
            session.prepareR(Statements.UnlinkObserverAsPiOrCoi).use: pc =>
              pc.execute(user.id, input.programId, input.userId).map:
                case Completion.Delete(1) => Result(true)
                case Completion.Delete(0) => OdbError.NotAuthorized(user.id).asFailure
                case other                => Result.internalError(s"unlinkCoi: unexpected completion: $other")

      override def unlinkUser(
        input: UnlinkUserInput
      )(using Transaction[F], Services.PiAccess): F[Result[Boolean]] =
        // Figure out how the specified user is linked to the specified program, then call the appropriate
        // unlink method (which will verify that the *calling* user is allowed to perform the unlink).
        val R  = ProgramUserRole
        val af = Statements.selectLinkType(user, input.programId, input.userId)
        session.prepareR(af.fragment.query(program_user_role)).use: pq =>
          pq.option(af.argument).flatMap:
            case None             => Result(false).pure[F] // no such link, or not visible
            case Some(R.Pi)       => OdbError.InvalidArgument("Cannot unlink the PI".some).asFailureF
            case Some(R.Coi)      => unlinkCoi(input)
            case Some(R.CoiRO)    => unlinkObserver(input)
            case Some(R.SupportPrimary | R.SupportSecondary)  => requireStaffAccess(unlinkUnconditionally(input))

      override def updateProgramUsers(
        SET:   ProgramUserPropertiesInput,
        which: AppliedFragment
      )(using Transaction[F]): F[Result[List[(Program.Id, User.Id)]]] =
        Statements.updateProgramUsers(user, SET, which).fold(Nil.success.pure[F]): af =>
          session.prepareR(af.fragment.query(program_id *: user_id)).use: pq =>
            pq.stream(af.argument, chunkSize = 1024).compile.toList.map(_.success)

      override def addProgramUser(
        sso:   SsoGraphQlClient[F],
        input: AddProgramUserInput
      )(using NoTransaction[F]): F[Result[(Program.Id, User.Id)]] =
        val set0 = input.SET.getOrElse(ProgramUserPropertiesInput.Empty)
        val link = set0.partnerLink.getOrElse(PartnerLink.HasUnspecifiedPartner)
        val set  = ProgramUserPropertiesInput.partnerLink.replace(link.some)(set0)

        def linkRequest(u: User.Id): Result[LinkUserRequest] =
          input.role match
            case ProgramUserRole.Coi   => LinkUserRequest.Coi(input.programId, link, u).success
            case ProgramUserRole.CoiRO => LinkUserRequest.CoiRo(input.programId, link, u).success
            case _                     => Result.failure("Only CoI or CoI-read-only pre-auth users may be created.")

        // Add the user to SSO without holding open a transaction (because it is
        // a remote service call) and without yet validating that the PI has
        // access to the program listed in input. All this does is create an
        // otherwise empty user record with an orcidId. Once the user is added
        // to SSO, we'll open the transaction and then rely upon the call to
        // `linkRequest` to do the access checking.  If that is successful, it
        // is also safe to update the program user attributes.

        requirePiAccess:
          sso.canonicalizePreAuthUser(input.orcidId).flatMap: u =>
            session.transaction.use: xa =>
              (for
                _ <- ResultT.liftF(Services.asSuperUser(UserService.fromSession(session).canonicalizeUser(u)))
                r <- ResultT(linkRequest(u.id).flatTraverse(r => linkUser(r)(using xa)))
                _ <- ResultT(updateProgramUsers(set, sql"($program_id, $user_id)"(r))(using xa)).void
              yield r).value

      override def userHasAccess(
        programId: Program.Id
      )(using Transaction[F]): F[Boolean] =
        Statements.existsUserAccess(user, programId).fold(true.pure[F]): af =>
          val stmt = sql"SELECT ${af.fragment}".query(bool)
          session.prepareR(stmt).use: pg =>
            pg.unique(af.argument)

  end instantiate

  object Statements:

    def selectLinkType(user: User, pid: Program.Id, uid: User.Id): AppliedFragment =
      sql"""
        SELECT
          c_role
        FROM t_program_user
        WHERE
          c_program_id = $program_id
        AND
          c_user_id = $user_id
      """.apply(pid, uid) |+| existsUserAccess(user, pid).foldMap(void"AND " |+| _)

    val UnlinkUnconditionally: Command[(Program.Id, User.Id)] =
      sql"""
        DELETE FROM
          t_program_user
        WHERE
          c_program_id = $program_id
        AND
          c_user_id = $user_id
      """.command

    val UnlinkCoiAsPi: Command[(User.Id, Program.Id, User.Id)] =
      sql"""
        DELETE FROM t_program_user
        WHERE c_program_id = $program_id
        AND   c_user_id    = $user_id
        AND   c_role       = 'coi'
        AND EXISTS (
          SELECT 1
          FROM t_program_user u
          WHERE u.c_program_id = $program_id
          AND   u.c_user_id = $user_id
          AND   u.c_role = 'pi'
        )
      """
        .command
        .contramap((caller, pid, uid) => (pid, uid, pid, caller))

    val UnlinkObserverAsPiOrCoi: Command[(User.Id, Program.Id, User.Id)] =
      sql"""
        DELETE FROM t_program_user
        WHERE c_program_id = $program_id
        AND   c_user_id    = $user_id
        AND   c_role       = 'coi_ro'
        AND EXISTS (
          SELECT 1
          FROM t_program_user u
          WHERE u.c_program_id = $program_id
          AND   u.c_user_id    = $user_id
          AND   (u.c_role = 'pi' OR u.c_role = 'coi')
        )
      """
        .command
        .contramap((caller, pid, uid) => (pid, uid, pid, caller))

    /** Link a user to a program, without any access checking. */
    val LinkUser: Fragment[(Program.Id, User.Id, UserType, ProgramUserRole, PartnerLink)] =
      sql"""
        INSERT INTO t_program_user (c_program_id, c_user_id, c_user_type, c_role, c_partner_link, c_partner)
        SELECT $program_id, $user_id, $user_type, $program_user_role, $partner_link
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
      partnerLink: PartnerLink,
      user: User, // current user
    ): Option[AppliedFragment] =
      val up = LinkUser(targetProgram, targetUser, UserType.Standard, ProgramUserRole.Coi, partnerLink)
      user.role match
        case GuestRole                    => None
        case ServiceRole(_)               => Some(up)
        case StandardRole.Admin(_)        => Some(up)
        case StandardRole.Ngo(_, partner) => Some(up |+| void" WHERE " |+| existsAllocationForPartner(targetProgram, partner))
        case StandardRole.Pi(_)           => Some(up |+| void" WHERE " |+| existsUserAsPi(targetProgram, user.id))
        case StandardRole.Staff(_)        => Some(up)

    /**
     * Link an observer to a program.
     * - Guests cannot do this.
     * - Staff, Admin, and Service users can always do this.
     * - Standard user can only do this if they're the program's PI or Coi.
     */
    def linkCoiReadOnly(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      partnerLink: PartnerLink,
      user: User, // current user
    ): Option[AppliedFragment] =
      val up = LinkUser(targetProgram, targetUser, UserType.Standard, ProgramUserRole.CoiRO, partnerLink)
      user.role match
        case GuestRole                    => None
        case ServiceRole(_)               => Some(up)
        case StandardRole.Admin(_)        => Some(up)
        case StandardRole.Ngo(_, partner) => Some(up |+| void" WHERE " |+| existsAllocationForPartner(targetProgram, partner))
        case StandardRole.Staff(_)        => Some(up)
        case StandardRole.Pi(_)           =>
          Some(
            up |+| void" WHERE " |+| existsUserAsPi(targetProgram, user.id)  |+|
                   void" OR "    |+| existsUserAsCoi(targetProgram, user.id)
          )

    /**
     * Link staff support to a program.
     * - Staff, Admin, and Service users can always do this.
     * - Nobody else can do this.
     */
    def linkSupport(
      targetProgram: Program.Id,
      targetUser: User.Id, // user to link
      user: User, // current user
      tpe: ProgramUserRole.SupportPrimary.type | ProgramUserRole.SupportSecondary.type
    ): Option[AppliedFragment] =
      import lucuma.core.model.Access._
      val up = LinkUser(targetProgram, targetUser, UserType.Standard, tpe, PartnerLink.HasUnspecifiedPartner)
      user.role.access match
        case Admin | Staff | Service => Some(up) // ok
        case _                       => None // nobody else can do this

    def existsUserAs(
      programId: Program.Id,
      userId: User.Id,
      role: ProgramUserRole
    ): AppliedFragment =
      sql"""
        EXISTS (select c_role from t_program_user where c_program_id = $program_id and c_user_id = $user_id and c_role = $program_user_role)
      """.apply(programId, userId, role)

    def existsUserAsPi(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      existsUserAs(programId, userId, ProgramUserRole.Pi)

    def existsUserAsCoi(
      programId: Program.Id,
      userId: User.Id,
    ): AppliedFragment =
      existsUserAs(programId, userId, ProgramUserRole.Coi)

    def existsAllocationForPartner(
      programId: Program.Id,
      partner: Partner
    ): AppliedFragment =
      sql"""
        EXISTS (select c_duration from t_allocation where c_program_id = $program_id and c_ta_category=${lucuma.odb.util.Codecs.time_accounting_category} and c_duration > 'PT')
      """.apply(programId, partner.timeAccountingCategory)

    def existsUserAccess(
      user:      User,
      programId: Program.Id
    ): Option[AppliedFragment] =
      user.role match
        case GuestRole                    => existsUserAsPi(programId, user.id).some
        case StandardRole.Pi(_)           => (void"(" |+| existsUserAsPi(programId, user.id) |+| void" OR " |+| existsUserAsCoi(programId, user.id) |+| void")").some
        case StandardRole.Ngo(_, partner) => existsAllocationForPartner(programId, partner).some
        case ServiceRole(_) |
             StandardRole.Admin(_) |
             StandardRole.Staff(_)        => none

    def whereUserAccess(
      user:      User,
      programId: Program.Id
    ): AppliedFragment =
      existsUserAccess(user, programId).fold(AppliedFragment.empty) { af =>
        void"WHERE " |+| af
      }

    def andWhereUserAccess(
      user:      User,
      programId: Program.Id
    ): AppliedFragment =
      existsUserAccess(user, programId).fold(AppliedFragment.empty) { af =>
        void"AND " |+| af
      }

    private def correlatedExistsUserAs(
      userId:     User.Id,
      outerAlias: String,
      innerAlias: String,
      role:       ProgramUserRole
    ): AppliedFragment =
      sql"""
        EXISTS (SELECT 1 FROM t_program_user #$innerAlias where #$innerAlias.c_program_id = #$outerAlias.c_program_id and #$innerAlias.c_user_id = $user_id and #$innerAlias.c_role = $program_user_role)
      """.apply(userId, role)

    private def correlatedExistsAllocationForPartner(
      outerAlias: String,
      innerAlias: String
    ): AppliedFragment =
      sql"""
        EXISTS (SELECT 1 FROM t_allocation WHERE #$innerAlias.c_program_id = #$outerAlias.c_program_id and #$innerAlias.c_ta_category=#$outerAlias.c_partner and #$innerAlias.c_duration > 'PT')
      """(Void)

    private def correlatedExistsUserAccess(
      user:       User,
      outerAlias: String,
      innerAlias: String
    ): Option[AppliedFragment] =
      user.role match
        case GuestRole                    => correlatedExistsUserAs(user.id, outerAlias, innerAlias, ProgramUserRole.Pi).some
        case StandardRole.Pi(_)           => (void"(" |+| correlatedExistsUserAs(user.id, outerAlias, innerAlias, ProgramUserRole.Pi) |+| void" OR " |+| correlatedExistsUserAs(user.id, outerAlias, innerAlias, ProgramUserRole.Coi) |+| void")").some
        case StandardRole.Ngo(_, partner) => correlatedExistsAllocationForPartner(outerAlias, innerAlias).some
        case ServiceRole(_)        |
             StandardRole.Admin(_) |
             StandardRole.Staff(_)        => none

    def updateProgramUsers(
      user:  User,
      SET:   ProgramUserPropertiesInput,
      which: AppliedFragment
    ): Option[AppliedFragment] =

      val alias = "o"

      val upGivenName         = sql"c_fallback_given_name  = ${varchar.opt}"
      val upFamilyName        = sql"c_fallback_family_name = ${varchar.opt}"
      val upCreditName        = sql"c_fallback_credit_name = ${varchar.opt}"
      val upEmail             = sql"c_fallback_email       = ${varchar.opt}"

      val upEducationalStatus = sql"c_educational_status   = ${educational_status.opt}"
      val upThesis            = sql"c_thesis               = ${bool.opt}"
      val upGender            = sql"c_gender               = ${gender.opt}"

      val ups: Option[NonEmptyList[AppliedFragment]] = NonEmptyList.fromList(
        List(
          SET.fallbackProfile.flatMap(_.givenName).foldPresent(upGivenName),
          SET.fallbackProfile.flatMap(_.familyName).foldPresent(upFamilyName),
          SET.fallbackProfile.flatMap(_.creditName).foldPresent(upCreditName),
          SET.fallbackProfile.flatMap(_.email).foldPresent(upEmail),
          SET.educationalStatus.foldPresent(upEducationalStatus),
          SET.thesis.foldPresent(upThesis),
          SET.gender.foldPresent(upGender)
        ).flattenOption :::
        SET.partnerLink.toList.flatMap { pl => List(
          sql"c_partner_link = $partner_link_type"(pl.linkType),
          sql"c_partner      = ${partner.opt}"(pl.partnerOption)
        )}
      )

      ups.map: nel =>
        val up =
          sql"""
            UPDATE t_program_user AS #$alias
            SET """(Void) |+| nel.intercalate(void", ") |+| void" " |+|
          sql"WHERE (#$alias.c_program_id, #$alias.c_user_id) IN ("(Void) |+| which |+| void")"

        (correlatedExistsUserAccess(user, alias, "i").fold(up) { exists =>
          up |+| void" AND " |+| exists
        }) |+| sql" RETURNING #$alias.c_program_id, #$alias.c_user_id"(Void)

    end updateProgramUsers
  end Statements