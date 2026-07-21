// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.graphql

import _root_.skunk.Channel
import _root_.skunk.Session
import _root_.skunk.implicits.*
import cats.ApplicativeThrow
import cats.effect.{Unique as _, *}
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosLong
import fs2.Stream
import grackle.*
import grackle.Predicate.*
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.QueryCompiler.SelectElaborator
import grackle.Value.AbsentValue
import grackle.Value.EnumValue
import grackle.Value.NullValue
import grackle.Value.StringValue
import grackle.skunk.SkunkMapping
import grackle.skunk.SkunkMonitor
import lucuma.core.enums.Partner
import lucuma.core.model
import lucuma.core.model.Access
import lucuma.core.model.OrcidId
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.odb.graphql.schema.SchemaStitcher
import lucuma.sso.service.database.Database
import lucuma.sso.service.database.RoleRequest
import lucuma.sso.service.database.RoleType
import natchez.Trace
import org.typelevel.log4cats.Logger
import lucuma.odb.graphql.binding.UserIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.sso.service.graphql.input.WhereUser
import lucuma.sso.service.graphql.predicate.Predicates

object SsoMapping {

  case class Channels[F[_]](
    apiKeyDeletions: Channel[F, String, String]
  )

  object Channels {
    def apply[F[_]](pool: Resource[F, Session[F]]): Resource[F, Channels[F]] =
      pool.map { s =>
        Channels(
          apiKeyDeletions = s.channel(id"lucuma_api_key_deleted")
        )
      }
  }

  def loadSchema[F[_]: ApplicativeThrow: Logger]: F[Schema] =
    SchemaStitcher.load("Sso.graphql")

  def apply[F[_]: Async: Trace](
    channels: Channels[F],
    pool:     Resource[F, Session[F]],
    monitor:  SkunkMonitor[F],
    loadedSchema: Schema
  ): StandardUser => Mapping[F] =
    (user: StandardUser) => {
      // Directly-computed result for `createApiKey` mutation.
      def createApiKey(env: Env): F[Result[String]] =
        env
          .getR[StandardRole.Id]("roleId")
          .flatTraverse: roleId =>
            if (user.role :: user.otherRoles).exists(_.id === roleId) then
              pool.map(Database.fromSession(_)).use: db =>
                db.createApiKey(roleId)
                  .map(apiKey => Result(lucuma.sso.client.ApiKey.fromString.reverseGet(apiKey)))
            else
              Result.failure(show"No such role: $roleId").pure[F]

      // Add a role
      def addRole(env: Env): F[Result[StandardRole.Id]] =
        if user.role.access < Access.Admin then
          Result.failure(s"User ${user.id} is not authorized to perform this action.").pure[F]
        else
          (
            env.getR[User.Id]("userId"), 
            env.getR[RoleRequest]("roleRequest"),
          ) .parTupled
            .flatTraverse:
              case (uid, roleRequest) =>
                pool.map(Database.fromSession(_)).use: db =>
                  db.canonicalizeRole(uid, roleRequest)
                    .map(Result.success)

      def deleteRole(env: Env): F[Result[Boolean]] =
        if user.role.access < Access.Admin then
          Result.failure(s"User ${user.id} is not authorized to perform this action.").pure[F]
        else
          (
            env.getR[StandardRole.Id]("roleId"), 
          ) .flatTraverse:
              case (roleId) =>
                pool.map(Database.fromSession(_)).use: db =>
                  db.deleteRole(roleId).as(Result.success(true))

      def deleteApiKey(env: Env): F[Result[Boolean]] =
        env
          .getR[PosLong]("id")
          .flatTraverse: id =>
            pool.map(Database.fromSession(_)).use: db =>
              db.deleteApiKey(id, Some(user.id)).map(Result.success)

      val apiKeyRevocation: Stream[F, Result[String]] =
        channels
          .apiKeyDeletions
          .listen(1024)
          .evalTap(n => Async[F].delay(println(n)))
          .map(a => Result.success(a.value))

      new SkunkMapping[F](pool, monitor) with SsoTables[F] with Predicates[F] with BaseMapping[F] with ResultMapping[F] {

        val schema: Schema = loadedSchema

        val typeMappings: TypeMappings =
          TypeMappings(
            List[TypeMapping](
              ObjectMapping(
                tpe = QueryType,
                fieldMappings = List(
                  SqlObject("user"),
                  SqlObject("role"),
                  SqlObject("users"),
                )
              ),
              ObjectMapping(
                tpe = MutationType,
                fieldMappings = List(
                  RootEffect.computeEncodable("createApiKey")((_, e) => createApiKey(e)),
                  RootEffect.computeEncodable("deleteApiKey")((_, e) => deleteApiKey(e)),
                  RootEffect.computeEncodable("addRole")((_, e) => addRole(e)),
                  RootEffect.computeEncodable("deleteRole")((_, e) => deleteRole(e)),
                )
              ),
              ObjectMapping(
                tpe = UserProfileType,
                fieldMappings = List(
                  SqlField("synthetic-id", User.Id, key = true, hidden = true),
                  SqlField("givenName",  User.Profile.GivenName),
                  SqlField("familyName", User.Profile.FamilyName),
                  SqlField("creditName", User.Profile.CreditName),
                  SqlField("email",      User.Profile.Email)
                )
              ),
              ObjectMapping(
                tpe = UserType,
                fieldMappings = List(
                  SqlField("id", User.Id, key = true),
                  SqlField("orcidId",  User.OrcidId),
                  SqlField("type", User.Type),
                  SqlField("enabled", User.Enabled),
                  SqlObject("profile"),
                  SqlObject("roles", Join(User.Id, Role.UserId)),
                  SqlObject("apiKeys", Join(User.Id, ApiKey.UserId)),
                )
              ),
              ObjectMapping(
                tpe = RoleType,
                fieldMappings = List(
                  SqlField("id", Role.Id, key = true),
                  SqlField("type", Role.Type),
                  SqlField("partner", Role.Partner),
                  SqlField("«unused»", Role.UserId, hidden = true),
                  SqlObject("user", Join(Role.UserId, User.Id)),
                )
              ),
              ObjectMapping(
                tpe = ApiKeyType,
                fieldMappings = List(
                  SqlField("id", ApiKey.Id, key = true),
                  SqlField("«unused»", ApiKey.UserId, hidden = true),
                  SqlField("«unused»", ApiKey.RoleId, hidden = true),
                  SqlObject("user", Join(ApiKey.UserId, User.Id)),
                  SqlObject("role", Join(ApiKey.RoleId, Role.Id)),
                )
              ),
              ObjectMapping(
                tpe = SubscriptionType,
                fieldMappings = List(
                  RootStream.computeEncodable("apiKeyRevocation")((_,_) => apiKeyRevocation)
                )
              ),
              LeafMapping[model.User.Id](UserIdType),
              LeafMapping[OrcidId](OrcidIdType),
              LeafMapping[StandardRole.Id](RoleIdType),
              LeafMapping[RoleType](RoleTypeType),
              LeafMapping[Partner](PartnerType),
              LeafMapping[String](ApiKeyIdType),
              LeafMapping[lucuma.odb.data.UserType](UserTypeType),
              topLevelSelectResultMapping(UserSelectResultType),
            )
          )

        val WhereUserBinding = WhereUser.binding(Path.from(UserType))

        override val selectElaborator = SelectElaborator {

          case (QueryType, "users", List(
            WhereUserBinding.Option("WHERE", rWHERE),
            UserIdBinding.Option("OFFSET", rOFFSET),
            NonNegIntBinding.Option("LIMIT", rLIMIT),
            BooleanBinding("includeDisabled", rIncludeDisabled)
          )) =>
            Elab.transformChild { child =>
              (rWHERE, rOFFSET, rLIMIT, rIncludeDisabled).parTupled.flatMap { (WHERE, OFFSET, LIMIT, includeDisabled) =>
                val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
                ResultMapping.selectResult(child, limit) { q =>
                  FilterOrderByOffsetLimit(
                    pred = Some(
                      and(List(
                        OFFSET.map(Predicates.user.id.gtEql).getOrElse(True),
                        Predicates.user.enabled.includeDisabled(includeDisabled),
                        WHERE.getOrElse(True)
                      ))
                    ),
                    oss = Some(List(
                      OrderSelection[lucuma.core.model.User.Id](UserType / "id"),
                    )),
                    offset = None,
                    limit = Some(limit + 1), // Select one extra row here.
                    child = q
                  )
                }
              }
            }

          case (QueryType, "user", Nil) =>
            Elab.transformChild(c => Unique(Filter(Eql(UserType / "id", Const(user.id)), c)))

          case (QueryType, "role", Nil) =>
            Elab.transformChild(c => Unique(Filter(Eql(UserType / "id", Const(user.role.id)), c)))

          case (MutationType, "createApiKey", List(Binding("role", Value.StringValue(id)))) =>
            val rRoleId = Result.fromOption(StandardRole.Id.parse(id), s"Not a valid role id: $id")
            Elab.liftR(rRoleId).flatMap { roleId =>  Elab.env("roleId" -> roleId) }

          case (MutationType, "deleteApiKey", List(Binding("id", Value.StringValue(hexString)))) =>
            val rKeyId = Result.fromOption(lucuma.sso.client.ApiKey.Id.fromString.getOption(hexString), s"Not a valid API key id: $hexString")
            Elab.liftR(rKeyId).flatMap { keyId => Elab.env("id" -> keyId)}

          case (MutationType, "deleteRole", List(Binding("roleId", Value.StringValue(roleString)))) =>
            val rRoleId = Result.fromOption(StandardRole.Id.parse(roleString), s"Not a valid API key id: $roleString")
            Elab.liftR(rRoleId).flatMap { roleId => Elab.env("roleId" -> roleId)}

          case (MutationType, "addRole", List(
            Binding("userId", Value.StringValue(id)), 
            Binding("roleType", Value.EnumValue(roleType)), 
            Binding("partner", pValue))
          ) =>
            import lucuma.sso.service.database.{ RoleType => RT }
            Elab
              .liftR:
                if user.role.access < Access.Admin then
                  Result.failure(s"User ${user.id} is not authorized to perform this action.")
                else (
                    Result.fromOption(lucuma.core.model.User.Id.parse(id), s"Not a valid user id: $id"),
                    Result.fromOption(RT.parse(roleType), s"Not a valid role type: $roleType"),
                    pValue match
                      case EnumValue(name) => Result.fromOption(Partner.values.find(_.tag.equalsIgnoreCase(name)), s"Not a valid partner: $name").map(_.some)
                      case AbsentValue | NullValue => Result.success(None)
                      case _ => Result.internalError("Unpossible; validation should disallow these cases.")                
                  ) .parTupled
                    .flatMap:
                      case (uid, RT.Pi, None)                        => Result.success((uid, RoleRequest.Pi))
                      case (uid, RT.Staff, None)                     => Result.success((uid, RoleRequest.Staff))
                      case (uid, RT.Admin, None)                     => Result.success((uid, RoleRequest.Admin))
                      case (uid, RT.Ngo, Some(p))                    => Result.success((uid, RoleRequest.Ngo(p)))
                      case (_, RT.Pi | RT.Staff | RT.Admin, Some(_)) => Result.failure("Only NGO roles may specify a partner.")
                      case (_, RT.Ngo, None)                         => Result.failure("NGO roles must specify a partner.")
              .flatMap:
                case (uid, rr) =>
                  Elab.env("userId" -> uid, "roleRequest" -> rr)

        }

      }

    }
}
