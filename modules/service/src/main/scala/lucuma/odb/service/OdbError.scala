// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.kernel.Eq
import cats.syntax.all.*
import clue.model.GraphQLError
import grackle.Problem
import grackle.Result
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.sso.client.codec.user.*

import scala.collection.SortedMap
import scala.language.implicitConversions

/** 
 * A semi-structured error (or warning) produced by the ODB. A client application might
 * wish to report the error category and detail (if any) and in some cases may wish to
 * examine the `data` map for additional details.
 */
case class OdbError(code: OdbError.Category, user: User, detail: Option[String] = None, data: SortedMap[String, Json] = SortedMap.empty)

object OdbError:

  /** Errors are grouped into categories that might correspond with different kinds of user dialogs on the client side. */
  enum Category(val tag: String, val text: String):
    case InvalidArgument     extends Category("invalid_argument", "The provided argument is not valid.")
    case NoAction            extends Category("no_action", "No action was taken.")
    case NotAuthorized       extends Category("not_authorized", "User is not authorized to perform this operation.")
    case InvitationError     extends Category("invitation_error", "Invitation operation could not be completed.")
    case InvalidProgram      extends Category("invalid_program", "Specified program does not exist, is not visible, or is ineligible for the requested operation.")
    case InvalidObservation  extends Category("invalid_observation", "Specified observation does not exist, is not visible, or is ineligible for the requested operation.")
    case SequenceUnavailable extends Category("sequence_unavailable", "Could not generate the requested sequence.")
    case InvalidTarget       extends Category("invalid_target", "Specified target does not exist, is not visible, or is ineligible for the requested operation.")
    case InvalidTargetList   extends Category("invalid_target_list", "The provided target list is not valid for the requested operation.")
    case InvalidVisit        extends Category("invalid_visit", "The specified visit does not exist, is not visible, or is ineligible for the requested operation.")
    case InvalidStep         extends Category("invalid_step", "The specified step does not exist, is not visible, or is ineligible for the requested operation.")
    case InvalidFilename     extends Category("invalid_filename", "The specified filename is invalid or already exists.")
    case InvalidAtom         extends Category("invalid_atom", "The specified atom does not exist, is not visible, or is ineligible for the requested operation.")
    case InvalidDataset      extends Category("invalid_dataset", "The specified dataset does not exist, is not visible, or is ineligible for the requested operation.")
    case InvalidUser         extends Category("invalid_user", "The specified user does not exist, or is ineligible for the requested operation.")
    case UpdateFailed        extends Category("update_failed", "The specified operation could not be completed.")
    case ItcError            extends Category("itc_error", "The requested ITC operation could not be completed.")

    def asOdbError(user: User): OdbError =
      OdbError(this, user, None, SortedMap.empty)

  given Enumerated[Category] = Enumerated.derived
  given Eq[OdbError] = Eq.by(e => (e.code, e.user, e.detail, e.data.toList)) // :-\

  // "private" fields we pack into the extensions for transport
  private val FieldPrefix = "odb_error."
  object Field:
    val Category = s"${FieldPrefix}category"
    val Detail   = s"${FieldPrefix}detail"
    val User     = s"${FieldPrefix}user"

  /** Get a named extension. */
  extension (e: GraphQLError) private def ext[A: Decoder](key: String): Option[A] =
    e.extensions.flatMap(_.get(key).flatMap(_.as[A].toOption))

  /** If `e` contains the expected extensions we can turn it into a OdbError. */
  def fromGraphQLError(e: GraphQLError): Option[OdbError] =
    for 
      code   <- e.ext[Category](Field.Category)
      detail <- e.ext[Option[String]](Field.Detail)
      user   <- e.ext[User](Field.User)
    yield OdbError(
      code, 
      user, 
      detail, 
      e.extensions
        .map(_.filterNot(_._1.startsWith(FieldPrefix)).to(SortedMap))
        .getOrElse(SortedMap.empty)
    )



// this is local to the ODB

extension (e: OdbError)

  def withDetail(detail: String): OdbError = e.copy(detail = Some(detail))
  def withData(data: (String, Json)*): OdbError = e.copy(data = e.data ++ data)

  def asProblem: Problem =
    import OdbError.Field
    Problem(e.detail.getOrElse(e.code.text), Nil, Nil, Some(JsonObject.fromFoldable(
      (Field.Category -> e.code.asJson)   :: 
      (Field.Detail   -> e.detail.asJson) ::
      (Field.User     -> e.user.asJson)   ::
      e.data.toList
    )))

  def asFailure: Result[Nothing] =
    Result.failure(asProblem)

  def asFailureF[F[_]: Applicative, A]: F[Result[A]] =
    asFailure.pure[F]

  def asWarning[A](a: A): Result[A] =
    Result.warning(asProblem, a)

  def asWarningF[F[_]: Applicative, A](a: A): F[Result[A]] =
    asWarning(a).pure[F]


