// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.Monad
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import skunk.*

import Services.Syntax.*


/**
 * Resolves queries presented with a GID or reference to the corresponding GID,
 * if any.  This is shared logic between the DatasetService and the
 * ObservationService.
 */
class IdResolver[F[_]: Monad, G: Eq, R](
  name:  String,
  query: Query[R, G],
  label: R => String
)(using Services[F]) {

  /** Resolve the GID given the provided information. */
  def resolve(
    gid:  Option[G],
    ref:  Option[R]
  ): F[Result[G]] = {

    def noId: OdbError =
      OdbError.InvalidArgument(s"One of ${name}Id or ${name}Reference must be provided.".some)

    def notFound(ref: R): OdbError =
      OdbError.InvalidArgument(s"${name.capitalize} '${label(ref)}' was not found.".some)

    def lookup(ref: R): F[Result[G]] =
      session.option(query)(ref).map(_.fold(notFound(ref).asFailure)(_.success))

    def reconcile(gid: G, ref: R): F[Result[G]] =
      (ResultT(lookup(ref)).flatMap { found =>
        ResultT(
          OdbError
            .InvalidArgument(s"${name.capitalize} '${label(ref)}' (id $found) does not correspond to $name id $gid.".some)
            .asFailure
            .unlessA(found === gid)
            .as(gid)
            .pure[F]
        )
      }).value

    (gid, ref) match {
      case (None,    None   ) => noId.asFailureF
      case (Some(g), None   ) => g.success.pure[F]
      case (None,    Some(r)) => lookup(r)
      case (Some(g), Some(r)) => reconcile(g, r)
    }
  }

}
