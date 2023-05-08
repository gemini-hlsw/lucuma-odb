// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Async
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import fs2.io.file.Path
import lucuma.core.model.GuestUser
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.NewType
import natchez.Trace
import skunk._

trait AttachmentFileService {

  import AttachmentFileService.AttachmentException
  import AttachmentException.*

  protected type FileName = FileName.Type
  protected object FileName extends NewType[NonEmptyString] {
    def fromString(name: String): Either[AttachmentException, FileName] = {
      val path         = Path(name)
      val segmentCount = path.names.length
      val fileName     = NonEmptyString.from(path.fileName.toString).toOption

      fileName.fold(
        InvalidRequest("File name is required").asLeft
      )(fn =>
        if (path.names.length > 1) {
          InvalidRequest("File name cannot include a path").asLeft
        } else FileName(fn).asRight
      )
    }

    extension (fileName: FileName)
      def extName: Option[NonEmptyString] =
        NonEmptyString.from(Path(fileName.value.value).extName).toOption
  }

  // TODO: eventually will probably want to check for write access for uploading/deleting files.
  def checkAccess[F[_]: Async: Trace](
    session: Session[F],
    user: User,
    programId: Program.Id
  ): F[Unit] = user match {
    // guest users not allowed to upload files - at least for now.
    case GuestUser(_) => Async[F].raiseError(Forbidden)
    case _            =>
      ProgramService
        .fromSessionAndUser(session, user)
        .userHasAccess(programId)
        .flatMap(hasAccess =>
          if (hasAccess) Async[F].unit
          else Async[F].raiseError(Forbidden)
        )
  }
}

object AttachmentFileService {

  sealed trait AttachmentException extends Exception
  
  object AttachmentException {
    case object Forbidden                   extends AttachmentException
    case class InvalidRequest(message: String) extends AttachmentException
    case object FileNotFound                extends AttachmentException
  }
}
