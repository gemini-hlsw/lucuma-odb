// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.effect.Resource
import fs2.text.utf8
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Attachment
import lucuma.core.model.User
import org.http4s.*
import org.http4s.client.Client
import org.http4s.client.JavaNetClientBuilder

class customMaskInUse
    extends OdbSuiteWithS3
    with query.ExecutionTestSupportForGmos
    with CustomMaskOps:

  private val client: Client[IO] = JavaNetClientBuilder[IO].create

  private def deleteAttachment(user: User, aid: Attachment.Id): Resource[IO, Response[IO]] =
    Resource.eval(authorizationHeader(user)).flatMap: auth =>
      server.flatMap: svr =>
        client.run(
          Request[IO](
            method  = Method.DELETE,
            uri     = svr.baseUri / "attachment" / aid.toString,
            headers = Headers(auth)
          )
        )

  private def expectDelete(user: User, aid: Attachment.Id, status: Status, body: String): IO[Unit] =
    deleteAttachment(user, aid)
      .use(r => r.body.through(utf8.decode).compile.string.map((r.status, _)))
      .assertEquals((status, body))

  test("a MOS mask referenced by a step cannot be deleted"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      a <- insertMosMaskAttachment(p, "GN2025AQ001-02_ODF.fits", Instrument.GmosNorth)
      m  = s"""{ attachmentId: "$a", slitWidth: $SlitWidth }"""
      i  = input(o, SequenceType.Science, atomInput("Masked", gmosStep(m)))
      _ <- query(pi, mutation(Instrument.GmosNorth, i))
      _ <- expectDelete(pi, a, Status.Conflict, "The attachment is in use and cannot be deleted.")
    yield ()
