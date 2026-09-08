// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.enums.AttachmentType
import lucuma.core.util.Enumerated
import lucuma.odb.service.AttachmentFileService.presignHeaders
import munit.FunSuite
import org.http4s.MediaType

class AttachmentPresignHeadersSuite extends FunSuite:

  test("pdf attachments presign inline, everything else downloads"):
    Enumerated[AttachmentType].all.foreach: at =>
      val isPdf = at.fileExtensions.map(_.value) === Set("pdf")
      assertEquals(
        at.presignHeaders,
        Option.when(isPdf)(S3FileService.ResponseHeaders.InlinePdf),
        s"$at"
      )

  test("the inline headers are the S3 read-time overrides"):
    assertEquals(S3FileService.ResponseHeaders.InlinePdf.contentType, MediaType.application.pdf)
    assertEquals(S3FileService.ResponseHeaders.InlinePdf.contentDisposition, "inline")
