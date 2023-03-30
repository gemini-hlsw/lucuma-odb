// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package attachments

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.refined.*

class attachments extends OdbSuiteWithS3 {
  
  val pi = TestUsers.Standard.pi(1, 30)

  val validUsers = List(pi)

  test("Placeholder Test") {
    // TODO: Need to be able to evaluate the DB changes.
    val fileName: NonEmptyString = "file".refined
    for {
      pid    <- createProgramAs(pi, "xxx")
      fileKey = awsConfig.fileKey(pid, fileName)
      aid    <- uploadAttachment(pi, pid, fileName.value, "finder", "Hopeful")
      _      <- assertS3(fileKey, "Hopeful")
      _ <- getAttachment(pi, pid, aid).assertEquals("Hopeful")
      _      <- deleteAttachment(pi, pid, aid)
      _      <- assertS3NotThere(fileKey)
    } yield ()
  }
}
