// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*

// Regression guard for the program resource-limit triggers (V1182): concurrent
// inserts into one program must not deadlock. An earlier design maintained the
// count on t_program itself, which deadlocked under concurrency because child
// inserts hold a FOR KEY SHARE lock on the program row while the counter UPDATE
// wanted FOR NO KEY UPDATE. The count now lives in a dedicated table.
class concurrentInsertDeadlock extends OdbSuite:

  val pi = TestUsers.Standard.pi(nextId, nextId)
  lazy val validUsers = List(pi)

  test("concurrent observations into one program"):
    createProgramAs(pi).flatMap: pid =>
      List.range(0, 16).parTraverse_(_ => createObservationAs(pi, pid))

  test("concurrent groups into one program"):
    createProgramAs(pi).flatMap: pid =>
      List.range(0, 16).parTraverse_(_ => createGroupAs(pi, pid))

  test("concurrent targets into one program"):
    createProgramAs(pi).flatMap: pid =>
      List.range(0, 16).parTraverse_(_ => createTargetAs(pi, pid))
