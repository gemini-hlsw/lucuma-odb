// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.sequence.Atom
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class ShortCut_3219 extends ExecutionTestSupportForGmos {

  import lucuma.odb.testsyntax.execution.*

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      30.minTimeSpan,
      PosInt.unsafeFrom(40)
    )

  test("no duplicated atom ids") {
    val aids: IO[List[Atom.Id]] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(serviceUser, p, List(t))
        gn <- generateOrFail(p, o, 20.some).map(_.gmosNorthScience)
      } yield gn.nextAtom.id :: gn.possibleFuture.map(_.id)

    assertIOBoolean(aids.map(lst => lst.size === lst.distinct.size))
  }
}
