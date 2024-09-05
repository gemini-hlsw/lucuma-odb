// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.effect.IO
import cats.syntax.eq.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.sequence.Atom
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.query.ExecutionTestSupport

class ShortCut_3219 extends ExecutionTestSupport {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      30.minTimeSpan,
      PosInt.unsafeFrom(40),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  test("no duplicated atom ids") {
    val aids: IO[List[Atom.Id]] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(serviceUser, p, List(t))
        aids <- genGmosNorthSequence(o, SequenceType.Science, 20)
      } yield aids

    assertIOBoolean(aids.map(lst => lst.size === lst.distinct.size))
  }
}