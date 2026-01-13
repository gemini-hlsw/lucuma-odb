// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.itc.client.InstrumentMode
import lucuma.odb.sequence.data.ItcInput

class ShortCut_7265 extends ExecutionTestSupportForGmos:

  test("issue #7265: itc acquisition lookup uses acquisition filter"):
    val imaging: IO[InstrumentMode.GmosNorthImaging] = for
      p <- createProgram
      t <- createTargetAs(pi, p, "Science Target")
      o <- createObservationWithModeAs(pi, p, List(t), s"""
             gmosNorthLongSlit: {
               grating: R831_G5302
               filter: R_PRIME
               fpu: LONG_SLIT_0_50
               centralWavelength: {
                 nanometers: 500
               }
               explicitYBin: TWO
               acquisition: {
                 explicitFilter: Z_PRIME
               }
            }
           """)
      r <- withServices(pi): s =>
             s.transactionally:
               s.generatorParamsService
                .selectOne(p, o)
                .flatMap: e =>
                  e.leftMap(error => new RuntimeException(s"unexpected error: $error")).liftTo[IO]
      i <- r.itcInput.leftMap(error => new RuntimeException(s"unexpected error: $error")).liftTo[IO]
      s <- ItcInput.spectroscopy.getOption(i).toRight(new RuntimeException(s"expected spectroscopy input: $i")).liftTo[IO]
      g <- InstrumentMode.gmosNorthImaging.getOption(s.acquisition.mode).toRight(new RuntimeException(s"expected GMOS North Imaging: $s")).liftTo[IO]
    yield g

    assertIO(imaging.map(_.filter), GmosNorthFilter.ZPrime)