// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

class insertSmartGcal_Flamingos2 extends query.ExecutionTestSupportForFlamingos2 with InsertSmartGcalOps with ReplaceSequenceOps:

  // Long enough that the science read mode differs from that of the (much
  // shorter) smart GCAL lamp exposures.
  val ExposureTime: TimeSpan = 5.minTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(4))

  test("a calibration's read mode follows its own exposure time, not the science step's"):
    for
      p    <- createProgram
      t    <- createTargetWithProfileAs(pi, p)
      o    <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      seq0 <- generateFlamingos2OrFail(serviceUser, o).map(_.executionConfig.science.toList.flatMap(s => (s.nextAtom :: s.possibleFuture).flatMap(_.steps.toList)))
      sci   = seq0.find(_.stepConfig === StepConfig.Science).get
      _    <- insertAs(pi, o, List(SmartGcalType.Flat, SmartGcalType.Arc), sci.id.some, reference = sci.id.some)
      seq1 <- generateFlamingos2OrFail(serviceUser, o).map(_.executionConfig.science.toList.flatMap(s => (s.nextAtom :: s.possibleFuture).flatMap(_.steps.toList)))
    yield
      val old = seq0.map(_.id).toSet
      val science = flamingos2Science(ExposureTime)
      assertEquals(sci.instrumentConfig, science)
      assertNotEquals(science.readMode, Flamingos2Flat.readMode)
      assertNotEquals(science.readMode, Flamingos2Arc.readMode)
      assertEquals(seq1.filterNot(s => old.contains(s.id)).map(_.instrumentConfig), List(Flamingos2Flat, Flamingos2Arc))

  // Only a hand-written sequence can carry this slit width, since the MOS mode
  // rejects it.  Searching with the custom mask itself would match the smart
  // GCAL entries for configurations without an FPU.
  test("a custom mask whose slit width has no builtin FPU cannot be calibrated"):
    val step =
      s"""
        {
          instrumentConfig: {
            exposure: { seconds: 20 }
            disperser: ${Flamingos2Disperser.R1200JH.tag.toScreamingSnakeCase}
            filter: ${Flamingos2Filter.JH.tag.toScreamingSnakeCase}
            readMode: BRIGHT
            lyotWheel: F16
            fpu: { customMask: { slitWidth: ${Flamingos2CustomSlitWidth.Other.tag.toScreamingSnakeCase} } }
            decker: ${Flamingos2Decker.MOS.tag.toScreamingSnakeCase}
            readoutMode: SCIENCE
            reads: READS_1
          }
          stepConfig: { science: true }
          observeClass: SCIENCE
        }
      """

    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      _ <- query(pi, mutation(Instrument.Flamingos2, input(o, SequenceType.Science, atomInput("Custom", step))))
      _ <- insertFails(
             pi,
             o,
             List(SmartGcalType.Flat),
             none,
             List("Cannot insert a smart GCAL flat step: Flamingos 2 has no builtin FPU equivalent to the 'Other' custom slit width")
           )
    yield ()
