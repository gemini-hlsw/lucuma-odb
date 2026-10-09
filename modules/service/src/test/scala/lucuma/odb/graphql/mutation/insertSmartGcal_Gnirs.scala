// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig

class insertSmartGcal_Gnirs extends query.ExecutionTestSupportForGnirs with InsertSmartGcalOps:

  // The remaining (unexecuted or executing) science sequence.
  def science(oid: Observation.Id): IO[List[Step[GnirsDynamicConfig]]] =
    generateOrFailAs(serviceUser, oid).flatMap:
      case InstrumentExecutionConfig.Gnirs(e) =>
        e.science.toList.flatMap(s => (s.nextAtom :: s.possibleFuture).flatMap(_.steps.toList)).pure[IO]
      case i                                  =>
        IO.raiseError(new RuntimeException(s"Expected GNIRS, but got ${i.instrument.map(_.longName)}"))

  // The generated sequence ends its science with a flat and an arc taken at the
  // last science offset, so inserting a flat and an arc based on the last
  // science step should reproduce them.  Their exposure times call for the
  // Bright read mode, and the smart GCAL table for coadds of their own, both
  // differing from the 30s, single coadd science step.
  test("inserted calibrations match the generated ones, with their own read mode and coadds"):
    for
      p    <- createProgram
      t    <- createTargetWithProfileAs(pi, p)
      o    <- createGnirsLongSlitObservationAs(pi, p, t)
      seq0 <- science(o)
      sci   = seq0.filter(_.stepConfig === StepConfig.Science).last
      _    <- insertAs(pi, o, List(SmartGcalType.Flat, SmartGcalType.Arc), sci.id.some, reference = sci.id.some)
      seq1 <- science(o)
    yield
      val generated = seq0.filter(s => StepConfig.gcal.getOption(s.stepConfig).isDefined)
      val old       = seq0.map(_.id).toSet
      val inserted  = seq1.filterNot(s => old.contains(s.id))
      assertEquals(sci.instrumentConfig.readMode, GnirsReadMode.Faint)
      assertEquals(generated.map(_.instrumentConfig.readMode), List(GnirsReadMode.Bright, GnirsReadMode.Bright))
      assertEquals(generated.map(_.instrumentConfig.coadds.value), List(2, 3))
      assertEquals(inserted.map(_.stepConfig), generated.map(_.stepConfig))
      assertEquals(inserted.map(_.instrumentConfig), generated.map(_.instrumentConfig))
      assertEquals(inserted.map(_.telescopeConfig), generated.map(_.telescopeConfig))
      assertEquals(inserted.map(_.observeClass), generated.map(_.observeClass))
