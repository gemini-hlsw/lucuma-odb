// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptySet
import cats.effect.IO
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.syntax.timespan.*
import lucuma.odb.graphql.query.ExecutionTestSupportForGhost
import lucuma.odb.smartgcal.data.Ghost
import lucuma.odb.smartgcal.data.SmartGcalValue

class SmartGcalServiceSuite_Ghost extends ExecutionTestSupportForGhost:

  val key = Ghost.SearchKey(
    GhostResolutionMode.High,
    GhostBinning.OneByOne,
    GhostBinning.OneByOne
  )

  val row = Ghost.TableRow(
    key,
    SmartGcalValue(
      gcalConfig       = StepConfig.Gcal(
        StepConfig.Gcal.Lamp.fromArcs(NonEmptySet.one(GcalArc.ThArArc)),
        GcalFilter.None,
        GcalDiffuser.Visible,
        GcalShutter.Closed
      ),
      baselineType     = GcalBaselineType.Night,
      stepCount        = PosInt.unsafeFrom(1),
      instrumentConfig = Ghost.GhostUpdate(
        10.secondTimeSpan,
        PosInt.unsafeFrom(10),
        20.secondTimeSpan,
        PosInt.unsafeFrom(20),
        30.secondTimeSpan
      )
    )
  )

  private def insert: IO[Unit] =
    withServices(serviceUser): s =>
      Services.asSuperUser:
        s.smartGcalService
         .insertGhost(1, PosLong.unsafeFrom(1), row)

  test("select - found"):

    val select = withServices(serviceUser): s =>
      Services.asSuperUser:
        s.smartGcalService
         .selectGhost(key, SmartGcalType.Arc)

    val one = PosInt.unsafeFrom(1)

    val initial = GhostDynamicConfig(
      GhostDetector(
        1.secondTimeSpan,
        one,
        GhostBinning.OneByOne,
        GhostReadMode.Slow
      ).asRed,
      GhostDetector(
        1.secondTimeSpan,
        one,
        GhostBinning.OneByOne,
        GhostReadMode.Slow
      ).asBlue
    )

    val expected = GhostDynamicConfig(
      GhostDetector(
        10.secondTimeSpan,
        PosInt.unsafeFrom(10),
        GhostBinning.OneByOne,
        GhostReadMode.Slow
      ).asRed,
      GhostDetector(
        20.secondTimeSpan,
        PosInt.unsafeFrom(20),
        GhostBinning.OneByOne,
        GhostReadMode.Slow
      ).asBlue
    )

    assertIO(insert *> select.map(lst => lst.head._1.apply(initial)), expected)

  test("select - missing"):
    val missingKey = Ghost.SearchKey(
      GhostResolutionMode.High,
      GhostBinning.OneByOne,
      GhostBinning.OneByTwo
    )

    val select = withServices(serviceUser): s =>
      Services.asSuperUser:
        s.smartGcalService
         .selectGhost(missingKey, SmartGcalType.Arc)

    assertIO(select, Nil)


