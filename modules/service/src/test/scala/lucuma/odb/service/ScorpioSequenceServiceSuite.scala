// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import lucuma.core.enums.ScorpioChannelSelection
import lucuma.core.enums.ScorpioDisperser
import lucuma.core.enums.ScorpioFpu
import lucuma.core.enums.ScorpioVisReadoutMode
import lucuma.core.model.sequence.scorpio.ScorpioChannelConfig
import lucuma.core.model.sequence.scorpio.ScorpioDynamicConfig
import lucuma.core.model.sequence.scorpio.ScorpioFpuMask
import lucuma.core.model.sequence.scorpio.ScorpioStaticConfig
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import munit.FunSuite

class ScorpioSequenceServiceSuite extends FunSuite:

  import ScorpioSequenceService.checkDynamic
  import ScorpioSequenceService.checkStatic

  private val Time: TimeSpan =
    TimeSpan.unsafeFromMicroseconds(10_000_000L)

  private val Count: PosInt =
    PosInt.unsafeFrom(1)

  private val Vis: Option[ScorpioChannelConfig.Vis] =
    ScorpioChannelConfig.Vis(Time, Count, ScorpioVisReadoutMode.Default).some

  private val Dynamic: ScorpioDynamicConfig =
    ScorpioDynamicConfig(
      Vis, Vis, none, none,
      none, none, none, none,
      ScorpioFpuMask.LongSlit(Enumerated[ScorpioFpu].all.head),
      ScorpioDisperser.Vph
    )

  private val Static: ScorpioStaticConfig =
    ScorpioStaticConfig(ScorpioChannelSelection.VisOnly)

  private def assertRejected(r: Result[Unit], message: String): Unit =
    assertEquals(r.toProblems.toList.map(_.message), List(message))

  test("a long slit step with only visible channels is accepted"):
    assert(checkDynamic(Dynamic).hasValue)

  test("a step that is not long slit is rejected"):
    List(ScorpioFpuMask.Imaging, ScorpioFpuMask.ColdBlock).foreach: fpu =>
      assertRejected(checkDynamic(Dynamic.copy(fpu = fpu)), "SCORPIO steps that are not long slit are not supported yet.")

  test("a step with an IR channel is rejected"):
    val ir = ScorpioChannelConfig.Ir(Time, Count).some
    List[ScorpioDynamicConfig => ScorpioDynamicConfig](
      _.copy(y = ir), _.copy(j = ir), _.copy(h = ir), _.copy(ks = ir)
    ).foreach: f =>
      assertRejected(checkDynamic(f(Dynamic)), "SCORPIO IR channels are not supported yet.")

  test("the default visible only static config is accepted"):
    assert(checkStatic(Static).hasValue)

  test("a static config that selects IR channels is rejected"):
    Enumerated[ScorpioChannelSelection].all.filterNot(_ == ScorpioChannelSelection.VisOnly).foreach: sel =>
      assertRejected(checkStatic(Static.copy(channelSelection = sel)), "SCORPIO IR channels are not supported yet.")

  test("a static config with settings that are not stored is rejected"):
    assertRejected(checkStatic(Static.copy(svcExposureTime = Time.some)), "SCORPIO ADC, SVC and nod and shift settings are not supported yet.")
