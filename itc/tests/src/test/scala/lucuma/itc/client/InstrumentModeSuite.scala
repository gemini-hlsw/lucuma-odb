// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.kernel.laws.discipline.*
import lucuma.itc.client.arb.ArbInstrumentMode
import monocle.law.discipline.*
import munit.*

class InstrumentModeSuite extends DisciplineSuite:

  import ArbInstrumentMode.given

  checkAll("InstrumentMode", EqTests[InstrumentMode].eqv)
  checkAll("InstrumentMode.gmosNorthSpectroscopy", PrismTests(InstrumentMode.gmosNorthSpectroscopy))
  checkAll("InstrumentMode.gmosSouthSpectroscopy", PrismTests(InstrumentMode.gmosSouthSpectroscopy))
  checkAll("InstrumentMode.gmosNorthImaging", PrismTests(InstrumentMode.gmosNorthImaging))
  checkAll("InstrumentMode.gmosSouthImaging", PrismTests(InstrumentMode.gmosSouthImaging))
