// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import lucuma.core.util.Enumerated

enum FpuOption(val tag: String, val label: String) derives Enumerated:
  case Ifu        extends FpuOption("ifu", "ifu")
  case Multislit  extends FpuOption("multiple_slit", "multislit")
  case Singleslit extends FpuOption("single_slit", "singleslit")
