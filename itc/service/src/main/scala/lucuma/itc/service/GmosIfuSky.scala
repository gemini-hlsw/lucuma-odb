// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import eu.timepit.refined.types.numeric.PosInt

/**
 * How many GMOS IFU lenslets sample blank sky, which is to say the background. "Sky" is the OCS
 * term: `IfuMethod.skyFibres` is documented there as the "# fibres (area) assumed to be on sky",
 * and it sets the noise the recipe charges for subtracting that background, a factor of
 * `1 + 1/fibres` on the variance (`SpecS2NSlitVisitor`). Counts are the OCS OT defaults
 * (`AnalysisMethodPanel.defaultMethod`).
 *
 * The IFU has two separate lenslet fields, 60" apart: 7"x5" on the target and a dedicated 3.5"x5"
 * on blank sky, half the area and so half the lenslets (`GmosCommonType.IFU_FOV`). Both fields feed
 * the same two pseudo-slits at the spectrograph, so a pseudo-slit carries target and sky lenslets
 * alike — masking one does not choose between them, it halves the whole instrument.
 */
enum GmosIfuSky(val fibres: PosInt):
  /** The unmasked IFU, using both pseudo-slits: the whole 500-lenslet sky field. */
  case TwoSlit extends GmosIfuSky(PosInt.unsafeFrom(500))

  /** Masked to one pseudo-slit, which halves target and sky fields together. */
  case OneSlit extends GmosIfuSky(PosInt.unsafeFrom(250))

  /**
   * Nod & shuffle, which does not use the dedicated sky field: the target lenslets themselves
   * alternate between target and sky as the telescope nods, for equal time on each. That yields one
   * sky sample per target sample rather than a field to average down.
   */
  case NodAndShuffle extends GmosIfuSky(PosInt.unsafeFrom(1))
