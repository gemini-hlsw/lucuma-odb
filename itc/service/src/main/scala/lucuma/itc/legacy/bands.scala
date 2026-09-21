// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import lucuma.core.enums.Band
import lucuma.core.model.SourceProfile

/**
 * The legacy OCS ITC (the Scala 2.11 jars in `itc/service/ocslib`) decodes the `normBand` field
 * into `edu.gemini.spModel.core.MagnitudeBand`, whose members are
 * `_u _g _r _i _z U B V UC R I Y J H K L M N Q AP`. It has no Gaia bands, so a `normBand` of "G",
 * "G_BP" or "G_RP" fails to decode and surfaces as an opaque calculation error.
 *
 * Brightnesses in bands the legacy ITC cannot name are therefore removed from the source profile
 * before `nearestBand` gets a chance to select one. This is an allowlist rather than a Gaia
 * denylist because the authoritative set is what `MagnitudeBand` contains, which is frozen until
 * someone runs `update_itc_jars.sh`; a band newly added to lucuma-core is then filtered rather than
 * emitted as an undecodable `shortName`.
 */
object LegacyBands:

  val Supported: Set[Band] =
    Set(
      Band.SloanU,
      Band.SloanG,
      Band.SloanR,
      Band.SloanI,
      Band.SloanZ,
      Band.U,
      Band.B,
      Band.V,
      Band.R,
      Band.I,
      Band.Y,
      Band.J,
      Band.H,
      Band.K,
      Band.L,
      Band.M,
      Band.N,
      Band.Q,
      Band.Ap
    )

  extension (self: SourceProfile)
    /** Removes brightnesses in bands the legacy ITC cannot represent. */
    def legacyBandsOnly: SourceProfile =
      SourceProfile.integratedBrightnesses
        .modifyOption(_.filter((b, _) => Supported(b)))(self)
        .orElse:
          SourceProfile.surfaceBrightnesses
            .modifyOption(_.filter((b, _) => Supported(b)))(self)
        .getOrElse(self)
