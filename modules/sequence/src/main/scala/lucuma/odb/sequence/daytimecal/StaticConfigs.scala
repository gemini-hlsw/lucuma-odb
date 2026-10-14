// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.Igrins2SlitOffsetPreset
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.model.sequence.igrins2.Igrins2SVCImages
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.odb.sequence.flamingos2
import lucuma.odb.sequence.gmos.InitialConfigs

/**
 * Static configurations for daytime calibrations.  The usual defaults are
 * derived from the observing mode's parameters or the asterism, neither of
 * which a daytime calibration has, so each instrument gets a fixed constant.
 */
object StaticConfigs:

  val Flamingos2: Flamingos2StaticConfig =
    flamingos2.Static

  val GmosNorth: StaticConfig.GmosNorth =
    InitialConfigs.GmosNorthStatic

  val GmosSouth: StaticConfig.GmosSouth =
    InitialConfigs.GmosSouthStatic

  val Gnirs: GnirsStaticConfig =
    GnirsStaticConfig(GnirsWellDepth.Shallow)

  val Igrins2: Igrins2StaticConfig =
    Igrins2StaticConfig(Igrins2SVCImages.DontSave, Igrins2SlitOffsetPreset.NodAlongSlit)
