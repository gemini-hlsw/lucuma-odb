// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.syntax.string.*

// Probes AGS has geometry for per observing mode
// TODO:. Move to lucuma-core
object GuideProbeRules:

  private val Pwfs: Set[GuideProbe] =
    Set(GuideProbe.PWFS1, GuideProbe.PWFS2)

  def allowedProbes(mode: ObservingModeType): Set[GuideProbe] =
    mode match
      case _: ExchangeObservingModeType =>
        Set.empty
      case ObservingModeType.GmosNorthLongSlit | ObservingModeType.GmosSouthLongSlit |
           ObservingModeType.GmosNorthImaging  | ObservingModeType.GmosSouthImaging  |
           ObservingModeType.GmosNorthMos      | ObservingModeType.GmosSouthMos      |
           ObservingModeType.GmosNorthIfu      | ObservingModeType.GmosSouthIfu      =>
        Pwfs + GuideProbe.GmosOIWFS
      case ObservingModeType.Flamingos2LongSlit | ObservingModeType.Flamingos2Imaging | ObservingModeType.Flamingos2Mos =>
        Pwfs + GuideProbe.Flamingos2OIWFS
      case ObservingModeType.Igrins2LongSlit                                                                         =>
        Pwfs
      case ObservingModeType.GnirsImaging | ObservingModeType.GnirsLongSlit | ObservingModeType.GnirsIfu              =>
        Pwfs
      case ObservingModeType.GhostIfu                                                                                =>
        Set(GuideProbe.PWFS2)
      case _: VisitorObservingModeType                                                                               =>
        Pwfs

  def isAllowed(mode: ObservingModeType, probe: GuideProbe): Boolean =
    allowedProbes(mode).contains(probe)

  def notAllowedMessage(mode: ObservingModeType, probe: GuideProbe): String =
    s"Guide probe ${probe.tag.toScreamingSnakeCase} cannot be used with observing mode ${mode.tag.toScreamingSnakeCase}."
