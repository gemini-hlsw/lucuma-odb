// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.CoolStarTemperature
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.model.UnnormalizedSED.CoolStarModel

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait PlanetaryNebulaSpectrumSyntax:
  extension (self: PlanetaryNebulaSpectrum)
    def ocs2Tag: String =
      self match
        case PlanetaryNebulaSpectrum.NGC7009 => "Planetary-nebula"
        case PlanetaryNebulaSpectrum.IC5117  => "Planetary-nebula2"

object planetarynebula extends PlanetaryNebulaSpectrumSyntax

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait CoolStarModelSyntax:
  extension (self: CoolStarModel)
    def ocs2Tag: String = self.temperature match
      case CoolStarTemperature.T400K => "T0400K"
      case CoolStarTemperature.T600K => "T0600K"
      case CoolStarTemperature.T800K => "T0800K"
      case CoolStarTemperature.T900K => "T0900K"
      case _                         => s"${self.temperature}"

object coolstar extends CoolStarModelSyntax

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait GalaxySpectrumSyntax:
  extension (self: GalaxySpectrum)
    def ocs2Tag: String =
      self match
        case GalaxySpectrum.Elliptical => "elliptical-galaxy"
        case GalaxySpectrum.Spiral     => "spiral-galaxy"

object gallaxyspectrum extends GalaxySpectrumSyntax

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait HIIRegionSpectrumSyntax:
  extension (self: HIIRegionSpectrum)
    def ocs2Tag: String =
      self match
        case HIIRegionSpectrum.OrionNebula => "Orion-nebula"

object hiiregionspectrum extends HIIRegionSpectrumSyntax

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait PlanetSpectrumSyntax:
  extension (self: PlanetSpectrum)
    def ocs2Tag: String =
      self match
        case PlanetSpectrum.Mars    => "Mars"
        case PlanetSpectrum.Jupiter => "Jupiter"
        case PlanetSpectrum.Saturn  => "Saturn"
        case PlanetSpectrum.Uranus  => "Uranus"
        case PlanetSpectrum.Neptune => "Neptune"

object planets extends PlanetSpectrumSyntax

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait QuasarSpectrumSyntax:
  extension (self: QuasarSpectrum)
    def ocs2Tag: String =
      self match
        case QuasarSpectrum.QS0  => "QSO"
        case QuasarSpectrum.QS02 => "QSO2"

object quasar extends QuasarSpectrumSyntax

/**
 * Syntax extensions for missing ocs2Tag items
 */
trait StellarLibrarySpectrumSyntax:
  extension (self: StellarLibrarySpectrum) def ocs2Tag: String = self.tag

object stellarLibrary extends StellarLibrarySpectrumSyntax
