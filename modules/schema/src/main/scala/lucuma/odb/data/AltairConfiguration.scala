// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.derived.*
import cats.syntax.option.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.CassRotator
import lucuma.core.enums.FieldLens
import lucuma.core.enums.GuideProbe
import lucuma.core.math.Angle
import monocle.Focus
import monocle.Lens

/**
 * Altair (Gemini North adaptive optics) configuration for an observation.
 *
 * `explicitFieldLens` is the user's override of the automatic choice; see `fieldLens`.
 */
case class AltairConfiguration(
  mode:              AltairMode,
  explicitFieldLens: Option[FieldLens],
  cassRotator:       CassRotator,
  ndFilter:          AltairNdFilter
) derives Eq:

  /** The wavefront sensor holding Altair's natural guide star. */
  def guideProbe: GuideProbe =
    mode.guideProbe

  /** The field lens position chosen automatically; see [[AltairConfiguration.defaultFieldLens]]. */
  def defaultFieldLens(guideStarSeparation: Option[Angle]): Option[FieldLens] =
    AltairConfiguration.defaultFieldLens(mode, guideStarSeparation)

  /**
   * The field lens position to use. The laser modes always need it; in NGS the
   * explicit choice wins, otherwise it follows the guide star separation, which
   * is unknown until a guide star has been selected.
   */
  def fieldLens(guideStarSeparation: Option[Angle]): Option[FieldLens] =
    AltairConfiguration.fieldLens(mode, explicitFieldLens, guideStarSeparation)

object AltairConfiguration:

  /** Beyond this guide star separation the NGS field lens goes in. */
  val FieldLensSeparationThreshold: Angle =
    Angle.fromDoubleArcseconds(1.0)

  /**
   * The automatic field lens choice, ignoring any explicit override. The laser modes always need
   * the field lens; NGS follows the guide star separation, which is unknown until a guide star has
   * been selected.
   */
  def defaultFieldLens(mode: AltairMode, guideStarSeparation: Option[Angle]): Option[FieldLens] =
    mode match
      case AltairMode.Lgs | AltairMode.LgsP1 =>
        FieldLens.In.some
      case AltairMode.Ngs                    =>
        guideStarSeparation.map: separation =>
          if separation.toMicroarcseconds > FieldLensSeparationThreshold.toMicroarcseconds
          then FieldLens.In
          else FieldLens.Out

  /** The field lens that will be used: the explicit override, else [[defaultFieldLens]]. */
  def fieldLens(
    mode:                AltairMode,
    explicitFieldLens:   Option[FieldLens],
    guideStarSeparation: Option[Angle]
  ): Option[FieldLens] =
    mode match
      case AltairMode.Lgs | AltairMode.LgsP1 =>
        FieldLens.In.some
      case AltairMode.Ngs                    =>
        explicitFieldLens.orElse(defaultFieldLens(mode, guideStarSeparation))

  val mode: Lens[AltairConfiguration, AltairMode] =
    Focus[AltairConfiguration](_.mode)

  val explicitFieldLens: Lens[AltairConfiguration, Option[FieldLens]] =
    Focus[AltairConfiguration](_.explicitFieldLens)

  val cassRotator: Lens[AltairConfiguration, CassRotator] =
    Focus[AltairConfiguration](_.cassRotator)

  val ndFilter: Lens[AltairConfiguration, AltairNdFilter] =
    Focus[AltairConfiguration](_.ndFilter)
