// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptySet
import cats.parse.Parser
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.model.sequence.StepConfig.Gcal

trait GcalParsers:

  import common.columnSep
  import util.*

  val baselineType: Parser[GcalBaselineType] =
    oneOfEnumerated[GcalBaselineType].withContext("Gcal baseline type")

  val arc: Parser[GcalArc] =
    oneOf(
      "Ar arc"   -> GcalArc.ArArc,
      "ThAr arc" -> GcalArc.ThArArc,
      "CuAr arc" -> GcalArc.CuArArc,
      "Xe arc"   -> GcalArc.XeArc
    ).withContext("Gcal arc")

  val arcs: Parser[NonEmptySet[GcalArc]] =
    arc.repSep(Parser.char(';')).map(_.toNes).withContext("Gcal arcs")

  val continuum: Parser[GcalContinuum] =
    oneOf(
      "IR grey body - low"  -> GcalContinuum.IrGreyBodyLow,
      "IR grey body - high" -> GcalContinuum.IrGreyBodyHigh,
      "Quartz Halogen"      -> GcalContinuum.QuartzHalogen5W,
      "100W Quartz Halogen" -> GcalContinuum.QuartzHalogen100W
    ).withContext("Gcal continuum")

  val diffuser: Parser[GcalDiffuser] =
    oneOf(
      "IR"      -> GcalDiffuser.Ir,
      "Visible" -> GcalDiffuser.Visible
    ).withContext("Gcal diffuser")

  val filter: Parser[GcalFilter] =
    oneOf(
      "none"         -> GcalFilter.None,
      "None"         -> GcalFilter.None,
      "ND1.0"        -> GcalFilter.Nd10,
      "ND1.6"        -> GcalFilter.Nd16,
      "ND2.0"        -> GcalFilter.Nd20,
      "ND3.0"        -> GcalFilter.Nd30,
      "ND4.0"        -> GcalFilter.Nd40,
      "ND4-5"        -> GcalFilter.Nd45,
      "ND5.0"        -> GcalFilter.Nd50,
      "GMOS balance" -> GcalFilter.Gmos,
      "HROS balance" -> GcalFilter.Hros,
      "NIR balance"  -> GcalFilter.Nir
    ).withContext("Gcal filter")

  val shutter: Parser[GcalShutter] =
    oneOfEnumerated[GcalShutter].withContext("Gcal shutter")

  val gcalLamp: Parser[Gcal.Lamp] =
    gcal.arcs.eitherOr(gcal.continuum).map(Gcal.Lamp.fromEither)

  val stepConfig: Parser[Gcal] =
    (
      (gcal.filter   <* columnSep) ~
      (gcal.diffuser <* columnSep) ~
      (gcalLamp      <* columnSep) ~
      gcal.shutter
    ).map { case (((filter, diffuser), lamp), shutter) =>
      Gcal(lamp, filter, diffuser, shutter)
    }

object gcal extends GcalParsers