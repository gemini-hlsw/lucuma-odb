// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.*
import cats.parse.Rfc5234.htab
import cats.parse.Rfc5234.sp
import cats.parse.Rfc5234.vchar
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.parser.MiscParsers.posBigDecimal
import lucuma.core.parser.MiscParsers.posInt
import lucuma.core.util.Enumerated

/*
Instrument	Config	Focal Plane	fpu	slit width	slit length	disperser	filter	wave min	wave max	wave optimal	wave coverage	resolution	AO	capabilities	site
GMOS-N	B1200 0.25"	singleslit,multislit	0.25"	0.250	330	B1200	none	0.36	0.72	0.463	0.164	7488	no		n
 */

sealed trait ConfigurationRow

/**
 * Represents a single row in the spectroscopy instrument option "phase 0" table.
 */
case class SpectroscopyRow(
  instrument:     Instrument,
  description:    String,
  fpuOption:      FpuOption,
  fpu:            String,
  slitWidth:      Angle,
  slitLength:     Angle,
  disperser:      String,
  filter:         Option[String],
  wavelengthMin:  Wavelength,
  wavelengthMax:  Wavelength,
  wavelengthOpt:  Wavelength,
  wavelengthCov:  Wavelength,
  resolution:     PosInt,
  ao:             Boolean,
  capability:     Option[Capability],
  site:           Site,
  hminHot:        Option[BigDecimal],
  hminSolar:      Option[BigDecimal]
) extends ConfigurationRow

trait RowParsers:
  val string: Parser[String] =
    (vchar | sp).rep.string

  val instrument: Parser[Instrument] =
    string.mapFilter { s =>
      Enumerated[Instrument].all.find { inst =>
        inst.shortName.equalsIgnoreCase(s) || inst.longName === s || inst.tag === s
      }
    }

  val arcsec: Parser0[Angle] =
    posBigDecimal.map { bd =>
      Angle.fromBigDecimalArcseconds(bd.value)
    }

  val ao: Parser[Boolean] =
    string.map {
      case "yes" => true
      case _     => false
    }

  val site: Parser[Site] =
    string.mapFilter {
      case "n" => Site.GN.some
      case "s" => Site.GS.some
      case _   => none
    }

object RowParsers extends RowParsers

object SpectroscopyRow extends RowParsers {

  val filter: Parser[Option[String]] =
    string.map { s => Option.when(s =!= "none")(s) }

  val fpuOptions: Parser[Set[FpuOption]] =
    string.map { s =>
      Set.from(
        s.split(',').toList.traverse { fpuOpt =>
          FpuOption.values.find(_.label === fpuOpt)
        }.toList.flatten
      )
    }

  val µm: Parser0[Wavelength] =
    posBigDecimal.mapFilter { bd =>
      Wavelength.decimalMicrometers.getOption(bd.value)
    }

  val capability: Parser0[Option[Capability]] =
    vchar.rep0.string.mapFilter {
      case "" => none.some
      case s  => Capability.values.find(_.label === s).map(_.some)
    }

  val optBigDecimal: Parser0[Option[BigDecimal]] =
    vchar.rep0.string.map {
      case "" => none
      case s  => scala.util.Try(BigDecimal(s)).toOption
    }

  /**
   * A single line in the .tsv file is split into 1 or more rows according to the FPU option.  So a
    * value of "singleslit,multislit" becames two rows identical in every respect except tht one is
    * single slit and the other multislit (MOS).
   */
  val hminColumns: Parser0[(Option[BigDecimal], Option[BigDecimal])] =
    ((htab *> optBigDecimal) ~ (htab *> optBigDecimal)).?.map {
      case Some((hot, solar)) => (hot, solar)
      case None               => (none, none)
    }

  val rows: Parser[List[SpectroscopyRow]] = (
    (instrument    <* htab) ~
    (string        <* htab) ~
    (fpuOptions    <* htab) ~
    (string        <* htab) ~
    (arcsec        <* htab) ~
    (arcsec        <* htab) ~
    (string        <* htab) ~
    (filter        <* htab) ~
    (µm            <* htab) ~
    (µm            <* htab) ~
    (µm            <* htab) ~
    (µm            <* htab) ~
    (posInt        <* htab) ~
    (ao            <* htab) ~
    (capability    <* htab) ~
    site ~
    hminColumns
  ).map { case ((((((((((((((((inst, desc), fpuOpts), fpu), slitWidth), slitLength), disp), filter), min), max), opt), cov), res), ao), capability), site), (hminHot, hminSolar)) =>
    fpuOpts.toList.map { fpuOpt =>
      SpectroscopyRow(inst, desc, fpuOpt, fpu, slitWidth, slitLength, disp, filter, min, max, opt, cov, res, ao, capability, site, hminHot, hminSolar)
    }
  }

}

/*
Instrument	FoV	 Filters	AO	capabilities	site
GMOS-S	330	u,g,r,i,CaT,z,Z,Y,ri,HeII,HeIIC,OIII,OIIIC,Ha,HaC,SII,OVIC,OVI,DS920,g+GG455,g+OG515, r+RG610,i+CaT,z+CaT	no		s
 */

/**
 * Represents a single row in the spectroscopy instrument option "phase 0" table.
 */
case class ImagingRow(
  instrument:     Instrument,
  fov:            Angle,
  filter:         String,
  ao:             Boolean,
  capability:     Option[Capability],
  site:           Site
) extends ConfigurationRow

object ImagingRow extends RowParsers {

  val filterOptions: Parser[Set[String]] =
    string.map { s =>
      Set.from(
        s.split(',')
      )
    }

  /**
   * A single line in the .tsv file is split into 1 or more rows according to the filters included.
   */
  val rows: Parser[List[ImagingRow]] = (
    (instrument    <* htab) ~
    (arcsec        <* htab) ~
    (filterOptions <* htab) ~
    (ao            <* htab) ~
    (string.?      <* htab) ~
    site
  ).map { case (((((inst, fov), filterOpts), ao), _), site) =>
    filterOpts.toList.map { filter =>
      ImagingRow(inst, fov, filter, ao, None, site)
    }
  }

}
