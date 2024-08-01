// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.phase0

import cats.parse.*
import cats.parse.Parser.*
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

/*
Instrument	Config	Focal Plane	fpu	slit width	slit length	disperser	filter	wave min	wave max	wave optimal	wave coverage	resolution	AO	capabilities	site
GMOS-N	B1200 0.25"	singleslit,multislit	0.25"	0.250	330	B1200	none	0.36	0.72	0.463	0.164	7488	no		n
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
  site:           Site
)

object SpectroscopyRow {

  val string: Parser[String] =
    (vchar | sp).rep.string

  val instrument: Parser[Instrument] =
    string.mapFilter { s =>
      Instrument.all.find { inst =>
        inst.shortName === s || inst.longName === s || inst.tag === s
      }
    }

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

  val arcsec: Parser0[Angle] =
    posBigDecimal.map { bd =>
      Angle.fromBigDecimalArcseconds(bd.value)
    }

  val µm: Parser0[Wavelength] =
    posBigDecimal.mapFilter { bd =>
      Wavelength.decimalMicrometers.getOption(bd.value)
    }

  val ao: Parser[Boolean] =
    string.map {
      case "yes" => true
      case _     => false
    }

  val capability: Parser0[Option[Capability]] =
    vchar.rep0.string.mapFilter {
      case "" => none.some
      case s  => Capability.values.find(_.label === s).map(_.some)
    }

  val site: Parser[Site] =
    string.mapFilter {
      case "n" => Site.GN.some
      case "s" => Site.GS.some
      case _   => none
    }

  val rows: Parser[List[SpectroscopyRow]] = (
    (instrument <* htab) ~
    (string     <* htab) ~
    (fpuOptions <* htab) ~
    (string     <* htab) ~
    (arcsec     <* htab) ~
    (arcsec     <* htab) ~
    (string     <* htab) ~
    (filter     <* htab) ~
    (µm         <* htab) ~
    (µm         <* htab) ~
    (µm         <* htab) ~
    (µm         <* htab) ~
    (posInt     <* htab) ~
    (ao         <* htab) ~
    (capability <* htab) ~
    site
  ).map { case (((((((((((((((inst, desc), fpuOpts), fpu), slitWidth), slitLength), disp), filter), min), max), opt), cov), res), ao), capability), site) =>
    fpuOpts.toList.map { fpuOpt =>
      SpectroscopyRow(inst, desc, fpuOpt, fpu, slitWidth, slitLength, disp, filter, min, max, opt, cov, res, ao, capability, site)
    }
  }

  def main(args: Array[String]): Unit =
    val x = (
      (instrument <* htab) ~
      (string     <* htab) ~
      (fpuOptions <* htab) ~
      (string     <* htab) ~
      (arcsec     <* htab)
    )

    println(x.parseAll("GMOS-N\tB1200 0.25\"\tsingleslit,multislit\t0.25\"\t0.250\t"))
    val s = "GMOS-N\tB1200 0.25\"\tsingleslit,multislit\t0.25\"\t0.250\t330\tB1200\tnone\t0.36\t0.72\t0.463\t0.164\t7488\tno\t\tn"
    println(rows.parseAll(s))

}