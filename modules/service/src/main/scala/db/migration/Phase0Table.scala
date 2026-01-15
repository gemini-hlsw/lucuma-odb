// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.syntax.foldable.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.Instrument
import lucuma.odb.phase0.Flamingos2SpectroscopyRow
import lucuma.odb.phase0.GmosImagingRow
import lucuma.odb.phase0.GmosSpectroscopyRow
import lucuma.odb.phase0.ImagingRow
import lucuma.odb.phase0.SpectroscopyRow
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.Encoder
import skunk.codec.boolean.bool
import skunk.codec.numeric.numeric
import skunk.codec.text.text

trait Phase0Table[A] {

  def name: String

  def columns: List[String]

  def indexColumn: String =
    "c_index"

  def encoder: Encoder[A]

  def truncate: String =
    s"TRUNCATE $name"

  def copyFromStdin: String =
    s"COPY $name ( ${(indexColumn :: columns).mkString("\n", ",", "\n")} ) FROM STDIN WITH ( DELIMITER '|', NULL 'NULL' )"

  def stdinLine(a: A, index: PosInt): String =
    (int4_pos *: encoder).encode((index, a)).map(_.getOrElse("NULL")).intercalate("|")

  def deleteFrom(inst: Instrument): String =
    s"""
      DELETE FROM $name
        WHERE c_instrument = '${inst.tag}';
    """
}

object Phase0Table {

  val Spectroscopy = new Phase0Table[SpectroscopyRow] {
    override def name: String =
      "t_spectroscopy_config_option"

    override def columns: List[String] =
      List(
        "c_instrument",
        "c_name",
        "c_focal_plane",
        "c_fpu_label",
        "c_slit_width",
        "c_slit_length",
        "c_disperser_label",
        "c_filter_label",
        "c_wavelength_min",
        "c_wavelength_max",
        "c_wavelength_optimal",
        "c_wavelength_coverage",
        "c_resolution",
        "c_ao",
        "c_capability",
        "c_site",
        "c_hmin_hot",
        "c_hmin_solar"
      )

    override def encoder: Encoder[SpectroscopyRow] =
      (
        instrument    *:
        text          *:
        text          *:
        text          *:
        angle_µas     *:
        angle_µas     *:
        text          *:
        text.opt      *:
        wavelength_pm *:
        wavelength_pm *:
        wavelength_pm *:
        wavelength_pm *:
        int4_pos      *:
        bool          *:
        text.opt      *:
        site          *:
        numeric.opt   *:
        numeric.opt
      ).contramap[SpectroscopyRow] { row => (
        row.instrument,
        row.description,
        row.fpuOption.tag,
        row.fpu,
        row.slitWidth,
        row.slitLength,
        row.disperser,
        row.filter,
        row.wavelengthMin,
        row.wavelengthMax,
        row.wavelengthOpt,
        row.wavelengthCov,
        row.resolution,
        row.ao,
        row.capability.map(_.tag),
        row.site,
        row.hminHot,
        row.hminSolar
      )}

  }

  trait GmosSpectroscopyTable[G, L, U, R <: GmosSpectroscopyRow[G, L, U]] extends Phase0Table[R] {

    protected def enc(g: Encoder[G], l: Encoder[L], u: Encoder[U]): Encoder[R] =
      (
        instrument *:
        g          *:
        l.opt      *:
        u
      ).contramap[R] { row => (
        row.spec.instrument,
        row.disperser,
        row.filter,
        row.fpu
      )}

    override def columns: List[String] =
      List(
        "c_instrument",
        "c_grating",
        "c_filter",
        "c_fpu"
      )
  }

  val SpectroscopyGmosNorth = new GmosSpectroscopyTable[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu, GmosSpectroscopyRow.GmosNorth] {
    override def name: String =
      s"${Spectroscopy.name}_gmos_north"

    override def encoder: Encoder[GmosSpectroscopyRow.GmosNorth] =
      enc(gmos_north_grating, gmos_north_filter, gmos_north_fpu)
  }

  val SpectroscopyGmosSouth = new GmosSpectroscopyTable[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu, GmosSpectroscopyRow.GmosSouth] {
    override def name: String =
      s"${Spectroscopy.name}_gmos_south"

    override def encoder: Encoder[GmosSpectroscopyRow.GmosSouth] =
      enc(gmos_south_grating, gmos_south_filter, gmos_south_fpu)
  }

  val SpectroscopyFlamingos2 = new Phase0Table[Flamingos2SpectroscopyRow] {

    override def name: String =
      s"${Spectroscopy.name}_f2"

    override def encoder: Encoder[Flamingos2SpectroscopyRow] =
      (
        instrument   *:
        flamingos_2_disperser *:
        flamingos_2_filter    *:
        flamingos_2_fpu
      ).contramap[Flamingos2SpectroscopyRow] { row => (
        row.spec.instrument,
        row.disperser,
        row.filter,
        row.fpu
      )}

    override def columns: List[String] =
      List(
        "c_instrument",
        "c_disperser",
        "c_filter",
        "c_fpu"
      )
  }

  val Imaging = new Phase0Table[ImagingRow] {
    override def name: String =
      "t_imaging_config_option"

    override def columns: List[String] =
      List(
        "c_instrument",
        "c_fov",
        "c_filter_label",
        "c_ao",
        "c_site"
      )

    override def encoder: Encoder[ImagingRow] =
      (
        instrument    *:
        angle_µas     *:
        text          *:
        bool          *:
        site
      ).contramap[ImagingRow] { row => (
        row.instrument,
        row.fov,
        row.filter,
        row.ao,
        row.site
      )}

  }

  val ImagingGmosNorth = new Phase0Table[GmosImagingRow.GmosNorth] {

    override def name: String =
      s"${Imaging.name}_gmos_north"

    override def encoder: Encoder[GmosImagingRow.GmosNorth] =
      (
        instrument        *:
        gmos_north_filter
      ).contramap[GmosImagingRow.GmosNorth]{ row => (
        row.img.instrument,
        row.filter,
      )}

    override def columns: List[String] =
      List(
        "c_instrument",
        "c_filter",
      )
  }

  val ImagingGmosSouth = new Phase0Table[GmosImagingRow.GmosSouth] {

    override def name: String =
      s"${Imaging.name}_gmos_south"

    override def encoder: Encoder[GmosImagingRow.GmosSouth] =
      (
        instrument       *:
        gmos_south_filter
      ).contramap[GmosImagingRow.GmosSouth]{ row => (
        row.img.instrument,
        row.filter,
      )}

    override def columns: List[String] =
      List(
        "c_instrument",
        "c_filter",
      )
  }
}
