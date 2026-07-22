// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.codec.numeric.numeric
import skunk.codec.temporal.date
import skunk.codec.text.text

trait ArchiveMatchView[F[_]] extends BaseMapping[F]:

  // The GOA record fields do not all map onto lucuma types; the archive
  // instrument name, disperser, filter, QA state, observation type and class,
  // and the program and observation ids are text.  The archive holds both OCS-
  // and GPP-era data, so those last four carry values from either era.
  object ArchiveMatchView extends TableDef("v_archive_match"):
    val Id: ColumnRef               = col("c_match_id",            text)
    val ObservationId: ColumnRef    = col("c_observation_id",      observation_id)
    val Name: ColumnRef             = col("c_file_name",           text)
    val DataLabel: ColumnRef        = col("c_data_label",          text.opt)
    val Instrument: ColumnRef       = col("c_instrument",          text)
    val ObservationType: ColumnRef  = col("c_observation_type",    text)
    val ObservationClass: ColumnRef = col("c_observation_class",   text.opt)
    val QaState: ColumnRef          = col("c_qa_state",            text.opt)
    val UtDateTime: ColumnRef       = col("c_ut_datetime",         core_timestamp.opt)
    val ReleaseDate: ColumnRef      = col("c_release_date",        date.opt)
    val ProgramId: ColumnRef        = col("c_goa_program_id",      text.opt)
    val GoaObservationId: ColumnRef = col("c_goa_observation_id",  text.opt)
    val ObjectName: ColumnRef       = col("c_object_name",         text.opt)
    val Disperser: ColumnRef        = col("c_disperser",           text.opt)
    val Filter: ColumnRef           = col("c_filter",              text.opt)
    val Airmass: ColumnRef          = col("c_airmass",             numeric.opt)

    object Coordinates:
      val SyntheticId: ColumnRef    = col("c_coordinates_id",      text.embedded)
      val Ra: ColumnRef             = col("c_ra",                  right_ascension.embedded)
      val Dec: ColumnRef            = col("c_dec",                 declination.embedded)

    object Exposure:
      val SyntheticId: ColumnRef    = col("c_exposure_id",         text.embedded)
      val Value: ColumnRef          = col("c_exposure",            time_span.embedded)

    object Wavelength:
      val SyntheticId: ColumnRef    = col("c_wavelength_id",       text.embedded)
      val Value: ColumnRef          = col("c_wavelength",          wavelength_pm.embedded)

    object Azimuth:
      val SyntheticId: ColumnRef    = col("c_azimuth_id",          text.embedded)
      val Value: ColumnRef          = col("c_azimuth",             angle_µas.embedded)

    object Elevation:
      val SyntheticId: ColumnRef    = col("c_elevation_id",        text.embedded)
      val Value: ColumnRef          = col("c_elevation",           angle_µas.embedded)

    // The match's own coordinates and those of the search it came from, read as
    // plain optional values so the separation can be derived from both.
    object Distance:
      val SyntheticId: ColumnRef    = col("c_distance_id",         text.embedded)
      val Ra: ColumnRef             = col("c_distance_ra",         right_ascension.opt)
      val Dec: ColumnRef            = col("c_distance_dec",        declination.opt)
      val SearchRa: ColumnRef       = col("c_search_ra",           right_ascension.opt)
      val SearchDec: ColumnRef      = col("c_search_dec",          declination.opt)
