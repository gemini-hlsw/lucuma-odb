-- Every GOA query the Archive Duplication Search sends carries the OBJECT, we can drop it.
--
-- The searches now also filter by Mode Class, so a snapshot taken before this
-- counts matches of both kinds and reads high.  Given we are not yet using this
-- feature we can drop the results and recalculate on demand.

DELETE FROM t_archive_duplication;

DROP VIEW v_archive_match;

ALTER TABLE t_archive_match DROP COLUMN c_observation_type;

CREATE VIEW v_archive_match AS
  SELECT
    m.c_observation_id,
    m.c_file_name,
    m.c_observation_id || ':' || m.c_file_name AS c_match_id,
    m.c_data_label,
    m.c_ra,
    m.c_dec,
    m.c_instrument,
    m.c_observation_class,
    m.c_qa_state,
    m.c_ut_datetime,
    m.c_release_date,
    m.c_goa_program_id,
    m.c_goa_observation_id,
    m.c_object_name,
    m.c_exposure,
    m.c_disperser,
    m.c_filter,
    m.c_wavelength,
    m.c_airmass::numeric AS c_airmass,
    m.c_azimuth,
    m.c_elevation,

    CASE WHEN m.c_ra         IS NOT NULL THEN m.c_observation_id || ':' || m.c_file_name END AS c_coordinates_id,
    -- The separation is measurable only when both the match and the search it
    -- came from have a pointing, so the distance reads as absent otherwise.
    CASE WHEN m.c_ra IS NOT NULL AND d.c_search_ra IS NOT NULL
         THEN m.c_observation_id || ':' || m.c_file_name END AS c_distance_id,
    CASE WHEN m.c_exposure   IS NOT NULL THEN m.c_observation_id || ':' || m.c_file_name END AS c_exposure_id,
    CASE WHEN m.c_wavelength IS NOT NULL THEN m.c_observation_id || ':' || m.c_file_name END AS c_wavelength_id,
    CASE WHEN m.c_azimuth    IS NOT NULL THEN m.c_observation_id || ':' || m.c_file_name END AS c_azimuth_id,
    CASE WHEN m.c_elevation  IS NOT NULL THEN m.c_observation_id || ':' || m.c_file_name END AS c_elevation_id,

    -- Aliased so the distance calculation reads the coordinates as plain
    -- nullable values, independently of the optional objects above.
    m.c_ra      AS c_distance_ra,
    m.c_dec     AS c_distance_dec,
    d.c_search_ra,
    d.c_search_dec
  FROM t_archive_match m
  JOIN t_archive_duplication d ON d.c_observation_id = m.c_observation_id;
