-- Views the Archive Duplication Search is read through.
--
-- Both add what a GraphQL mapping needs and the underlying tables do not
-- carry: a single column identifying a row, and synthetic ids that go null
-- alongside the optional values they key, so an absent value reads as an
-- absent object rather than an object full of nulls.

-- Adds c_search_radius_id: the radius is optional, and an observation that was
-- never searched (or whose mode has no science area) has none.
CREATE OR REPLACE VIEW v_archive_duplication AS
  SELECT
    o.c_observation_id,
    COALESCE(d.c_state, 'not_checked'::e_archive_duplication_state) AS c_state,
    COALESCE(d.c_match_count, 0)                                AS c_match_count,
    COALESCE(d.c_saturated, FALSE)                              AS c_saturated,
    d.c_last_checked_at,
    d.c_error,
    d.c_search_ra,
    d.c_search_dec,
    d.c_search_target,
    d.c_search_radius,
    CASE WHEN d.c_search_ra     IS NOT NULL THEN o.c_observation_id END AS c_search_center_id,
    CASE WHEN d.c_search_radius IS NOT NULL THEN o.c_observation_id END AS c_search_radius_id
  FROM t_observation o
  LEFT JOIN t_archive_duplication d ON d.c_observation_id = o.c_observation_id;

-- t_archive_match is keyed by observation and file name together, so the view
-- supplies c_match_id to identify a match with a single column.
--
-- The search center is carried on every row because the per-match angular
-- distance is derived from it, and a match cannot reach its snapshot header on
-- its own.
CREATE VIEW v_archive_match AS
  SELECT
    m.c_observation_id,
    m.c_file_name,
    m.c_observation_id || ':' || m.c_file_name AS c_match_id,
    m.c_data_label,
    m.c_ra,
    m.c_dec,
    m.c_instrument,
    m.c_observation_type,
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
