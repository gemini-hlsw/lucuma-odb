-- Archive Duplication Search: the per-observation snapshot of what the Gemini
-- Observatory Archive (GOA) returned around the observation's pointing.
--
-- The snapshot lives in its own table rather than in columns on t_observation
-- deliberately to avoid generating observation edit events.
--
-- The values count, saturation, last checked are denormalized here
-- so the proposal PDF and GraphQL can read them without aggregating over
-- t_archive_match.

-- The outcome of the most recent search attempt.
--
--   not_checked  the search ran but could not be performed -- an instrument GOA
--                does not know, or no resolvable pointing and no usable target name.  
--   checked      the search ran and the stored matches are its result.
--   error        the most recent attempt failed (GOA unreachable, bad response).
--                c_match_count / c_last_checked_at still describe the last good snapshot.
CREATE TYPE e_archive_duplication_state AS ENUM (
  'not_checked',
  'checked',
  'error'
);

CREATE TABLE t_archive_duplication (

  c_observation_id  d_observation_id        PRIMARY KEY
    REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,

  c_state           e_archive_duplication_state NOT NULL,

  -- File-level match count, matching the PIT.  Saturated when a constituent
  -- query came back with GOA's hard cap of 500 records, in which case the
  -- count is a floor rather than an exact figure.
  c_match_count     int4                    NOT NULL DEFAULT 0 CHECK (c_match_count >= 0),
  c_saturated       boolean                 NOT NULL DEFAULT FALSE,

  -- When the stored matches were gathered.  Null until a search has succeeded.
  c_last_checked_at timestamp               NULL,

  c_error           text                    NULL CHECK (c_error <> ''),

  -- The area searched.  Kept so the per-match angular distance can be derived
  -- from what was actually searched rather than from the observation as it stands now.  
  -- A sidereal search stores coordinates, a non-sidereal one the target name it was run against.
  c_search_ra       d_angle_µas             NULL,
  c_search_dec      d_angle_µas             NULL,
  c_search_target   text                    NULL,
  c_search_radius   d_angle_µas             NULL CHECK (c_search_radius > 0),

  CONSTRAINT archive_duplication_error_message CHECK ((c_state = 'error') = (c_error IS NOT NULL)),
  CONSTRAINT archive_duplication_checked_at    CHECK (c_state <> 'checked' OR c_last_checked_at IS NOT NULL),
  CONSTRAINT archive_duplication_search_coords CHECK (num_nulls(c_search_ra, c_search_dec) <> 1),
  CONSTRAINT archive_duplication_search_key    CHECK (num_nonnulls(c_search_ra, c_search_target) <= 1)
);

COMMENT ON TABLE t_archive_duplication IS
  'Archive Duplication Search snapshot header, one row per observation that has been searched.';

-- One row per matched archive file.  
--
-- The GOA record fields do not all map onto lucuma types, so the archive
-- instrument name, disperser, filter, QA state, observation type and class, and
-- the program and observation ids are all kept as text.  
--
-- The archive holds both
-- OCS- and GPP-era data, so those last four carry values from either era.
CREATE TABLE t_archive_match (

  c_observation_id     d_observation_id NOT NULL
    REFERENCES t_archive_duplication(c_observation_id) ON DELETE CASCADE,

  -- The archive file name, unique within a snapshot: matches are deduped by it.
  c_file_name          text             NOT NULL,

  PRIMARY KEY (c_observation_id, c_file_name),

  c_data_label         text             NULL,
  c_ra                 d_angle_µas      NULL,
  c_dec                d_angle_µas      NULL,
  c_instrument         text             NOT NULL,
  c_observation_type   text             NOT NULL,
  c_observation_class  text             NULL,
  c_qa_state           text             NULL,
  c_ut_datetime        timestamp        NULL,
  c_release_date       date             NULL,
  c_goa_program_id     text             NULL,
  c_goa_observation_id text             NULL,
  c_object_name        text             NULL,
  c_exposure           interval         NULL CHECK (c_exposure >= interval '0 seconds'),
  c_disperser          text             NULL,
  c_filter             text             NULL,
  c_wavelength         d_wavelength_pm  NULL,
  c_airmass            float8           NULL,
  c_azimuth            d_angle_µas      NULL,
  c_elevation          d_angle_µas      NULL,

  CONSTRAINT archive_match_coords CHECK (num_nulls(c_ra, c_dec) <> 1)
);

COMMENT ON TABLE t_archive_match IS
  'One archived file matched by an Archive Duplication Search.';
COMMENT ON COLUMN t_archive_match.c_goa_program_id IS
  'Program id as reported by GOA: a GPP program id, or an OCS one such as GN-2019A-Q-101.';
COMMENT ON COLUMN t_archive_match.c_goa_observation_id IS
  'Observation id as reported by GOA: a GPP observation id, or an OCS one such as GN-2019A-Q-101-11.';

-- Views the Archive Duplication Search is read through.
--
CREATE VIEW v_archive_duplication AS
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
