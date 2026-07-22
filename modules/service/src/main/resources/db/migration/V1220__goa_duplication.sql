-- Archive Duplication Search: the per-observation snapshot of what the Gemini
-- Observatory Archive (GOA) returned around the observation's pointing.
--
-- The snapshot lives in its own table rather than in columns on t_observation
-- deliberately.  Writing it is not an observation edit: t_observation carries
-- triggers that invalidate obscalc (and so re-run the ITC) and publish
-- observationEdit notifications on any row change, and a duplication refresh
-- must not do either.
--
-- The headline values (count, saturation, last checked) are denormalized here
-- so the proposal PDF and GraphQL can read them without aggregating over
-- t_goa_match.

-- The outcome of the most recent search attempt.
--
--   not_checked  the search ran but could not be performed -- an instrument GOA
--                does not know (MAROON-X), or no resolvable pointing and no
--                usable target name.  Advisory, not an error.
--   checked      the search ran and the stored matches are its result.
--   error        the most recent attempt failed (GOA unreachable, bad response).
--                Any previously stored matches are left untouched, so
--                c_match_count / c_last_checked_at still describe the last
--                good snapshot.
CREATE TYPE e_goa_duplication_state AS ENUM (
  'not_checked',
  'checked',
  'error'
);

CREATE TABLE t_goa_duplication (

  c_observation_id  d_observation_id        PRIMARY KEY
    REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,

  c_state           e_goa_duplication_state NOT NULL,

  -- File-level match count, matching the PIT.  Saturated when a constituent
  -- query came back with GOA's hard cap of 500 records, in which case the
  -- count is a floor rather than an exact figure.
  c_match_count     int4                    NOT NULL DEFAULT 0 CHECK (c_match_count >= 0),
  c_saturated       boolean                 NOT NULL DEFAULT FALSE,

  -- When the stored matches were gathered.  Null until a search has succeeded.
  c_last_checked_at timestamp               NULL,

  c_error           text                    NULL CHECK (c_error <> ''),

  -- The area searched.  Kept so the per-match angular distance can be derived
  -- from what was actually searched rather than from the observation as it
  -- stands now.  A sidereal search stores coordinates, a non-sidereal one the
  -- target name it was run against.
  c_search_ra       d_angle_µas             NULL,
  c_search_dec      d_angle_µas             NULL,
  c_search_target   text                    NULL,
  c_search_radius   d_angle_µas             NULL CHECK (c_search_radius > 0),

  CONSTRAINT goa_duplication_error_message CHECK ((c_state = 'error') = (c_error IS NOT NULL)),
  CONSTRAINT goa_duplication_checked_at    CHECK (c_state <> 'checked' OR c_last_checked_at IS NOT NULL),
  CONSTRAINT goa_duplication_search_coords CHECK (num_nulls(c_search_ra, c_search_dec) <> 1),
  CONSTRAINT goa_duplication_search_key    CHECK (num_nonnulls(c_search_ra, c_search_target) <= 1)
);

COMMENT ON TABLE t_goa_duplication IS
  'Archive Duplication Search snapshot header, one row per observation that has been searched.';

-- One row per matched archive file.  Rows hang off the snapshot header, so
-- replacing a snapshot is a delete of these plus an upsert of the header, and
-- there can be no matches without a header.
--
-- The GOA record fields do not all map onto lucuma types, so the archive
-- instrument name, disperser, filter, QA state, observation type and class, and
-- the program and observation ids are all kept as text.  The archive holds both
-- OCS- and GPP-era data, so those last four carry values from either era.
CREATE TABLE t_goa_match (

  c_observation_id     d_observation_id NOT NULL
    REFERENCES t_goa_duplication(c_observation_id) ON DELETE CASCADE,

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

  CONSTRAINT goa_match_coords CHECK (num_nulls(c_ra, c_dec) <> 1)
);

COMMENT ON TABLE t_goa_match IS
  'One archived file matched by an Archive Duplication Search.';
COMMENT ON COLUMN t_goa_match.c_goa_program_id IS
  'Program id as reported by GOA: a GPP program id, or an OCS one such as GN-2019A-Q-101.';
COMMENT ON COLUMN t_goa_match.c_goa_observation_id IS
  'Observation id as reported by GOA: a GPP observation id, or an OCS one such as GN-2019A-Q-101-11.';

-- Every observation has a duplication result, whether or not it has been
-- searched: an observation with no header row reads as never checked, with no
-- matches.
CREATE VIEW v_goa_duplication AS
  SELECT
    o.c_observation_id,
    COALESCE(d.c_state, 'not_checked'::e_goa_duplication_state) AS c_state,
    COALESCE(d.c_match_count, 0)                                AS c_match_count,
    COALESCE(d.c_saturated, FALSE)                              AS c_saturated,
    d.c_last_checked_at,
    d.c_error,
    d.c_search_ra,
    d.c_search_dec,
    d.c_search_target,
    d.c_search_radius,
    CASE WHEN d.c_search_ra IS NOT NULL THEN o.c_observation_id END AS c_search_center_id
  FROM t_observation o
  LEFT JOIN t_goa_duplication d ON d.c_observation_id = o.c_observation_id;
