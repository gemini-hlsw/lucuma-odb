-- Remove any existing GMOS north imaging modes without a requirement ETM.
DELETE FROM t_gmos_north_imaging m
WHERE NOT EXISTS (
  SELECT 1 FROM t_exposure_time_mode e WHERE e.c_observation_id = m.c_observation_id AND e.c_role = 'requirement'
);

-- Add the type discriminator and a FK reference to the associated exposure time
-- mode.
ALTER TABLE t_gmos_north_imaging_filter
  ADD COLUMN c_version                e_observing_mode_row_version NOT NULL DEFAULT 'current',
  ADD COLUMN c_exposure_time_mode_id  integer,
  ADD COLUMN c_role                   e_exposure_time_mode_role    NOT NULL DEFAULT 'science' CHECK (c_role = 'science'),
  ADD FOREIGN KEY (c_exposure_time_mode_id, c_role)
    REFERENCES t_exposure_time_mode(c_exposure_time_mode_id, c_role)
    ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED;

-- Change the primary key to include the mode value type since now just obs id
-- and filter are not enough to guarantee uniqueness.
ALTER TABLE t_gmos_north_imaging_filter
  DROP CONSTRAINT t_gmos_north_imaging_filter_pkey;

ALTER TABLE t_gmos_north_imaging_filter
  ADD PRIMARY KEY (c_observation_id, c_filter, c_version);

-- Move the "initial" filters over to the normal filter table.
INSERT INTO t_gmos_north_imaging_filter (
  c_observation_id,
  c_filter,
  c_version
)
SELECT
  c_observation_id,
  c_filter,
  'initial'::e_observing_mode_row_version
FROM t_gmos_north_imaging_initial_filter;

-- For each existing imaging filter entry, add a 'science' ETM that is a copy of
-- the requirements ETM and set the c_exposure_time_mode_id to it.
DO $$
DECLARE
  f          RECORD;
  req_etm    t_exposure_time_mode%ROWTYPE;
  new_etm_id integer;
BEGIN
  FOR f IN SELECT * FROM t_gmos_north_imaging_filter LOOP
    SELECT *
      INTO req_etm
      FROM t_exposure_time_mode
      WHERE c_observation_id = f.c_observation_id
      AND c_role = 'requirement';

    IF FOUND THEN
      INSERT INTO t_exposure_time_mode (
        c_observation_id,
        c_role,
        c_exposure_time_mode,
        c_signal_to_noise,
        c_signal_to_noise_at,
        c_exposure_time,
        c_exposure_count
      ) VALUES (
        f.c_observation_id,
        'science',
        req_etm.c_exposure_time_mode,
        req_etm.c_signal_to_noise,
        req_etm.c_signal_to_noise_at,
        req_etm.c_exposure_time,
        req_etm.c_exposure_count
      ) RETURNING c_exposure_time_mode_id INTO new_etm_id;

      UPDATE t_gmos_north_imaging_filter
        SET c_exposure_time_mode_id = new_etm_id
        WHERE c_observation_id  = f.c_observation_id
          AND c_filter          = f.c_filter
          AND c_version = f.c_version;
    END IF;
  END LOOP;
END $$;

-- Going forward, we want every imaging filter to be associated with a unique
-- science ETM.
ALTER TABLE t_gmos_north_imaging_filter
  ALTER COLUMN c_exposure_time_mode_id SET NOT NULL,
  ADD CONSTRAINT t_gmos_north_imaging_filter_unique_exposure_time_mode_id
    UNIQUE (c_exposure_time_mode_id);

DROP VIEW v_gmos_north_imaging;
DROP VIEW v_gmos_north_imaging_filter;
DROP VIEW v_gmos_north_imaging_initial_filter;

-- No need for this now.
DROP TABLE t_gmos_north_imaging_initial_filter;

-- Create a view in order to present a column with the array of filter names
-- as needed by the 'configuration' checking.
CREATE VIEW v_gmos_north_imaging AS
  SELECT
    i.*,
    f.c_filters
  FROM
    t_gmos_north_imaging i
  LEFT JOIN (
    SELECT
      c_observation_id,
      array_remove(array_agg(c_filter ORDER BY c_filter), NULL) AS c_filters
    FROM t_gmos_north_imaging_filter
    WHERE c_version = 'current'
    GROUP BY c_observation_id
  ) AS f USING (c_observation_id);