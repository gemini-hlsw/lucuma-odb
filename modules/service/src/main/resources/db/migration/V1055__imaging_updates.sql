-- We track "initial" filters and the current value.  We'd like to do this in
-- just one table so create a type discriminator to distinguish.
CREATE TYPE e_observing_mode_row_version AS ENUM('initial', 'current');

-- We'll reference the ETM id and its role, which will further be fixed at
-- 'science', in the GMOS imaging filter tables.  This ensures that we only
-- reference 'science' ETMs, but requires having a unique constraint.
ALTER TABLE t_exposure_time_mode
  ADD CONSTRAINT t_exposure_time_mode_unique_id_role
  UNIQUE (c_exposure_time_mode_id, c_role);

-- A function that clones all exposure time modes, returning the old
-- exposure time mode id paired with the matching new exposure time mode id.
CREATE OR REPLACE FUNCTION clone_exposure_time_modes(
  old_obs_id d_observation_id,
  new_obs_id d_observation_id
)
RETURNS TABLE (
  old_exposure_time_mode_id integer,
  new_exposure_time_mode_id integer
)
LANGUAGE plpgsql
AS $$
BEGIN

  -- We'll make a temporary table to hold the mapping from the existing ETM to
  -- the new ETM.  Sometimes though, we'll clone multiple observations in the
  -- same transaction, so drop it explictly if it already exists.
  DROP TABLE IF EXISTS etm_map;

  -- The etm_map holds the mapping from old ETM id to the corresponding new
  -- ETM.
  CREATE TEMPORARY TABLE etm_map (
    rn         integer PRIMARY KEY,
    old_etm_id integer NOT NULL,
    new_etm_id integer
  ) ON COMMIT DROP;

  -- Start by inserting the ids of the existing ETMs that will be cloned. We
  -- do this in order by ETM id and hold on to this order in the "rn", or
  -- "row number" field.  We are going to use the row number to join with this
  -- map below.
  INSERT INTO etm_map (
    rn,
    old_etm_id
  )
  SELECT
    row_number() OVER (ORDER BY c_exposure_time_mode_id),
    c_exposure_time_mode_id
  FROM t_exposure_time_mode
  WHERE c_observation_id = old_obs_id
  ORDER BY c_exposure_time_mode_id;

  -- Do the actual copy of the ETMs, returning just new ETM id.  Importantly,
  -- this is done in the same order as the etm_map was created.
  WITH inserted_etms AS (
    INSERT INTO t_exposure_time_mode(
      c_observation_id,
      c_role,
      c_exposure_time_mode,
      c_signal_to_noise,
      c_signal_to_noise_at,
      c_exposure_time,
      c_exposure_count
    )
    SELECT
      new_obs_id,
      c_role,
      c_exposure_time_mode,
      c_signal_to_noise,
      c_signal_to_noise_at,
      c_exposure_time,
      c_exposure_count
    FROM t_exposure_time_mode
    WHERE c_observation_id = old_obs_id
    ORDER BY c_exposure_time_mode_id
    RETURNING c_exposure_time_mode_id AS new_etm_id
  ),

  -- Now we get a result with the row number and the newly inserted ETM id.
  -- The row number here matches the row number of the existing ETM from which
  -- the copy was made in the partially filled in 'etm_map' from above.
  new_etm_ids_with_rn AS (
    SELECT
      row_number() OVER (ORDER BY new_etm_id) AS rn,
      new_etm_id
    FROM inserted_etms
    ORDER BY new_etm_id
  )

  -- Finally we can fill in the etm map, joining on the row number.
  UPDATE etm_map AS e
  SET new_etm_id = n.new_etm_id
  FROM new_etm_ids_with_rn AS n
  WHERE e.rn = n.rn;

  -- Then return just the old -> new mapping in a table.
  RETURN QUERY SELECT
    e.old_etm_id,
    e.new_etm_id
  FROM etm_map e;

END;
$$;