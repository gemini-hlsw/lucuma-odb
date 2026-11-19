-- Versions of the time accounting rules.  Time accounting only implements the
-- current rules (`TimeAccounting.version.Current` in Scala, which a startup
-- check compares with the latest version here).  Each visit records the
-- version that produced its stored charge, and a visit on an older version is
-- never recomputed: its stored charge is frozen.
--
-- When the rules change, the migration that introduces the new version calls
-- `add_time_accounting_version` (below), which moves every visit whose charge
-- isn't yet locked (see `visit_time_accounting_locked`) to the new version and
-- invalidates it so that it is recomputed under the new rules.  Locked visits
-- keep the charge they have.  Staff can move an observation or a single visit
-- to the current version with the `unfreezeTimeAccounting` mutation.

CREATE TABLE t_time_accounting_version (
  c_version     smallint PRIMARY KEY CHECK (c_version >= 1),
  c_description text     NOT NULL
);

COMMENT ON TABLE t_time_accounting_version IS
  'Versions of the time accounting rules.  The latest is the one the service implements.';

INSERT INTO t_time_accounting_version (c_version, c_description) VALUES (
  1,
  'Time accounting rules in place before versions were recorded.  Only the atoms with failing datasets are discounted, so time between atoms (e.g., while the sequence is paused) is charged even when every dataset fails QA.'
);

-- The latest version of the time accounting rules.
CREATE FUNCTION current_time_accounting_version()
  RETURNS smallint
  LANGUAGE sql
  STABLE
AS $$
  SELECT max(c_version) FROM t_time_accounting_version
$$;

ALTER TABLE t_visit
  ADD COLUMN c_ta_version smallint NOT NULL DEFAULT 1
    REFERENCES t_time_accounting_version (c_version);

ALTER TABLE t_visit
  ALTER COLUMN c_ta_version SET DEFAULT current_time_accounting_version();

COMMENT ON COLUMN t_visit.c_ta_version IS
  'The version of the time accounting rules that produced the stored charge.  Visits on an older version than the current one are not recomputed.';

-- The end of the observing night that contains `t`, at `site`.  As in
-- lucuma-core's ObservingNight, a night runs from 14:00 local time to 14:00
-- local time the next day.  Times are UTC timestamps, as stored everywhere in
-- the database.
CREATE FUNCTION observing_night_end(site e_site, t timestamp)
  RETURNS timestamp
  LANGUAGE sql
  STABLE
  PARALLEL SAFE
AS $$
  SELECT (
           (
             date_trunc('day', ((t AT TIME ZONE 'UTC') AT TIME ZONE z.tz) - interval '14 hours')
               + interval '1 day 14 hours'
           ) AT TIME ZONE z.tz
         ) AT TIME ZONE 'UTC'
    FROM (
      SELECT CASE site
               WHEN 'gn' THEN 'Pacific/Honolulu'
               WHEN 'gs' THEN 'America/Santiago'
             END AS tz
    ) z
$$;

-- Whether a visit's time accounting is locked as of `as_of`, so that a change
-- to the rules leaves its stored charge alone.  As in `v_observation_qa`,
-- acquisition datasets and datasets that were never written are not expected
-- to be assessed, so they don't keep a visit open.
CREATE FUNCTION visit_time_accounting_locked(visit d_visit_id, as_of timestamp)
  RETURNS boolean
  LANGUAGE sql
  STABLE
AS $$
  SELECT
    -- Staff have corrected the time charge.  Recomputing it under different
    -- rules could count the correction twice.
    EXISTS (
      SELECT 1
        FROM t_time_charge_correction c
       WHERE c.c_visit_id = v.c_visit_id
    )
    OR (
      -- The night is over, and either
      as_of >= n.night_end
      AND (
        -- a week has passed
        as_of >= n.night_end + interval '7 days'
        -- or no written science dataset is waiting for QA.
        OR NOT EXISTS (
          SELECT 1
            FROM t_dataset d
            JOIN t_step s ON s.c_step_id = d.c_step_id
            JOIN t_atom a ON a.c_atom_id = s.c_atom_id
           WHERE d.c_visit_id      = v.c_visit_id
             AND d.c_end_time      IS NOT NULL
             AND d.c_qa_state      IS NULL
             AND a.c_sequence_type = 'science'
        )
      )
    )
    FROM t_visit v
    CROSS JOIN LATERAL (
      SELECT observing_night_end(v.c_site, v.c_effective_time) AS night_end
    ) n
   WHERE v.c_visit_id = visit
$$;

-- Adds a new version of the time accounting rules, which must follow the
-- current one.  Moves every visit that wasn't frozen when the upgrade started
-- (it is on the version being replaced) and is either awaiting a recompute or
-- not locked, then invalidates it so that the obscalc worker recomputes it
-- under the new rules.
--
--   * Visits that were already frozen stay frozen: a change to the rules
--     shouldn't unfreeze them (unfreezeTimeAccounting does that explicitly).
--   * A visit awaiting a recompute (invalidated after its last time accounting
--     update) moves even if it is locked: the code for the old rules is gone,
--     so leaving it behind would silently drop the change that invalidated it.
--
-- New visits get the new version through the column default.  Call it from
-- the migration that accompanies the rule change, with the same version as
-- `TimeAccounting.version.Current`.
--
-- It updates t_visit before invalidating obscalc and takes no observation
-- execution locks, the reverse of invalidate_visit_time_accounting's order.
-- That is safe because migrations run as the ODB service starts, before it
-- accepts requests, and obscalc waits for them to finish, so nothing else is
-- writing.  Run against a live database, it could deadlock with execution
-- events; Postgres then aborts one side, and rerunning is the remedy.
CREATE PROCEDURE add_time_accounting_version(
  version     smallint,
  description text
) LANGUAGE plpgsql AS $$
DECLARE
  obs_ids d_observation_id[];
BEGIN
  IF version IS DISTINCT FROM current_time_accounting_version() + 1 THEN
    RAISE EXCEPTION 'Time accounting version % does not follow the current version %.',
      version, current_time_accounting_version();
  END IF;

  INSERT INTO t_time_accounting_version (c_version, c_description)
    VALUES (version, description);

  WITH upgraded AS (
    UPDATE t_visit
       SET c_ta_version      = version,
           c_ta_invalidation = now()
     -- Not frozen when the upgrade started: on the version being replaced,
     -- which the check above guarantees is `version - 1`.
     -- (current_time_accounting_version() already returns the new one, since
     -- its row was inserted above.)
     WHERE c_ta_version = version - 1
       AND (
         c_ta_invalidation > c_ta_update
         OR NOT visit_time_accounting_locked(c_visit_id, (now() AT TIME ZONE 'UTC'))
       )
    RETURNING c_observation_id
  )
  SELECT array_agg(DISTINCT c_observation_id) INTO obs_ids
    FROM upgraded;

  IF obs_ids IS NOT NULL THEN
    CALL invalidate_obscalc_many(obs_ids);
  END IF;
END;
$$;

CALL add_time_accounting_version(
  2::smallint,
  'A visit in which no science dataset passes QA is not charged at all: what remains after the per-atom QA discounts (e.g., time between atoms while the sequence was paused) is discounted too.'
);
