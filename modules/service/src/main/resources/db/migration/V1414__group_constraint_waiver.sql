-- Group constraint waivers: acceptance of one violation of a group's
-- constraints, say because the PI judged data taken at the wrong time usable.
-- The waived violation no longer counts against the group, while the
-- constraint stays as written, so any other violation is still caught.
--
-- A violation is identified by its code and where it happened: the element
-- out of order (ORDER), the element a gap follows (MINIMUM_INTERVAL and
-- MAXIMUM_INTERVAL), or the night (SAME_NIGHT and the per-night minimum and
-- maximum).  The other codes can't be waived.
--
-- A revoked waiver is kept, with who revoked it, when, and why if they said,
-- but no longer applies.  A waiver whose element has left the group, or whose
-- violation no longer happens, simply finds nothing to waive.

-- Every violation code, matching lucuma.core.enums.GroupConstraintViolationCode,
-- though only the waivable ones are stored.
CREATE TYPE e_group_constraint_violation_code AS ENUM (
  'order',
  'minimum_interval',
  'maximum_interval',
  'same_night',
  'same_night_multiple_sites',
  'observations_per_night_minimum',
  'observations_per_night_maximum',
  'observations_per_night_multiple_sites',
  'insufficient_elements',
  'subgroup_unschedulable'
);

CREATE DOMAIN d_group_constraint_waiver_id AS varchar
  CHECK (VALUE ~ '^w-[1-9a-f][0-9a-f]*$');
COMMENT ON DOMAIN d_group_constraint_waiver_id IS 'GID type for group constraint waivers.';

CREATE SEQUENCE s_group_constraint_waiver_id START WITH 256;

-- Every waiver granted, one row each, kept once revoked.  Group progress is
-- evaluated with the active ones (c_revoked_at null).
CREATE TABLE t_group_constraint_waiver (
  c_waiver_id              d_group_constraint_waiver_id      PRIMARY KEY DEFAULT 'w-' || to_hex(nextval('s_group_constraint_waiver_id')),
  c_group_id               d_group_id                        NOT NULL REFERENCES t_group(c_group_id) ON DELETE CASCADE,

  -- The violation: its code, and its element (a group or an observation) or
  -- its night (a site and the local date on which the night ends).
  c_code                   e_group_constraint_violation_code NOT NULL,
  c_element_group_id       d_group_id                        NULL REFERENCES t_group(c_group_id) ON DELETE CASCADE,
  c_element_observation_id d_observation_id                  NULL REFERENCES t_observation(c_observation_id) ON DELETE CASCADE,
  c_night_site             e_site                            NULL,
  c_night_date             date                              NULL,

  c_granted_by             d_user_id                         NOT NULL REFERENCES t_user(c_user_id) DEFAULT current_setting('lucuma.user', true),
  c_granted_at             timestamp                         NOT NULL DEFAULT now(),
  c_granted_reason         text                              NULL CHECK (length(c_granted_reason) > 0),

  c_revoked_by             d_user_id                         NULL REFERENCES t_user(c_user_id),
  c_revoked_at             timestamp                         NULL,
  c_revoked_reason         text                              NULL CHECK (length(c_revoked_reason) > 0),

  -- Each waivable code has exactly the location it needs, and the others
  -- can't be stored.
  CONSTRAINT group_constraint_waiver_location_check CHECK (
    CASE c_code
      WHEN 'order'            THEN num_nonnulls(c_element_group_id, c_element_observation_id) = 1 AND num_nonnulls(c_night_site, c_night_date) = 0
      WHEN 'minimum_interval' THEN num_nonnulls(c_element_group_id, c_element_observation_id) = 1 AND num_nonnulls(c_night_site, c_night_date) = 0
      WHEN 'maximum_interval' THEN num_nonnulls(c_element_group_id, c_element_observation_id) = 1 AND num_nonnulls(c_night_site, c_night_date) = 0
      WHEN 'same_night'                     THEN num_nonnulls(c_element_group_id, c_element_observation_id) = 0 AND num_nonnulls(c_night_site, c_night_date) = 2
      WHEN 'observations_per_night_minimum' THEN num_nonnulls(c_element_group_id, c_element_observation_id) = 0 AND num_nonnulls(c_night_site, c_night_date) = 2
      WHEN 'observations_per_night_maximum' THEN num_nonnulls(c_element_group_id, c_element_observation_id) = 0 AND num_nonnulls(c_night_site, c_night_date) = 2
      ELSE false
    END
  ),

  -- A revocation has who and when together, and a reason only with them.
  CONSTRAINT group_constraint_waiver_revoked_check CHECK (
    (c_revoked_by IS NULL) = (c_revoked_at IS NULL) AND
    (c_revoked_reason IS NULL OR c_revoked_at IS NOT NULL)
  ),

  CONSTRAINT group_constraint_waiver_revoked_after_check CHECK (
    c_revoked_at IS NULL OR c_revoked_at >= c_granted_at
  )
);

COMMENT ON TABLE t_group_constraint_waiver IS
  'Group constraint waivers: each accepts one violation of a group''s constraints (its code and element or night), so that it no longer counts against the group.  Revoked waivers are kept; group progress is evaluated with the active ones (c_revoked_at null).';

CREATE INDEX i_group_constraint_waiver_group ON t_group_constraint_waiver (c_group_id);

-- At most one active waiver per violation.  A revoked one may be granted again.
CREATE UNIQUE INDEX i_group_constraint_waiver_active
  ON t_group_constraint_waiver (c_group_id, c_code, c_element_group_id, c_element_observation_id, c_night_site, c_night_date)
  NULLS NOT DISTINCT
  WHERE c_revoked_at IS NULL;

-- Granting or revoking a waiver can change the group's progress, so it marks
-- the group's tree for the group standing daemon.  During a cascade from the
-- group's deletion the group is gone and nothing is marked; the group's own
-- trigger has marked its tree.
CREATE FUNCTION group_standing_waiver_mark()
  RETURNS TRIGGER AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    CALL mark_group_standing(OLD.c_group_id);
  ELSE
    CALL mark_group_standing(NEW.c_group_id);
  END IF;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER group_standing_waiver_mark_trigger
  AFTER INSERT OR DELETE OR UPDATE OF c_revoked_at ON t_group_constraint_waiver
  FOR EACH ROW
  EXECUTE FUNCTION group_standing_waiver_mark();

-- Granting or revoking a waiver edits its group, for group edit subscribers.
CREATE FUNCTION ch_group_constraint_waiver_edit()
  RETURNS TRIGGER AS $$
DECLARE
  w RECORD;
BEGIN
  IF TG_OP = 'DELETE' THEN w := OLD; ELSE w := NEW; END IF;
  PERFORM pg_notify('ch_group_edit', g.c_group_id || ',' || g.c_program_id || ',' || 'UPDATE')
     FROM t_group g
    WHERE g.c_group_id = w.c_group_id;
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_group_constraint_waiver_edit_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_group_constraint_waiver
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_group_constraint_waiver_edit();

-- Waivers as Grackle maps them: synthetic ids, null when absent, for the
-- nullable embedded element, night and revocation, whether the waiver is
-- active (for filtering), and an always-null subgroup, which only an
-- unwaivable violation has.
CREATE VIEW v_group_constraint_waiver AS
  SELECT
    w.*,
    CASE WHEN num_nonnulls(w.c_element_group_id, w.c_element_observation_id) > 0 THEN w.c_waiver_id END AS c_element_id,
    CASE WHEN w.c_night_site IS NOT NULL THEN w.c_waiver_id END AS c_night_id,
    CASE WHEN w.c_revoked_at IS NOT NULL THEN w.c_waiver_id END AS c_revoked_id,
    w.c_revoked_at IS NULL AS c_active,
    NULL :: d_group_id AS c_subgroup_id
  FROM t_group_constraint_waiver w;
