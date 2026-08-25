-- The instruments an AEON/multi-facility proposal declares as required.
-- An entry is only valid while a backing observation uses the instrument and is pruned the
-- moment the last one goes away.
CREATE TABLE t_proposal_aeon_required_instrument (
  c_program_id d_program_id NOT NULL REFERENCES t_proposal(c_program_id) ON DELETE CASCADE,
  c_instrument d_tag        NOT NULL REFERENCES t_instrument(c_tag),
  PRIMARY KEY (c_program_id, c_instrument)
);

-- Whether an observation backs an AEON required instrument: present, not
-- user-deactivated, not a calibration, and its observing mode maps to an
-- instrument.
CREATE FUNCTION is_aeon_backing_observation(o t_observation)
RETURNS boolean AS $$
  SELECT o.c_existence = 'present'
     AND o.c_workflow_user_state IS DISTINCT FROM 'inactive'
     AND o.c_calibration_role IS NULL
     AND o.c_instrument IS NOT NULL;
$$ LANGUAGE sql IMMUTABLE;

-- An instrument may only be marked required on an AEON/multi-facility proposal,
-- and only while a backing observation uses it.
CREATE FUNCTION check_aeon_required_instrument()
RETURNS TRIGGER AS $$
DECLARE
  backing d_observation_id;
BEGIN
  IF NOT EXISTS (
    SELECT 1
      FROM t_proposal p
     WHERE p.c_program_id = NEW.c_program_id
       AND p.c_aeon_multi_facility
  ) THEN
    RAISE EXCEPTION 'Required instruments may only be set on AEON/multi-facility proposals';
  END IF;

  SELECT o.c_observation_id
    INTO backing
    FROM t_observation o
   WHERE o.c_program_id = NEW.c_program_id
     AND o.c_instrument = NEW.c_instrument
     AND is_aeon_backing_observation(o)
   LIMIT 1
     FOR SHARE OF o;

  IF backing IS NULL THEN
    RAISE EXCEPTION 'Instrument % cannot be marked required because no active observation in the program uses it', NEW.c_instrument;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ch_aeon_required_instrument
BEFORE INSERT OR UPDATE ON t_proposal_aeon_required_instrument
FOR EACH ROW
EXECUTE FUNCTION check_aeon_required_instrument();

-- Prune entries whose last backing observation went away.
CREATE FUNCTION prune_aeon_required_instruments()
RETURNS TRIGGER AS $$
DECLARE
  pid d_program_id := COALESCE(OLD.c_program_id, NEW.c_program_id);
BEGIN
  DELETE FROM t_proposal_aeon_required_instrument r
   WHERE r.c_program_id = pid
     AND NOT EXISTS (
       SELECT 1
         FROM t_observation o
        WHERE o.c_program_id = pid
          AND o.c_instrument = r.c_instrument
          AND is_aeon_backing_observation(o)
     );
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER prune_aeon_required_instruments
AFTER DELETE OR UPDATE OF c_existence, c_workflow_user_state, c_instrument, c_calibration_role
ON t_observation
FOR EACH ROW
EXECUTE FUNCTION prune_aeon_required_instruments();

-- Leaving the AEON/multi-facility program clears the set.
-- No undo support
CREATE FUNCTION clear_aeon_required_instruments()
RETURNS TRIGGER AS $$
BEGIN
  DELETE FROM t_proposal_aeon_required_instrument
   WHERE c_program_id = NEW.c_program_id;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER clear_aeon_required_instruments
AFTER UPDATE OF c_aeon_multi_facility ON t_proposal
FOR EACH ROW
WHEN (NOT NEW.c_aeon_multi_facility)
EXECUTE FUNCTION clear_aeon_required_instruments();

-- v_proposal as in V1262, plus the aggregated required-instrument list.
DROP VIEW v_proposal;

CREATE VIEW v_proposal AS
  SELECT
    p.*,
    -- Explicit / default / effective.  Unlike the observation's execution
    -- requirement, this is a plain COALESCE: an explicit ceiling replaces the
    -- derived one outright rather than acting as a floor beneath it.  Note the
    -- default keeps tracking the observations even after acceptance, when it no
    -- longer has any effect -- see the freeze in ProposalService.
    too_activation_ceiling_default(p.c_program_id, p.c_observatory, p.c_science_subtype)
                                                                             AS c_too_activation_default,
    COALESCE(
      p.c_too_activation,
      too_activation_ceiling_default(p.c_program_id, p.c_observatory, p.c_science_subtype)
    )                                                                        AS c_too_activation_effective,
    COALESCE(
      (SELECT ARRAY_AGG(r.c_instrument ORDER BY r.c_instrument)
         FROM t_proposal_aeon_required_instrument r
        WHERE r.c_program_id = p.c_program_id),
      '{}'
    )                                                                        AS c_aeon_required_instruments,
    -- Key for the nullable explicit time request: null when no request was
    -- stated, so the GraphQL object is null rather than a zero TimeSpan.
    CASE WHEN p.c_time_request IS NOT NULL              THEN c_program_id END AS c_time_request_id,
    CASE WHEN p.c_observatory = 'gemini'               THEN c_program_id END AS c_program_id_gemini,
    CASE WHEN p.c_observatory = 'keck'                 THEN c_program_id END AS c_program_id_keck,
    CASE WHEN p.c_observatory = 'subaru'               THEN c_program_id END AS c_program_id_subaru,
    -- Non-null discriminator for the GeminiProposalType interface mapping.  Only
    -- meaningful for Gemini proposals; others get a placeholder that is never
    -- rendered (their c_program_id_gemini key is null).
    COALESCE(p.c_science_subtype, 'queue')                                   AS c_gemini_science_subtype,
    CASE WHEN p.c_science_subtype = 'classical'           THEN c_program_id END AS c_program_id_c,
    CASE WHEN p.c_science_subtype = 'demo_science'        THEN c_program_id END AS c_program_id_s,
    CASE WHEN p.c_science_subtype = 'directors_time'      THEN c_program_id END AS c_program_id_d,
    CASE WHEN p.c_science_subtype = 'fast_turnaround'     THEN c_program_id END AS c_program_id_f,
    CASE WHEN p.c_science_subtype = 'large_program'       THEN c_program_id END AS c_program_id_l,
    CASE WHEN p.c_science_subtype = 'poor_weather'        THEN c_program_id END AS c_program_id_p,
    CASE WHEN p.c_science_subtype = 'queue'               THEN c_program_id END AS c_program_id_q,
    CASE WHEN p.c_science_subtype = 'system_verification' THEN c_program_id END AS c_program_id_v
  FROM
    t_proposal p;
