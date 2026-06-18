-- A proposal's time request is either apportioned across Gemini partners (via
-- t_partner_split) or assigned 100% to a single exchange partner.  Store the
-- latter here; null when the request uses partner splits.
ALTER TABLE t_proposal
  ADD COLUMN c_exchange_partner e_exchange_partner NULL;

-- Recreate v_proposal so its `p.*` picks up the new column (a plain ADD COLUMN
-- does not alter the existing view, and the new column lands before the
-- appended CASE columns so CREATE OR REPLACE cannot be used).
DROP VIEW v_proposal;
CREATE VIEW v_proposal AS
  SELECT
    p.*,
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

-- Enforce the time-request invariant: a proposal cannot have both an exchange
-- partner (t_proposal.c_exchange_partner) and Gemini partner splits
-- (t_partner_split rows).  These live in different tables, so a CHECK cannot
-- express it.  Plain (immediate) row triggers raise during the offending
-- statement so the service can surface a clean error (a deferred constraint
-- trigger would instead fail at COMMIT, outside the request handler).
CREATE FUNCTION check_proposal_time_request() RETURNS trigger AS $$
DECLARE
  pid          d_program_id := COALESCE(NEW.c_program_id, OLD.c_program_id);
  has_exchange boolean;
BEGIN
  SELECT (c_exchange_partner IS NOT NULL) INTO has_exchange
    FROM t_proposal WHERE c_program_id = pid;

  IF has_exchange AND EXISTS (SELECT 1 FROM t_partner_split WHERE c_program_id = pid) THEN
    RAISE EXCEPTION 'A proposal may not have both an exchange partner and partner splits'
      USING ERRCODE = 'P0001';
  END IF;

  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_proposal_time_request_proposal
  AFTER INSERT OR UPDATE ON t_proposal
  FOR EACH ROW EXECUTE FUNCTION check_proposal_time_request();

CREATE TRIGGER check_proposal_time_request_split
  AFTER INSERT OR UPDATE ON t_partner_split
  FOR EACH ROW EXECUTE FUNCTION check_proposal_time_request();
