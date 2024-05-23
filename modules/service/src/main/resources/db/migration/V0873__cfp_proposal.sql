-- We'll deal with intensive and exchange "later"
DELETE FROM t_proposal
  WHERE c_class = 'intensive' OR
        c_class = 'exchange';

ALTER TABLE t_proposal
  DROP CONSTRAINT t_proposal_check_min_total,
  DROP CONSTRAINT t_proposal_check_total;

-- This trigger would update the science subtype based on the proposal class.
-- The proposal class is being dropped in favor of the call for proposals type.
DROP TRIGGER update_science_subtype_trigger ON t_proposal;
DROP FUNCTION update_science_subtype;

ALTER TABLE t_proposal
  DROP COLUMN c_class,
   ADD COLUMN c_science_subtype e_science_subtype,
   ADD COLUMN c_cfp_id d_cfp_id REFERENCES t_cfp(c_cfp_id);

-- Assume 'queue' until told otherwise.
UPDATE t_program AS p
   SET c_science_subtype = 'queue'
  FROM t_proposal r
 WHERE p.c_program_id = r.c_program_id AND p.c_science_subtype IS NULL;

-- Set the proposal science subtype to match the program's value.
UPDATE t_proposal AS s
   SET c_science_subtype = g.c_science_subtype
  FROM t_program g
 WHERE s.c_program_id = g.c_program_id;

CREATE OR REPLACE FUNCTION t_proposal_science_subtype_checks()
RETURNS TRIGGER AS $$
BEGIN
  IF (NEW.c_science_subtype IS NULL) THEN
    RAISE EXCEPTION 'All proposals must be associated with a science subypte.';
  END IF;

  IF (NEW.c_science_subtype != 'large_program') AND (NEW.c_min_percent_total IS NOT NULL) THEN
    RAISE EXCEPTION 'Only Large Programs have a min percent total';
  END IF;

  IF (NEW.c_science_subtype != 'large_program') AND (NEW.c_total_time IS NOT NULL) THEN
    RAISE EXCEPTION 'Only Large Programs have a total time';
  END IF;

  IF (NEW.c_science_subtype = 'classical') AND (NEW.c_too_activation != 'none') THEN
    RAISE EXCEPTION 'Classical proposals must set TOO activation to None';
  END IF;

  IF (NEW.c_science_subtype = 'large_program') AND (NEW.c_min_percent_total IS NULL) THEN
    RAISE EXCEPTION 'Large Program proposals must define the min percent total.';
  END IF;

  IF (NEW.c_science_subtype = 'large_program') AND (NEW.c_total_time IS NULL) THEN
    RAISE EXCEPTION 'Large Program proposals must define the total time.';
  END IF;

  IF (NEW.c_science_subtype = 'poor_weather') AND (NEW.c_too_activation != 'none') THEN
    RAISE EXCEPTION 'Poor Weather proposals must set TOO activation to None';
  END IF;

  IF (NEW.c_science_subtype = 'poor_weather') AND (NEW.c_min_percent != 0) THEN
    RAISE EXCEPTION 'Poor Weather proposals must set min percent to 0';
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_proposal_science_subtype
AFTER INSERT OR UPDATE OF c_science_subtype ON t_proposal
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW
EXECUTE FUNCTION t_proposal_science_subtype_checks();

DROP TABLE t_proposal_class;

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
    CASE WHEN p.c_science_subtype = 'system_verification' THEN c_program_id END AS c_program_id_v,
    CASE WHEN p.c_science_subtype = 'fast_turnaround' THEN
      (SELECT
        CASE WHEN COUNT(*) = 1 THEN MIN(ps.c_partner::d_tag) ELSE NULL::d_tag END
        FROM t_partner_split ps
       WHERE ps.c_program_id = p.c_program_id)
    END AS c_ft_partner
  FROM
    t_proposal p;