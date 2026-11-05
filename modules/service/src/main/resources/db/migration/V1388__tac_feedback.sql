-- TAC (Time Allocation Committee) feedback, one row per program and partner.

CREATE TYPE e_tac_partner_decision AS ENUM ('accepted', 'rejected');

CREATE TABLE t_tac_feedback (
  c_program_id       d_program_id           NOT NULL REFERENCES t_program(c_program_id) ON DELETE CASCADE,
  c_partner          d_tag                  NOT NULL REFERENCES t_partner(c_tag),
  c_decision         e_tac_partner_decision NOT NULL,
  c_ranking          numeric                NOT NULL CHECK (c_ranking > 0),
  c_comments         text                   NULL     CHECK (c_comments IS NULL OR c_comments <> ''),
  PRIMARY KEY (c_program_id, c_partner)
);

-- The time each partner TAC recommends, at most one entry per science band.
CREATE TABLE t_tac_recommended_time (
  c_program_id       d_program_id   NOT NULL,
  c_partner          d_tag          NOT NULL,
  c_science_band     e_science_band NOT NULL,
  c_recommended_time interval       NOT NULL CHECK (c_recommended_time >= interval '0'),
  PRIMARY KEY (c_program_id, c_partner, c_science_band),
  FOREIGN KEY (c_program_id, c_partner)
    REFERENCES t_tac_feedback (c_program_id, c_partner) ON DELETE CASCADE
);

-- Changes to TAC feedback, including its recommended times, are program edits.
CREATE OR REPLACE FUNCTION ch_tac_feedback_edit()
  RETURNS trigger AS $$
DECLARE
  feedback record;
BEGIN
  feedback := COALESCE(NEW, OLD);
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_program_edit', feedback.c_program_id || ',' || 'UPDATE');
  END IF;
  RETURN feedback;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_tac_feedback_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_tac_feedback
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_tac_feedback_edit();

CREATE CONSTRAINT TRIGGER ch_program_edit_tac_recommended_time_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_tac_recommended_time
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_tac_feedback_edit();
