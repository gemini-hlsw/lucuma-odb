-- TAC (Time Allocation Committee) feedback, one row per program and partner.

CREATE TYPE e_tac_partner_decision AS ENUM ('accepted', 'rejected');

CREATE TABLE t_tac_feedback (
  c_program_id       d_program_id           NOT NULL REFERENCES t_program(c_program_id) ON DELETE CASCADE,
  c_partner          d_tag                  NOT NULL REFERENCES t_partner(c_tag),
  c_decision         e_tac_partner_decision NOT NULL,
  c_ranking          numeric                NOT NULL CHECK (c_ranking > 0),
  c_recommended_time interval               NOT NULL CHECK (c_recommended_time >= interval '0'),
  c_comments         text                   NULL     CHECK (c_comments IS NULL OR c_comments <> ''),
  PRIMARY KEY (c_program_id, c_partner)
);

-- Changes to TAC feedback are program edits.
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
