CREATE DOMAIN d_program_note_id AS varchar
  CHECK (VALUE ~ '^n-[1-9a-f][0-9a-f]*$');
COMMENT ON DOMAIN d_program_note_id IS 'GID type for program notes.';

CREATE SEQUENCE s_program_note_id START WITH 256;

CREATE TABLE t_program_note (

  c_program_note_id d_program_note_id PRIMARY KEY DEFAULT 'n-' || to_hex(nextval('s_program_note_id')),
  c_program_id      d_program_id      NOT NULL REFERENCES t_program(c_program_id) ON DELETE CASCADE,
  c_existence       e_existence       NOT NULL DEFAULT 'present',

  c_title           text              NOT NULL CHECK (length(c_title) > 0),
  c_text            text              CHECK (c_text IS NULL OR length(c_text) > 0),
  c_private         boolean           NOT NULL DEFAULT false

);

-- Trigger function, same as ch_program_edit but maybe it's better for each
-- trigger to have its own function?
CREATE OR REPLACE FUNCTION ch_program_edit_note()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*) THEN
    PERFORM pg_notify('ch_program_edit',  NEW.c_program_id || ',' || TG_OP);
  END IF;
  RETURN NEW; -- n.b. doesn't matter, it's an AFTER trigger
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_note_trigger
  AFTER INSERT OR UPDATE ON t_program_note
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_program_edit_note();