CREATE DOMAIN d_program_note_id AS varchar
  CHECK (VALUE ~ '^n-[1-9a-f][0-9a-f]*$');
COMMENT ON DOMAIN d_program_note_id IS 'GID type for program notes.';

CREATE SEQUENCE s_program_note_id START WITH 256;

CREATE TABLE t_program_note (

  c_program_note_id d_program_note_id PRIMARY KEY DEFAULT 'n-' || to_hex(nextval('s_program_note_id')),
  c_program_id      d_program_id      NOT NULL REFERENCES t_program(c_program_id) ON DELETE CASCADE,
  c_existence       e_existence       NOT NULL DEFAULT 'present',
  c_index           smallint          NOT NULL,

  c_title           text              NOT NULL CHECK (length(c_title) > 0),
  c_text            text              CHECK (c_text IS NULL OR length(c_text) > 0),
  c_private         boolean           NOT NULL DEFAULT false,

  UNIQUE (c_program_id, c_index)
);