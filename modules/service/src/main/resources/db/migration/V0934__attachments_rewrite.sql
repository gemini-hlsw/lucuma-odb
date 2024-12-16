-- Rewriting the attachments to unify proposal and obs attachments 
-- and add custom SEDs on targets

CREATE TYPE e_attachment_type as ENUM (
  'science',
  'team',
  'finder',
  'mos_mask',
  'pre_imaging',
  'custom_sed'
);

-------
-- The attachment table
-------

-- drop the old trigger and function
DROP FUNCTION ch_obs_attachment_edit CASCADE;

ALTER TABLE t_obs_attachment
  RENAME TO t_attachment;

ALTER TABLE t_attachment
  RENAME c_obs_attachment_id TO c_attachment_id;

ALTER TABLE t_attachment
  -- attachment types are now an enum instead of a table
  DROP CONSTRAINT t_attachment_c_attachment_type_fkey,
  ALTER COLUMN c_attachment_type TYPE e_attachment_type 
    USING c_attachment_type::e_attachment_type;

-- to support a new foreign key in t_obs_attachment_assignment
ALTER TABLE t_attachment
  ADD CONSTRAINT t_attachment_unique UNIQUE (c_program_id, c_attachment_id, c_attachment_type);

-- insert the current proposal attachments
INSERT INTO t_attachment (c_program_id, c_attachment_type, c_file_name, c_description, c_checked, c_file_size, c_updated_at, c_remote_path)
  SELECT c_program_id, c_attachment_type::e_attachment_type, c_file_name, c_description, c_checked, c_file_size, c_updated_at, c_remote_path 
  FROM t_proposal_attachment;

ALTER SEQUENCE s_obs_attachment_id
  RENAME TO s_attachment_id;

ALTER DOMAIN d_obs_attachment_id
  RENAME TO c_attachment_id;

-- need partial index on obs_attachment to make sure science and team 
-- unique per program
CREATE UNIQUE INDEX unique_proposal_attachments_index
  ON t_attachment (c_program_Id, c_attachment_type)
  WHERE c_attachment_type IN ('science', 'team');

-- make attachment type immutable
CREATE OR REPLACE FUNCTION attachment_type_immutable()
  RETURNS trigger as $$
DECLARE
BEGIN
  IF NEW.c_attachment_type != OLD.c_attachment_type THEN
    RAISE EXCEPTION 'The type of an attachment cannot be changed.';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER t_attachment_immutable_type_trigger
BEFORE UPDATE on t_attachment
FOR EACH ROW
EXECUTE FUNCTION attachment_type_immutable();

-- re-create the `edit` triggers
-- sends a program_edit and any linked observation_edits.
CREATE OR REPLACE FUNCTION ch_attachment_edit()
  RETURNS trigger AS $$
DECLARE
  attachment record;
  obs_id     d_observation_id;
BEGIN
  attachment := COALESCE(NEW, OLD);
  PERFORM pg_notify('ch_program_edit', attachment.c_program_id || ',' || 'UPDATE');
  FOR obs_id in 
    select c_observation_id 
    from t_obs_attachment_assignment 
    where c_attachment_id = attachment.c_attachment_id
  LOOP
    PERFORM pg_notify('ch_observation_edit', obs_id || ',' || attachment.c_program_id  || ',' || 'UPDATE');
  END LOOP;
  RETURN attachment;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_attachment_edit_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_attachment
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_attachment_edit();

-------
-- Assigning observation type attachments to observations
-------
ALTER TABLE t_obs_attachment_assignment
  RENAME c_obs_attachment_id TO c_attachment_id;

-- need to make sure only observation attachments can be assigned to
-- t_obs_attachment_assignment. So, we'll denormalize to include the 
-- attachment type and change the foreign key constraint and add a 
-- constraint so only the 'observation types' can be referenced.
ALTER TABLE t_obs_attachment_assignment
  ADD COLUMN c_attachment_type e_attachment_type;

UPDATE t_obs_attachment_assignment o
  SET c_attachment_type = a.c_attachment_type
  FROM t_attachment a
  WHERE o.c_attachment_id = a.c_attachment_id;

ALTER TABLE t_obs_attachment_assignment
ALTER COLUMN c_attachment_type SET NOT NULL;

ALTER TABLE t_obs_attachment_assignment
  DROP CONSTRAINT t_obs_attachment_assignment_c_program_id_c_obs_attachment__fkey,
  ADD CONSTRAINT t_obs_attachment_assignment_t_assignment__fkey
    FOREIGN KEY (c_program_id, c_attachment_id, c_attachment_type)
    REFERENCES t_attachment(c_program_id, c_attachment_id, c_attachment_type)
    ON DELETE CASCADE,
  ADD CONSTRAINT only_obs_attachments_check CHECK (c_attachment_type IN ('finder', 'mos_mask', 'pre_imaging'));

-- drop unused tables, etc.
DROP FUNCTION ch_proposal_attachment_edit CASCADE;

DROP table t_obs_attachment_file_ext;
DROP TABLE t_obs_attachment_type;
DROP TABLE t_proposal_attachment;
DROP TABLE t_proposal_attachment_type;
