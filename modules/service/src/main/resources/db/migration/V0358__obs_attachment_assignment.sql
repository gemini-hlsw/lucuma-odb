-- A join table for observations and obs_attachments.  Since you
-- cannot simply join any observation with any attachment (they must reference the
-- same program), it includes a shared program id column.
create table t_obs_attachment_assignment (
  c_program_Id          d_program_id          not null,
  c_observation_id      d_observation_id      not null,
  c_obs_attachment_id   d_obs_attachment_id   not null,

  foreign key (c_program_id, c_observation_id) references t_observation(c_program_id, c_observation_id),
  foreign key (c_program_id, c_obs_attachment_id) references t_obs_attachment(c_program_id, c_obs_attachment_id) 
    ON DELETE CASCADE,
  constraint t_obs_attachment_assignment_pkey primary key (c_program_Id, c_observation_id, c_obs_attachment_id)
);

-- Notify of obs attachment assignment changes in both the program and observation subscription channels.
-- Since it is an observationEdit subscription, it is always an UPDATE to the observation.
CREATE OR REPLACE FUNCTION ch_obs_attachment_assignment_edit()
  RETURNS trigger AS $$
DECLARE
  assignment record;
BEGIN
  assignment := COALESCE(NEW, OLD);
  PERFORM pg_notify('ch_observation_edit', assignment.c_observation_id || ',' || assignment.c_program_id || ',' || 'UPDATE');
  RETURN assignment;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_obs_attachment_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_obs_attachment_assignment
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_obs_attachment_assignment_edit();

-- Update the update handler for obsAttachments to also notify any assigned observations
-- Since this is an attachment change, it is always an UPDATE to the program or observation
CREATE OR REPLACE FUNCTION ch_obs_attachment_edit()
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
    where c_obs_attachment_id = attachment.c_obs_attachment_id
  LOOP
    PERFORM pg_notify('ch_observation_edit', obs_id || ',' || attachment.c_program_id  || ',' || 'UPDATE');
  END LOOP;
  RETURN attachment;
END;
$$ LANGUAGE plpgsql;
