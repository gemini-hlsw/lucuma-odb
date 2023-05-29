-- Notify of attachment changes in the program subscription channel

-- Since this is a programEdit subscription, it is always an UPDATE to the program
CREATE OR REPLACE FUNCTION ch_obs_attachment_edit()
  RETURNS trigger AS $$
DECLARE
  attachment record;
BEGIN
  attachment := COALESCE(NEW, OLD);
  PERFORM pg_notify('ch_program_edit', attachment.c_program_id || ',' || 'UPDATE');
  RETURN attachment;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_obs_attachment_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_obs_attachment
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_obs_attachment_edit();

-- Since this is a programEdit subscription, it is always an UPDATE to the program
CREATE OR REPLACE FUNCTION ch_proposal_attachment_edit()
  RETURNS trigger AS $$
DECLARE
  attachment record;
BEGIN
  attachment := COALESCE(NEW, OLD);
  PERFORM pg_notify('ch_program_edit', attachment.c_program_id || ',' || 'UPDATE');
  RETURN attachment;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_proposal_attachment_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_proposal_attachment
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_proposal_attachment_edit();
