-- Notify of changes to the proposal in the program edit subscription channel
CREATE OR REPLACE FUNCTION ch_proposal_edit()
  RETURNS trigger AS $$
DECLARE
  proposal record;
BEGIN
  proposal := COALESCE(NEW, OLD);
  -- Since this is a programEdit subscription, it is always an UPDATE to the program
  PERFORM pg_notify('ch_program_edit', proposal.c_program_id || ',' || 'UPDATE');
  RETURN proposal;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_proposal_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_proposal
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_proposal_edit();

-- Notify of changes to the partner splits in the program edit subscription channel
CREATE OR REPLACE FUNCTION ch_partner_splits_edit()
  RETURNS trigger AS $$
DECLARE
  splits record;
BEGIN
  splits := COALESCE(NEW, OLD);
  -- Since this is a programEdit subscription, it is always an UPDATE to the program
  PERFORM pg_notify('ch_program_edit', splits.c_program_id || ',' || 'UPDATE');
  RETURN splits;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_partner_split_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_partner_split
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_partner_splits_edit();

-- Notify of changes to the program users in the program edit subscription channel
CREATE OR REPLACE FUNCTION ch_program_user_edit()
  RETURNS trigger AS $$
DECLARE
  proguser record;
BEGIN
  proguser := COALESCE(NEW, OLD);
  -- Since this is a programEdit subscription, it is always an UPDATE to the program
  PERFORM pg_notify('ch_program_edit', proguser.c_program_id || ',' || 'UPDATE');
  RETURN proguser;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_program_user_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_program_user
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_program_user_edit();

-- Notify of changes to the user invitations in the program edit subscription channel
CREATE OR REPLACE FUNCTION ch_invitation_edit()
  RETURNS trigger AS $$
DECLARE
  invitation record;
BEGIN
  invitation := COALESCE(NEW, OLD);
  -- Since this is a programEdit subscription, it is always an UPDATE to the program
  PERFORM pg_notify('ch_program_edit', invitation.c_program_id || ',' || 'UPDATE');
  RETURN invitation;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_invitation_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_invitation
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_invitation_edit();
