-- Add the new external role.
ALTER TYPE e_program_user_role ADD VALUE 'external' BEFORE 'pi';

-- Add the data access flag.
ALTER TABLE t_program_user
  ADD COLUMN c_has_data_access boolean;

-- Set the data access flag to true for all existing PIs, COIs and COI_ROs.
UPDATE t_program_user
  SET c_has_data_access = (c_role IN ( 'pi', 'coi', 'coi_ro' ));

-- Initialize the data access flag for new rows using a trigger.  Support users
-- will default to no data access, PIs, and COIs default to having access.
CREATE FUNCTION set_default_data_access()
RETURNS TRIGGER AS $$
BEGIN
  NEW.c_has_data_access := NEW.c_role IN ( 'pi', 'coi', 'coi_ro', 'external' );
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_default_data_access_trigger
BEFORE INSERT on t_program_user
FOR EACH ROW EXECUTE FUNCTION set_default_data_access();

-- Make the data access column not nullable.
ALTER TABLE t_program_user
  ALTER COLUMN c_has_data_access SET NOT NULL;