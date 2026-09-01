-- A soft-deleted system group is invisible but still has a name under
-- unique_system_group_name, so regenerating a per-observation calibration
-- group for the same science observation failed with a unique violation.
--
-- Two-part fix: enforce name uniqueness only among present system groups, and
-- forbid soft-deleting system groups going forward -- the service manages
-- their lifecycle and always hard-deletes them. 
-- 
-- Existing soft-deleted rows
-- are left in place (their deleted observations still reference them); the
-- narrowed index makes them harmless.

DROP INDEX unique_system_group_name;

CREATE UNIQUE INDEX unique_system_group_name
  ON t_group (c_program_id, c_name)
  WHERE c_system IS TRUE AND c_existence = 'present';

CREATE OR REPLACE FUNCTION check_system_group_no_soft_delete()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.c_system AND NEW.c_existence = 'deleted' THEN
    RAISE EXCEPTION 'System group % cannot be soft-deleted, it must be removed instead', NEW.c_group_id;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER system_group_no_soft_delete_trigger
  BEFORE INSERT OR UPDATE OF c_existence, c_system ON t_group
  FOR EACH ROW
  EXECUTE FUNCTION check_system_group_no_soft_delete();
