-- Replace the triggger, it needs to run on several column
DROP TRIGGER IF EXISTS maintain_f2_reads_default on t_flamingos_2_long_slit;

CREATE TRIGGER maintain_f2_reads_default
BEFORE INSERT OR UPDATE OF c_read_mode, c_read_mode_default
ON t_flamingos_2_long_slit
FOR EACH ROW EXECUTE FUNCTION update_f2_reads();
