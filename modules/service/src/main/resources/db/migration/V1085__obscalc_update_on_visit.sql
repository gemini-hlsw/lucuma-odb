CREATE TRIGGER visit_invalidate_trigger
AFTER INSERT OR UPDATE OR DELETE ON t_visit
FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();