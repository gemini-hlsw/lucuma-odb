-- invalidate obscalc for visitors

CREATE TRIGGER visitor_invalidate_trigger
  AFTER INSERT OR UPDATE OR DELETE ON t_visitor
  FOR EACH ROW EXECUTE FUNCTION obsid_obscalc_invalidate();
