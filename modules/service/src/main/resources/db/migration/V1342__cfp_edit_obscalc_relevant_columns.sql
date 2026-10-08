-- Any edit to a t_cfp row, even one to the title alone, invalidated the obscalc
-- results of every observation in every program assigned to the call, sending
-- all of them back through the obscalc workers.  Only invalidate when a column
-- that an obscalc result can depend on changes:
--
--   * c_observatory, the coordinate limits, and the Keck and Subaru instrument
--     lists, read by the workflow validations
--     (ObservationValidationInfo.Statements.CfpInfos)
--   * c_active_start and c_active_end, from which the validations take the
--     call's midpoint and v_observation derives c_reference_time
--
-- The Gemini instruments live in t_gemini_cfp_instrument, which has its own
-- triggers (V1341).

CREATE OR REPLACE FUNCTION cfp_edit_obscalc_invalidate()
  RETURNS TRIGGER AS $$
BEGIN
  CALL invalidate_all_obscalc_for_cfp(NEW.c_cfp_id);
  RETURN NULL;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER cfp_edit_invalidate_obscalc_trigger ON t_cfp;

CREATE TRIGGER cfp_edit_invalidate_obscalc_trigger
  AFTER UPDATE ON t_cfp
  FOR EACH ROW
  WHEN (
    (
      OLD.c_observatory,
      OLD.c_north_ra_start, OLD.c_north_ra_end, OLD.c_north_dec_start, OLD.c_north_dec_end,
      OLD.c_south_ra_start, OLD.c_south_ra_end, OLD.c_south_dec_start, OLD.c_south_dec_end,
      OLD.c_active_start,   OLD.c_active_end,
      OLD.c_keck_instruments, OLD.c_subaru_instruments
    ) IS DISTINCT FROM (
      NEW.c_observatory,
      NEW.c_north_ra_start, NEW.c_north_ra_end, NEW.c_north_dec_start, NEW.c_north_dec_end,
      NEW.c_south_ra_start, NEW.c_south_ra_end, NEW.c_south_dec_start, NEW.c_south_dec_end,
      NEW.c_active_start,   NEW.c_active_end,
      NEW.c_keck_instruments, NEW.c_subaru_instruments
    )
  )
  EXECUTE FUNCTION cfp_edit_obscalc_invalidate();
