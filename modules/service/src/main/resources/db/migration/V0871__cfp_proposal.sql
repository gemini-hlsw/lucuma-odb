DELETE FROM t_proposal
  WHERE c_class = 'intensive' OR
        c_class = 'exchange';

ALTER TABLE t_proposal
  DROP CONSTRAINT t_proposal_check_min_total,
  DROP CONSTRAINT t_proposal_check_total;

DELETE FROM t_proposal_class
  WHERE c_tag = 'intensive' OR
        c_tag = 'exchange';

DROP TRIGGER update_science_subtype_trigger ON t_proposal;

DROP FUNCTION update_science_subtype;

--    "t_proposal_c_abstract_check" CHECK (c_abstract IS NULL OR length(c_abstract) > 0)
--    "t_proposal_c_title_check" CHECK (c_title IS NULL OR length(c_title) > 0)
--    "t_proposal_check_min_total" CHECK ((c_min_percent_total IS NOT NULL) = (c_class::text = 'intensive'::text OR c_class::text = 'large_program'::text))
--    "t_proposal_check_total" CHECK ((c_total_time IS NOT NULL) = (c_class::text = 'intensive'::text OR c_class::text = 'large_program'::text))

--    "t_proposal_c_category_fkey" FOREIGN KEY (c_category) REFERENCES t_tac_category(c_tag)
--    "t_proposal_c_class_fkey" FOREIGN KEY (c_class) REFERENCES t_proposal_class(c_tag)
--    "t_proposal_c_program_id_fkey" FOREIGN KEY (c_program_id) REFERENCES t_program(c_program_id) ON DELETE CASCADE

--    ch_program_edit_proposal_trigger AFTER INSERT OR DELETE OR UPDATE ON t_proposal DEFERRABLE INITIALLY IMMEDIATE FOR EACH ROW EXECUTE FUNCTION ch_proposal_edit()
--    reset_proposal_status_trigger AFTER DELETE ON t_proposal FOR EACH ROW EXECUTE FUNCTION reset_proposal_status()
--    update_science_subtype_trigger BEFORE INSERT OR UPDATE ON t_proposal FOR EACH ROW EXECUTE FUNCTION update_science_subtype()
