-- TAC feedback is now given per time accounting category rather than per
-- partner, and the recommended times live in t_allocation alongside the
-- allocated durations.

-- The time recommended by the TAC for the category and band, if any.  A
-- recommendation is always for some time.
ALTER TABLE t_allocation
  ADD COLUMN c_recommended_time interval NULL
    CHECK (c_recommended_time IS NULL OR c_recommended_time > interval '0');

-- Every partner has the same tag as its time accounting category.  A new
-- allocation starts with the recommended time as its duration, while an
-- existing allocation keeps its duration.  A recommendation of no time carries
-- nothing worth keeping, and is dropped.
INSERT INTO t_allocation (c_program_id, c_ta_category, c_science_band, c_duration, c_recommended_time)
SELECT c_program_id, c_partner, c_science_band, c_recommended_time, c_recommended_time
  FROM t_tac_recommended_time
 WHERE c_recommended_time > interval '0'
ON CONFLICT (c_program_id, c_ta_category, c_science_band)
  DO UPDATE SET c_recommended_time = EXCLUDED.c_recommended_time;

DROP TABLE t_tac_recommended_time;

ALTER TABLE t_tac_feedback
  DROP CONSTRAINT t_tac_feedback_c_partner_fkey;

ALTER TABLE t_tac_feedback
  RENAME COLUMN c_partner TO c_ta_category;

ALTER TABLE t_tac_feedback
  ADD CONSTRAINT t_tac_feedback_c_ta_category_fkey
    FOREIGN KEY (c_ta_category) REFERENCES t_time_accounting_category(c_tag);

-- The recommended times, which are the allocations that carry one.
CREATE VIEW v_tac_recommended_time AS
  SELECT c_program_id,
         c_ta_category,
         c_science_band,
         c_recommended_time
    FROM t_allocation
   WHERE c_recommended_time IS NOT NULL;
