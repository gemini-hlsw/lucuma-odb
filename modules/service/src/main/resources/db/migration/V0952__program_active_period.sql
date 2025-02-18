-- Add active start / end.  The defaults seem fairly random but correspond to
-- the limiting dates for our LST calculation.

ALTER TABLE t_program
  ADD COLUMN c_active_start date NOT NULL DEFAULT '1901-01-01'::date,
  ADD COLUMN c_active_end   date NOT NULL DEFAULT '2099-12-31'::date,
  ADD CONSTRAINT active_dates_check CHECK (c_active_start < c_active_end);

-- Any existing programs should get the active period of the corresponding
-- CfP (if any).

UPDATE t_program AS g
   SET c_active_start = c.c_active_start,
       c_active_end   = c.c_active_end
  FROM t_proposal p
  JOIN t_cfp c ON p.c_cfp_id = c.c_cfp_id
 WHERE p.c_program_id = g.c_program_id;

-- When a CfP is updated, change all the related programs which were using the
-- previous period.

CREATE FUNCTION update_program_active_period_after_cfp_update()
RETURNS TRIGGER AS $$
BEGIN
  IF ((NEW.c_active_start != OLD.c_active_start) OR
      (NEW.c_active_end   != OLD.c_active_end  )) THEN

    UPDATE t_program AS g
       SET c_active_start = NEW.c_active_start,
           c_active_end   = NEW.c_active_end
      FROM t_proposal p
     WHERE p.c_cfp_id       = NEW.c_cfp_id
       AND p.c_program_id   = g.c_program_id
       AND g.c_active_start = OLD.c_active_start
       AND g.c_active_end   = OLD.c_active_end;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_program_active_period_after_cfp_update_trigger
AFTER UPDATE ON t_cfp
FOR EACH ROW EXECUTE FUNCTION update_program_active_period_after_cfp_update();

-- When a CfP is assigned, change the program active period.

CREATE FUNCTION update_program_active_period_after_cfp_assigned()
RETURNS TRIGGER AS $$
BEGIN

  UPDATE t_program AS g
     SET c_active_start = c.c_active_start,
         c_active_end   = c.c_active_end
    FROM t_proposal p
    JOIN t_cfp c ON p.c_cfp_id = c.c_cfp_id
   WHERE p.c_program_id = NEW.c_program_id
     AND g.c_program_id = p.c_program_id;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_program_active_period_after_cfp_assigned_trigger
AFTER INSERT OR UPDATE OF c_cfp_id ON t_proposal
FOR EACH ROW
WHEN (NEW.c_cfp_id IS NOT NULL)
EXECUTE FUNCTION update_program_active_period_after_cfp_assigned();