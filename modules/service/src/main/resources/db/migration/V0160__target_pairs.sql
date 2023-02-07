-- This seems unethical but it's a straightforward to make a data type with a pair
-- of targets, like a CloneTargetResult. When provided with a pair of IDs in its
-- WHERE clause it will compile down to a pair of indexed lookups. Either column
-- can be used as the key in the mapping, assuming you're only selecting one pair.

CREATE OR REPLACE VIEW v_target_pairs AS
  SELECT
    a.c_target_id c_left,
    b.c_target_id c_right
  FROM
    t_target a,
    t_target b
    
