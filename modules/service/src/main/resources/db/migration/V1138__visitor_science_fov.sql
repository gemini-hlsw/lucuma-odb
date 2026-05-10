-- Rename t_visitor.c_guide_star_min_sep => t_visitor.c_science_fov
ALTER TABLE t_visitor
  RENAME COLUMN c_guide_star_min_sep TO c_science_fov;
