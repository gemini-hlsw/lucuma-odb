-- Adds the "is visitor" flag to the instrument table.
ALTER TABLE t_instrument
  ADD COLUMN c_is_visitor bool NOT NULL DEFAULT false;

UPDATE t_instrument SET c_is_visitor = true WHERE c_tag IN (
  'Alopeke',
  'MaroonX',
  'VisitorNorth',
  'VisitorSouth',
  'Zorro'
);
