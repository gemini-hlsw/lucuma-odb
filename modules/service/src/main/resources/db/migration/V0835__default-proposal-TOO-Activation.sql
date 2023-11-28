-- Update the proposal table to default to 'none'

ALTER TABLE t_proposal 
  ALTER COLUMN c_too_activation TYPE e_too_activation USING (coalesce(c_too_activation, 'none')),
  ALTER COLUMN c_too_activation SET DEFAULT 'none',
  ALTER COLUMN c_too_activation SET NOT NULL;
