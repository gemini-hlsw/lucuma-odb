-- The scheduler's atom listing pages by observation in the order of the ids'
-- numbers (see V1423), then by atom.
CREATE INDEX i_atom_digest_gid_index ON t_atom_digest (gid_index(c_observation_id), c_atom_index);
