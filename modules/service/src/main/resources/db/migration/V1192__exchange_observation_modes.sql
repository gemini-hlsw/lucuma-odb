-- Exchange observing modes: a Gemini PI requesting time at another observatory
-- (Keck or Subaru).  The new enum values must be committed before they can be
-- referenced in CHECK constraints and the observing-mode registry (see V1187).
ALTER TYPE e_observing_mode_type ADD VALUE 'exchange_keck';
ALTER TYPE e_observing_mode_type ADD VALUE 'exchange_subaru';
