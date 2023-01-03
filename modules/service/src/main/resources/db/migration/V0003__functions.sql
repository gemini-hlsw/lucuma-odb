-- https://stackoverflow.com/questions/54372666/create-an-immutable-clone-of-concat-ws/54384767#54384767
-- CREATE OR REPLACE FUNCTION immutable_concat_ws(text, VARIADIC text[])
-- RETURNS text AS 'text_concat_ws' LANGUAGE internal IMMUTABLE PARALLEL SAFE;

-- We don't have permission to do the above hack on Heroku. But this works.
CREATE OR REPLACE FUNCTION immutable_concat_ws(delim text, parts VARIADIC text[])
RETURNS text AS $$
DECLARE
BEGIN
  RETURN array_to_string(parts, delim);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
