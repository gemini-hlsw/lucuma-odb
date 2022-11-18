-- https://stackoverflow.com/questions/54372666/create-an-immutable-clone-of-concat-ws/54384767#54384767
CREATE OR REPLACE FUNCTION immutable_concat_ws(text, VARIADIC text[])
RETURNS text AS 'text_concat_ws' LANGUAGE internal IMMUTABLE PARALLEL SAFE;
