
CREATE TABLE urls
  ( short_url TEXT NOT NULL PRIMARY KEY
  , original_url TEXT NOT NULL
  , creation_date TIMESTAMPTZ NOT NULL DEFAULT now()
  );

