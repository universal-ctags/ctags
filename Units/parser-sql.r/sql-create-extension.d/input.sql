-- Taken from #2580 submitted by @nobrick
CREATE EXTENSION IF NOT EXISTS "pgcrypto" WITH SCHEMA public;

CREATE TABLE public.foo (
    id bigserial PRIMARY KEY,
);
