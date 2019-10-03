-- Taken a comment posted by @akemrir in #2215
create table api.comment (
  id serial primary key,
  body character varying(250) not null,
  created_at timestamptz default now(),
  updated_at timestamptz default now()
);
