-- Based on https://github.com/tonyta/ScrallWall/blob/master/db/data/neighborhoods.sql
SET CLIENT_ENCODING TO UTF8;
SET STANDARD_CONFORMING_STRINGS TO ON;
BEGIN;
CREATE TABLE "neighborhoods" (gid serial,
"objectid" numeric(10,0),
"pri_neigh_" varchar(3),
"pri_neigh" varchar(50),
"sec_neigh_" varchar(3),
"sec_neigh" varchar(50),
"shape_area" numeric,
"shape_len" numeric);
ALTER TABLE "neighborhoods" ADD PRIMARY KEY (gid);
CREATE INDEX "neighborhoods_geom_gist" ON "neighborhoods" USING GIST ("geom");
COMMIT;

BEGIN TRANSACTION;
CREATE TABLE "player" (uid serial, "name" varchar(50));
COMMIT;
