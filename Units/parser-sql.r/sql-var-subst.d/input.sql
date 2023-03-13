-- Based on issue #3169 opened by @Appalled

create table  database.tb_name${dt} as
select col_a, col_b from database.tb_name;

create table  database.tb_name${dt}${dt0} as
select col_a, col_b from database.tb_name;

create table  database.${dt1}tb_name${dt}${dt0} as
select col_a, col_b from database.tb_name;

create table  database.${dt1}tb_name${dt}${dt0}Z as
select col_a, col_b from database.tb_name;

create table  database.tb_${dt2}_name as
select col_${key0}, col_${key1} from database.tb_name;

create table  database.tb_${${d}${t:h}${i}}_name as
select col_${key${n}${m}a}, col_${key${m}${n}b} from database.tb_name;
