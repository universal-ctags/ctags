-- Taken from https://www.postgresql.org/docs/current/sql-createdatabase.html
CREATE DATABASE lusiadas;
CREATE DATABASE sales OWNER salesapp TABLESPACE salesspace;
CREATE DATABASE music
    LOCALE 'sv_SE.utf8'
    TEMPLATE template0;
CREATE DATABASE music2
    LOCALE 'sv_SE.iso885915'
    ENCODING LATIN9
    TEMPLATE template0;

-- Taken from http://www.hplsql.org/create-database
create database 'test' || replace(current_date, '-', '');
