-- The sql parser would go into an endless loop with the open but no closing comment.
-- ctags -f - --format=2 --excmd=pattern --fields=nks  --sort=no  --sql-types=cdfFlLPprstTvieURDVnxy bug1324663.sql
--
'\'/*'
