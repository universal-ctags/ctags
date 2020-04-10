-- Taken from #2504 opened by @koenmeersman
select substr(sess_evnt.event, 1, 18) event
from  v$session_event sess_evnt;
