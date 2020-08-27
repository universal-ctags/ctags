# Taken from candlepin-0.9.54.7/code/schema/quartz/tables_hsqldb.sql
# ------------------------------------------------------------------
#
# In your Quartz properties file, you'll need to set 
# org.quartz.jobStore.driverDelegateClass = org.quartz.impl.jdbcjobstore.HSQLDBDelegate
#

DROP TABLE qrtz_locks IF EXISTS;
DROP TABLE qrtz_scheduler_state IF EXISTS;
DROP TABLE qrtz_fired_triggers IF EXISTS;
DROP TABLE qrtz_paused_trigger_grps IF EXISTS;
DROP TABLE qrtz_calendars IF EXISTS;
DROP TABLE qrtz_trigger_listeners IF EXISTS;
DROP TABLE qrtz_blob_triggers IF EXISTS;
DROP TABLE qrtz_cron_triggers IF EXISTS;
DROP TABLE qrtz_simple_triggers IF EXISTS;
DROP TABLE qrtz_triggers IF EXISTS;
DROP TABLE qrtz_job_listeners IF EXISTS;
DROP TABLE qrtz_job_details IF EXISTS;

CREATE TABLE qrtz_job_details
(
JOB_NAME VARCHAR(200) NOT NULL,
JOB_GROUP VARCHAR(200) NOT NULL,
DESCRIPTION VARCHAR(250) NULL,
JOB_CLASS_NAME VARCHAR(250) NOT NULL,
IS_DURABLE BOOLEAN NOT NULL,
IS_VOLATILE BOOLEAN NOT NULL,
IS_STATEFUL BOOLEAN NOT NULL,
REQUESTS_RECOVERY BOOLEAN NOT NULL,
JOB_DATA BINARY NULL,
PRIMARY KEY (JOB_NAME,JOB_GROUP)
);
