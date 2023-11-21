-- Taken from leapp-0.15.1/res/schema/audit-layout.sql

CREATE VIEW IF NOT EXISTS messages_data AS
  SELECT
    message.id        AS id,
    message.context   AS context,
    message.stamp     AS stamp,
    message.topic     AS topic,
    message.type      AS type,
    data_source.actor AS actor,
    data_source.phase AS phase,
    msg_data.hash     AS message_hash,
    msg_data.data     AS message_data,
    host.hostname     AS hostname
  FROM
    message
  JOIN
    data_source              ON data_source.id            = message.data_source_id,
    message_data AS msg_data ON message.message_data_hash = msg_data.hash,
    host                     ON host.id                   = data_source.host_id
;
