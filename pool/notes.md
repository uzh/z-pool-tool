PROMOTED CONTACTS, What to do with Invitations, etc..??
SELECT
 pool_experiments.title,
 user_users.email,
 pool_invitations.*
FROM
 pool_invitations
 LEFT JOIN pool_contacts ON pool_invitations.contact_uuid = pool_contacts.user_uuid
 LEFT JOIN user_users ON pool_invitations.contact_uuid = user_users.uuid

=========

Why do these experiments not have a reset_at timestamp???
    => Resend Invitations button was hit

SELECT
 experiment_uuid,
 invitation_reset_at,
 MAX(send_count) AS max_count
FROM
 pool_invitations
 INNER JOIN pool_experiments ON pool_experiments.uuid = pool_invitations.experiment_uuid
GROUP BY
 experiment_uuid
HAVING
 max_count = 2;

=============

Pool queue jobs mapping: 404'911
 since 2024-02-19

Toal Invitations: 624'326
 since 2013-02-11

 124'800 since start of mapping  --- SUM send_count 148'290 (SELECT SUM(send_count) FROM pool_invitations WHERE created_at >= "2024-02-19 13:12:28")

This offset is 20'933:

SELECT
 COUNT(Q.queue_uuid) AS queue_count,
 I.send_count,
 Q.invitation_uuid
FROM
 pool_queue_job_invitation Q
 INNER JOIN pool_invitations I ON Q.invitation_uuid = I.uuid
GROUP BY
 Q.invitation_uuid
HAVING
 queue_count <> I.send_count
ORDER BY
 queue_count DESC;
