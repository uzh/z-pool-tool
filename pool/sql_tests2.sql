CREATE PROCEDURE FindQueueInvitationMapping (IN chunk_size INT) DETERMINISTIC
BEGIN DECLARE offset_value INT DEFAULT 0;

DECLARE total_rows INT DEFAULT 0;

DROP TABLE IF EXISTS temp_invitations;

CREATE TEMPORARY TABLE temp_invitations (
    uuid BINARY(16) NOT NULL,
    experiment_uuid BINARY(16) NOT NULL,
	contact_uuid BINARY(16) NOT NULL,
	created_at TIMESTAMP NOT NULL,
	INDEX tmp_id_experiment (experiment_uuid),
    INDEX tmp_id_contact (contact_uuid)
);

INSERT INTO temp_invitations (uuid, experiment_uuid, contact_uuid, created_at)
SELECT
	pool_invitations.uuid,
	pool_invitations.experiment_uuid,
	pool_invitations.contact_uuid,
	pool_invitations.created_at
FROM
	pool_invitations
	LEFT JOIN pool_queue_job_invitation ON pool_invitations.uuid = pool_queue_job_invitation.invitation_uuid
WHERE
	pool_queue_job_invitation.invitation_uuid IS NULL
	AND pool_invitations.created_at >= "2023-08-02 11:09:01"
	AND pool_invitations.created_at <= "2024-02-19 13:12:28";


SELECT COUNT(*) INTO total_rows FROM temp_invitations;

WHILE offset_value < total_rows DO
INSERT IGNORE INTO
	pool_queue_job_invitation (queue_uuid, invitation_uuid, created_at, updated_at)
WITH
	invitations AS (
		SELECT
			temp_invitations.uuid,
			user_users.email,
			pool_experiments.title AS title,
			LOWER(
				CONCAT(
					SUBSTR(HEX(temp_invitations.uuid), 1, 8),
					'-',
					SUBSTR(HEX(temp_invitations.uuid), 9, 4),
					'-',
					SUBSTR(HEX(temp_invitations.uuid), 13, 4),
					'-',
					SUBSTR(HEX(temp_invitations.uuid), 17, 4),
					'-',
					SUBSTR(HEX(temp_invitations.uuid), 21)
				)
			) AS exp_uuid
		FROM
			temp_invitations
			INNER JOIN pool_contacts ON pool_contacts.user_uuid = temp_invitations.contact_uuid
			INNER JOIN user_users ON pool_contacts.user_uuid = user_users.uuid
			INNER JOIN pool_experiments ON pool_experiments.uuid = temp_invitations.experiment_uuid
			LIMIT chunk_size OFFSET offset_value
	)
SELECT
	Q.uuid,
	I.uuid,
	Q.created_at,
	Q.created_at
FROM
	invitations I
	INNER JOIN pool_queue_jobs_history Q 
ON
	Q.input LIKE CONCAT('%', I.email, '%')
	AND (
		Q.input LIKE "%Einladung%"
		OR Q.input LIKE "%Invitation%"
	)
	AND (Q.input LIKE CONCAT('%', I.exp_uuid, '%'))
ON DUPLICATE KEY UPDATE
	updated_at = NOW();

SET
	offset_value = offset_value + chunk_size;

END WHILE;

END;

CALL FindQueueInvitationMapping(1000);
