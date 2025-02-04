-- Insert missing mappings



INSERT IGNORE INTO pool_queue_job_invitation (queue_uuid, invitation_uuid, created_at, updated_at)
WITH
	invitations AS (
		SELECT
			pool_invitations.*,
			user_users.uuid AS user_uuid,
			user_users.email,
			pool_experiments.title AS title,
			LOWER(
				CONCAT(
				SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
				SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
				SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
				SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
				SUBSTR(HEX(pool_experiments.uuid), 21))
			) AS exp_uuid
		FROM
			pool_invitations
			INNER JOIN pool_contacts ON pool_contacts.user_uuid = pool_invitations.contact_uuid
			INNER JOIN user_users ON pool_contacts.user_uuid = user_users.uuid
			INNER JOIN pool_experiments ON pool_invitations.experiment_uuid = pool_experiments.uuid
			LEFT JOIN pool_queue_job_invitation ON pool_invitations.uuid = pool_queue_job_invitation.invitation_uuid
		WHERE
			pool_queue_job_invitation.invitation_uuid IS NULL
			AND pool_invitations.created_at >= "2023-08-02 11:09:01" 
			AND pool_invitations.created_at <= "2024-02-19 13:12:28"
	)
SELECT
	Q.uuid,
	I.uuid,
	Q.created_at,
	Q.created_at
FROM
	pool_queue_jobs_history Q
	CROSS JOIN invitations I
	LEFT JOIN pool_queue_jobs_mapping QM ON Q.uuid = QM.queue_uuid
WHERE
	QM.queue_uuid IS NULL
	AND Q.input LIKE CONCAT('%', I.email, '%')
	AND (
		Q.input LIKE "%Einladung%"
		OR Q.input LIKE "%Invitation%"
	)
	AND (Q.input LIKE CONCAT('%', I.exp_uuid, '%'));



------------------------------------------------------------

CREATE PROCEDURE FindQueueInvitationMapping (IN chunk_size INT) DETERMINISTIC
BEGIN DECLARE offset_value INT DEFAULT 0;

DECLARE total_rows INT DEFAULT 0;

SELECT
	COUNT(*) INTO total_rows
FROM
	pool_invitations
	INNER JOIN pool_contacts ON pool_contacts.user_uuid = pool_invitations.contact_uuid
	INNER JOIN user_users ON pool_contacts.user_uuid = user_users.uuid
	INNER JOIN pool_experiments ON pool_invitations.experiment_uuid = pool_experiments.uuid
	LEFT JOIN pool_queue_job_invitation ON pool_invitations.uuid = pool_queue_job_invitation.invitation_uuid
WHERE
	pool_queue_job_invitation.invitation_uuid IS NULL
	AND pool_invitations.created_at >= "2023-08-02 11:09:01"
	AND pool_invitations.created_at <= "2024-02-19 13:12:28";

WHILE offset_value < total_rows DO
INSERT IGNORE INTO
	pool_queue_job_invitation (queue_uuid, invitation_uuid, created_at, updated_at)
WITH
	invitations AS (
		SELECT
			pool_invitations.*,
			user_users.uuid AS user_uuid,
			user_users.email,
			pool_experiments.title AS title,
			LOWER(
				CONCAT(
					SUBSTR(HEX(pool_experiments.uuid), 1, 8),
					'-',
					SUBSTR(HEX(pool_experiments.uuid), 9, 4),
					'-',
					SUBSTR(HEX(pool_experiments.uuid), 13, 4),
					'-',
					SUBSTR(HEX(pool_experiments.uuid), 17, 4),
					'-',
					SUBSTR(HEX(pool_experiments.uuid), 21)
				)
			) AS exp_uuid
		FROM
			pool_invitations
			INNER JOIN pool_contacts ON pool_contacts.user_uuid = pool_invitations.contact_uuid
			INNER JOIN user_users ON pool_contacts.user_uuid = user_users.uuid
			INNER JOIN pool_experiments ON pool_invitations.experiment_uuid = pool_experiments.uuid
			LEFT JOIN pool_queue_job_invitation ON pool_invitations.uuid = pool_queue_job_invitation.invitation_uuid
		WHERE
			pool_queue_job_invitation.invitation_uuid IS NULL
			AND pool_invitations.created_at >= "2023-08-02 11:09:01"
			AND pool_invitations.created_at <= "2024-02-19 13:12:28"
			LIMIT
	chunk_size
OFFSET
	offset_value
	)
SELECT
	Q.uuid,
	I.uuid,
	Q.created_at,
	Q.created_at
FROM
	pool_queue_jobs_history Q
	CROSS JOIN invitations I
	LEFT JOIN pool_queue_jobs_mapping QM ON Q.uuid = QM.queue_uuid
WHERE
	QM.queue_uuid IS NULL
	AND Q.input LIKE CONCAT('%', I.email, '%')
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

CALL FindQueueInvitationMapping(100)




