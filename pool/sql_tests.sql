      INSERT INTO
	pool_experiment_invitation_reset (experiment_uuid, contacts_matching_filter, sent_invitations, created_at, updated_at)
WITH
	experiment_max_sent AS (
		SELECT
			experiment_uuid,
			MAX(send_count) AS max_count
		FROM
			pool_invitations
		GROUP BY
			experiment_uuid
	),
	experiment_reset_at AS (
		SELECT
			experiment_uuid,
			send_count,
			MIN(updated_at) AS updated_at
		FROM
			pool_invitations
		GROUP BY
			experiment_uuid,
			send_count
	),
	computed_resets AS (
		SELECT
			I.experiment_uuid,
			I.send_count,
			EMS.max_count,
			CASE
				WHEN EMS.max_count = 2 THEN (
					SELECT
						COUNT(id)
					FROM
						pool_invitations
					WHERE
						experiment_uuid = I.experiment_uuid
						AND send_count = 2
				)
				ELSE COUNT(I.send_count)
			END AS sent_invitations,
			CASE
				WHEN EMS.max_count = 2 THEN E.invitation_reset_at
				ELSE ERA.updated_at
			END AS reset_at
		FROM
			pool_invitations I
			LEFT JOIN experiment_max_sent EMS ON I.experiment_uuid = EMS.experiment_uuid
			INNER JOIN experiment_reset_at ERA ON I.experiment_uuid = ERA.experiment_uuid
			LEFT JOIN pool_experiments E ON E.uuid = I.experiment_uuid
		WHERE
			I.send_count != EMS.max_count
		GROUP BY
			experiment_uuid,
			send_count
		ORDER BY
			experiment_uuid,
			reset_at DESC
	)
SELECT
	experiment_uuid,
	-1,
	sent_invitations,
	reset_at,
	reset_at
FROM
	computed_resets;









      INSERT INTO
	pool_experiment_invitation_reset (experiment_uuid, contacts_matching_filter, sent_invitations, created_at, updated_at)
WITH
	experiment_max_sent AS (
		SELECT
			experiment_uuid,
			MAX(send_count) AS max_count
		FROM
			pool_invitations
		GROUP BY
			experiment_uuid
	),
	experiment_reset_at AS (
		SELECT
			experiment_uuid,
			send_count,
			MIN(updated_at) AS updated_at
		FROM
			pool_invitations
		GROUP BY
			experiment_uuid,
			send_count
	),
	computed_resets AS (
		SELECT
			I.experiment_uuid,
			I.send_count,
			EMS.max_count,
			CASE
				WHEN EMS.max_count = 2 THEN (
					SELECT
						COUNT(id)
					FROM
						pool_invitations
					WHERE
						experiment_uuid = I.experiment_uuid
						AND created_at >= E.invitation_reset_at
				)
				ELSE COUNT(I.send_count)
			END AS sent_invitations,
			CASE
				WHEN EMS.max_count = 2 THEN E.invitation_reset_at
				ELSE ERA.updated_at
			END AS reset_at
		FROM
			pool_invitations I
			LEFT JOIN experiment_max_sent EMS ON I.experiment_uuid = EMS.experiment_uuid
			INNER JOIN experiment_reset_at ERA ON I.experiment_uuid = ERA.experiment_uuid
			LEFT JOIN pool_experiments E ON E.uuid = I.experiment_uuid
		WHERE
			I.send_count != EMS.max_count
		GROUP BY
			experiment_uuid,
			send_count
		ORDER BY
			experiment_uuid,
			reset_at DESC
	)
SELECT
	experiment_uuid,
	-1,
	sent_invitations,
	reset_at,
	reset_at
FROM
	computed_resets;
