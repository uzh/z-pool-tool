let chunked ~name ~count_from query =
  [%string
    {sql|
      CREATE PROCEDURE %{name}(IN chunk_size INT)
      DETERMINISTIC
      BEGIN
          DECLARE offset_value INT DEFAULT 0;
          DECLARE total_rows INT DEFAULT 0;

          SELECT COUNT(*) INTO total_rows FROM %{count_from};

          WHILE offset_value < total_rows DO
              %{query}
              LIMIT chunk_size OFFSET offset_value
              ON DUPLICATE KEY UPDATE updated_at = NOW();

              SET offset_value = offset_value + chunk_size;
          END WHILE;
      END
    |sql}]
;;
