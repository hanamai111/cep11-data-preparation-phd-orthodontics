-- US. Read-only. Run the two statements separately.
-- Coverage, identifiers, event inventory and volume. Job 5QgjWCtSnRDVYUw43iFWN9FPqTH9.
WITH e AS (
  SELECT event_date, event_name, event_timestamp,
    NULLIF(TRIM(user_pseudo_id),'') AS uid,
    (SELECT MIN(p.value.int_value) FROM UNNEST(event_params) p
      WHERE p.key='ga_session_id') AS sid,
    (SELECT MIN(NULLIF(TRIM(p.value.string_value),'')) FROM UNNEST(event_params) p
      WHERE p.key='page_location') AS url
  FROM `bigquery-public-data.ga4_obfuscated_sample_ecommerce.events_*`
  WHERE _TABLE_SUFFIX BETWEEN '20201101' AND '20210131'
), daily AS (
  SELECT event_date, COUNT(*) AS n FROM e GROUP BY 1
), inventory AS (
  SELECT event_name, COUNT(*) AS n FROM e GROUP BY 1
), totals AS (
  SELECT MIN(event_date) AS first_day, MAX(event_date) AS last_day,
    COUNT(DISTINCT event_date) AS days, COUNT(*) AS events,
    COUNT(DISTINCT uid) AS observed_ids,
    COUNT(DISTINCT IF(uid IS NOT NULL AND sid IS NOT NULL,
      TO_JSON_STRING(STRUCT(uid,sid)), NULL)) AS observed_sessions,
    COUNTIF(uid IS NULL) AS missing_uid, COUNTIF(sid IS NULL) AS missing_sid,
    COUNTIF(event_name='page_view' AND url IS NULL) AS page_views_missing_url,
    COUNTIF(uid IS NOT NULL AND event_timestamp IS NOT NULL AND event_name IS NOT NULL)
      - COUNT(DISTINCT IF(uid IS NOT NULL AND event_timestamp IS NOT NULL
          AND event_name IS NOT NULL, TO_JSON_STRING(STRUCT(uid,event_timestamp,event_name)),NULL))
      AS repeat_triplet_excess
  FROM e
)
SELECT * FROM totals
CROSS JOIN (
  SELECT MIN(n) AS min_daily, MAX(n) AS max_daily, ROUND(AVG(n),2) AS mean_daily,
    ARRAY_AGG(STRUCT(event_date,n) ORDER BY n DESC,event_date LIMIT 3) AS busiest
  FROM daily
)
CROSS JOIN (
  SELECT STRING_AGG(CONCAT(event_name, ': ', n), ' | ' ORDER BY n DESC) AS inventory
  FROM inventory
);

-- Existing sample views. Job v5aqiwImaCnfI7kZPLFzVzJC3vnP.
SELECT
  (SELECT COUNT(*) FROM `our-truth-494618-b1.ga4_sample_mart_v1.v_daily_kpis`) AS daily_rows,
  (SELECT COUNT(*) FROM `our-truth-494618-b1.ga4_sample_mart_v1.v_ecommerce_funnel`) AS funnel_rows,
  COUNT(*) AS session_rows,
  COUNT(*)-COUNT(DISTINCT session_key) AS duplicate_keys,
  COUNTIF(observed_dates>1) AS cross_date,
  COUNTIF(session_start_timestamp IS NULL) AS no_session_start,
  COUNTIF(landing_page IS NULL) AS missing_landing,
  COUNTIF(LOWER(first_user_source) IN ('(not set)','<other>','(data deleted)','(other)')) AS source_placeholder,
  COUNTIF(LOWER(first_user_medium) IN ('(not set)','<other>','(data deleted)','(other)')) AS medium_placeholder,
  COUNTIF(LOWER(country) IN ('(not set)','<other>','(data deleted)','(other)')) AS country_placeholder
FROM `our-truth-494618-b1.ga4_sample_mart_v1.v_session_activity`;
