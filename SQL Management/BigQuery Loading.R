##Big Query Loading##
  #pulling data from the SQL in BigQuery to R

library(bigrquery)

detections <- 'motus-mpg'

bq_sql <-
  "
  SELECT *
  FROM `motus-mpg.wblake_datadump.filtered`
  "
df_bq <- bq_project_query(detections, bq_sql)
df_tb <- bq_table_download(df_bq)
detections <- as.data.frame(df_tb) 

