## COPO Updated Departure ##
#Updated 4.8.24#

library(tidyverse)
library(viridis)


##Load in detections
COPO.detect<- read.csv("./Processed Data/COPO/4.9.24/COPO_tag_detections_FILTERED_4.9.24.csv")
#you lose tz with the csv version... double check that the filtered csv is in UTC before converting
#confirmed they are in UTC...  must first convert to POSIXct format with UTC tz
COPO.detect$ts <- as.POSIXct(COPO.detect$ts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
COPO.detect$tagDeployStart<- as.POSIXct(COPO.detect$tagDeployStart, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#then force tz to Moutain time
#modify time stamps and reduce variables
COPO.detect<- COPO.detect %>%
  mutate(ts_LocalTime = with_tz(ts, "America/Denver"),
         date_Local = as.Date(format(ts_LocalTime, tz = "America/Denver")),
         tagDeployStart_Local = with_tz(tagDeployStart, "America/Denver"))

COPO.detect<- COPO.detect %>%
  filter(motusFilter == 1)

#load a receiver list to filter for Bitterroot Valley Only stations
recvs<- COPO.detect %>%
  select(recvName, recvDeployID, port, antBearing, antType, antHeight) %>%
  arrange(recvName) %>%
  distinct()
write.csv(recvs, "./COPO Breeding Movements/COPO_Recvs_4.8.24.csv", row.names = FALSE)
BV_recvs<- read_xlsx("./COPO Breeding Movements/COPO_BV ONLY Recvs_4.8.24.xlsx")
BV_recvs_ids<- BV_recvs %>%
  select(recvName, recvDeployID) %>%
  distinct()

#load desired receiver vector 
BV_RecvIDs<- c(BV_recvs_ids$recvDeployID)

#filter detections to just BV, remove duplicated runs 
COPO.depart <- COPO.detect %>%
  arrange(motusTagID, ts_LocalTime) %>%
  filter(recvDeployID %in% BV_RecvIDs)%>% 
  distinct(motusTagID, ts_LocalTime, recvName, port, .keep_all = TRUE)


##Filter to years##
COPO.depart.2020 <- COPO.depart %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime <= "2021-01-01 00:00:00")

COPO.depart.2021 <- COPO.depart %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= "2021-01-01 00:00:00" & ts_LocalTime <= "2022-01-01 00:00:00")

COPO.depart.2022<- COPO.depart %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= "2022-01-01 00:00:00" & ts_LocalTime <= "2023-01-01 00:00:00")

COPO.depart.2023<- COPO.depart %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= "2023-01-01 00:00:00")

#reduce variables for each year
COPO.depart.2020 <- COPO.depart.2020 %>%
  arrange(motusTagID, ts_LocalTime) %>%
  select(motusTagID, runID, hitID, ts_LocalTime, date_Local, year, sig, runLen, port, recvName, recvDeployLat,
         recvDeployLon, antBearing, antType)

COPO.depart.2021 <- COPO.depart.2021 %>%
  arrange(motusTagID, ts_LocalTime) %>%
  select(motusTagID, runID, hitID, ts_LocalTime, date_Local, year, sig, runLen, port, recvName, recvDeployLat,
         recvDeployLon, antBearing, antType)

COPO.depart.2022 <- COPO.depart.2022 %>%
  arrange(motusTagID, ts_LocalTime) %>%
  select(motusTagID, runID, hitID, ts_LocalTime, date_Local, year, sig, runLen, port, recvName, recvDeployLat,
         recvDeployLon, antBearing, antType)

COPO.depart.2023 <- COPO.depart.2023 %>%
  arrange(motusTagID, ts_LocalTime) %>%
  select(motusTagID, runID, hitID, ts_LocalTime, date_Local, year, sig, runLen, port, recvName, recvDeployLat,
         recvDeployLon, antBearing, antType)

##Filter to last 8 days worth of detections for each tag
COPO.depart.20a <- COPO.depart.2020 %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= max(ts_LocalTime) - days(8)) %>%
  ungroup() %>%
  group_by(runID) %>%
  mutate(avg_sig = mean(sig),
         sd_sig = sd(sig),
         ts_start = min(ts_LocalTime),
         ts_end = max(ts_LocalTime),
         slope = coef(lm(sig ~ as.numeric(difftime(ts_LocalTime, min(ts_LocalTime), units = "mins"))))[2]) %>%
  ungroup() %>%
  select(motusTagID, date_Local, year, runID, ts_start, ts_end, runLen, avg_sig, sd_sig, slope, port, recvName, antBearing, antType,
         recvDeployLat, recvDeployLon) %>%
  distinct()

COPO.depart.21a <- COPO.depart.2021 %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= max(ts_LocalTime) - days(8)) %>%
  ungroup() %>%
  group_by(runID) %>%
  mutate(avg_sig = mean(sig),
         sd_sig = sd(sig),
         ts_start = min(ts_LocalTime),
         ts_end = max(ts_LocalTime),
         slope = coef(lm(sig ~ as.numeric(difftime(ts_LocalTime, min(ts_LocalTime), units = "mins"))))[2]) %>%
  ungroup() %>%
  select(motusTagID, date_Local, year, runID, ts_start, ts_end, runLen, avg_sig, sd_sig, slope, port, recvName, antBearing, antType,
         recvDeployLat, recvDeployLon) %>%
  distinct()

COPO.depart.22a <- COPO.depart.2022 %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= max(ts_LocalTime) - days(8)) %>%
  ungroup() %>%
  group_by(runID) %>%
  mutate(avg_sig = mean(sig),
         sd_sig = sd(sig),
         ts_start = min(ts_LocalTime),
         ts_end = max(ts_LocalTime),
         slope = coef(lm(sig ~ as.numeric(difftime(ts_LocalTime, min(ts_LocalTime), units = "mins"))))[2]) %>%
  ungroup() %>%
  select(motusTagID, date_Local, year, runID, ts_start, ts_end, runLen, avg_sig, sd_sig, slope, port, recvName, antBearing, antType,
         recvDeployLat, recvDeployLon) %>%
  distinct()

COPO.depart.23a <- COPO.depart.2023 %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= max(ts_LocalTime) - days(8)) %>%
  ungroup() %>%
  group_by(runID) %>%
  mutate(avg_sig = mean(sig),
         sd_sig = sd(sig),
         ts_start = min(ts_LocalTime),
         ts_end = max(ts_LocalTime),
         slope = coef(lm(sig ~ as.numeric(difftime(ts_LocalTime, min(ts_LocalTime), units = "mins"))))[2]) %>%
  ungroup() %>%
  select(motusTagID, date_Local, year, runID, ts_start, ts_end, runLen, avg_sig, sd_sig, slope, port, recvName, antBearing, antType,
         recvDeployLat, recvDeployLon) %>%
  distinct()

#Join years together ##TABLE 1##
COPO.depart.AllYears<- COPO.depart.20a %>%
  bind_rows(COPO.depart.21a, COPO.depart.22a, COPO.depart.23a)

write.csv(COPO.depart.AllYears, "./Lunar/COPO/Migration Movement/4.4.24/COPO_Departure_AllYears_Table1_4.4.24.csv", row.names =  FALSE)

COPO.depart.AllYears<- COPO.depart.AllYears %>%
  select(motusTagID, year, runID, date_Local, ts_start, ts_end, recvName, port, avg_sig, runLen, everything()) %>%
  arrange(motusTagID, year)


##Table 2 --- ungrouped by runID
COPO.depart.20b <- COPO.depart.2020 %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= max(ts_LocalTime) - days(8)) %>%
  ungroup() %>%
  select(motusTagID, date_Local, year, runID, hitID, ts_LocalTime, sig, runLen, port, recvName, antBearing, antType,
         recvDeployLat, recvDeployLon) %>%
  distinct()

COPO.depart.21b <- COPO.depart.2021 %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= max(ts_LocalTime) - days(8)) %>%
  ungroup() %>%
  select(motusTagID, date_Local, year, runID, hitID, ts_LocalTime, sig, runLen, port, recvName, antBearing, antType,
         recvDeployLat, recvDeployLon) %>%
  distinct()

COPO.depart.22b <- COPO.depart.2022 %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= max(ts_LocalTime) - days(8)) %>%
  ungroup() %>%
  select(motusTagID, date_Local, year, runID, hitID, ts_LocalTime, sig, runLen, port, recvName, antBearing, antType,
         recvDeployLat, recvDeployLon) %>%
  distinct()

COPO.depart.23b <- COPO.depart.2023 %>%
  group_by(motusTagID) %>%
  filter(ts_LocalTime >= max(ts_LocalTime) - days(8)) %>%
  ungroup() %>%
  select(motusTagID, date_Local, year, runID, hitID, ts_LocalTime, sig, runLen, port, recvName, antBearing, antType,
         recvDeployLat, recvDeployLon) %>%
  distinct()

#Join all years together
COPO.depart.table2 <- COPO.depart.20b %>%
  bind_rows(COPO.depart.21b, COPO.depart.22b, COPO.depart.23b)

COPO.tags<- c(unique(COPO.depart.table2$motusTagID))
names(COPO.detect)
COPO_Key<- COPO.detect %>%
  select(motusTagID, mfgID, tagDeployID, tagDeployStart_Local) %>%
  arrange(motusTagID) %>%
  distinct()
write.csv(COPO_Key, "./Lunar/COPO/Migration Movement/4.4.24/COPO_tag key_4.4.24.csv", row.names = FALSE)

##Table filtering function
Table.Filter<- function(data, Runs) {
  sub<- data %>%
    filter(runID %in% Runs)
  
  sub$port <- factor(sub$port)
  sub$recvName <- factor(sub$recvName)
  
  plot<- ggplot(sub, aes(ts_LocalTime, sig, color = interaction(recvName, port))) +
    geom_point(size = 3) +
    scale_color_manual(values = viridis_pal()(length(unique(interaction(sub$recvName, sub$port))))) +
    theme_minimal() +
    labs(title = paste("Motus Tag ID:", sub$motusTagID),
         subtitle = paste("Date:", min(sub$date_Local), "-", max(sub$date_Local)),
         x = "Timestamp",
         y = "Signal Strength",
         color = "Receiver Antenna") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot2<- ggplot(sub, aes(ts_LocalTime, sig, color = recvName))+
    geom_point(size = 3) +
    scale_color_manual(values = viridis_pal()(length(unique(sub$recvName)))) +
    theme_minimal() +
    labs(title = paste("Motus Tag ID:", sub$motusTagID),
         subtitle = paste("Date:", min(sub$date_Local), "-", max(sub$date_Local)),
         x = "Timestamp",
         y = "Signal Strength",
         color = "Receiver") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot)
  print(plot2)
  
  peak_hit<- sub %>%
    group_by(recvName) %>%
    filter(sig == max(sig)) %>%
    ungroup()
  
  view(sub)
  view(peak_hit)
}

data <- COPO.depart.table2

#example to isolate runs of interest
runs<- COPO.depart.22a %>%
  filter(motusTagID == 60911,
         ts_start >= "2022-09-20 21:21:51")
#load departure runs and run through the function
Runs<- runs$runID
Table.Filter(data, Runs)

#if you need a closer look at a bird without the 8 day filter
copo<- COPO.depart.2022 %>%
  filter(motusTagID == 60911) %>%
  group_by(runID) %>%
  mutate(avg_sig = mean(sig),
         sd_sig = sd(sig),
         ts_start = min(ts_LocalTime),
         ts_end = max(ts_LocalTime),
         slope = coef(lm(sig ~ as.numeric(difftime(ts_LocalTime, min(ts_LocalTime), units = "mins"))))[2]) %>%
  ungroup() %>%
  select(motusTagID, date_Local, year, runID, ts_start, ts_end, recvName, port, avg_sig, sd_sig, slope, antBearing, antType,
         recvDeployLat, recvDeployLon, runLen) %>%
  distinct()
