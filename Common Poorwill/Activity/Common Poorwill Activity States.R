####Common Poorwill Breeding Season Lines####
  #Created 6.18.2024#
  #Updated 8.7.2024#
library(tidyverse)
library(viridis)

##Load detections##
COPO.detect<-read_csv("./Processed Data/COPO/7.4.24/COPO_tag_detections_FILTERED_7.4.24.csv")

#Convert to POSIXct format (the timestamps are in UTC, so we keep in UTC)
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

COPO.detect <- COPO.detect %>%
  select(runID, motusTagID, mfgID, ts_LocalTime, date_Local, sig, port, recvName,
         freqsd, runLen, noise, antType, antBearing, antHeight, recvDeployLat, recvDeployLon) %>%
  arrange(motusTagID, ts_LocalTime) %>%
  distinct(motusTagID, ts_LocalTime, recvName, port, .keep_all = TRUE)

##Assign Time Bins##
COPO.act<- COPO.detect %>%
  filter(ts_LocalTime >= "2024-05-01 00:00:00") %>%
  mutate(bin_01 = cut(ts_LocalTime, breaks = "01 mins"),
         bin_05 = cut(ts_LocalTime, breaks = "05 mins"),
         bin_10 = cut(ts_LocalTime, breaks = "10 mins"),
         bin_15 = cut(ts_LocalTime, breaks = "15 mins")) %>%
  mutate(mfgID = case_when(
    mfgID == 148 ~ 598,
    mfgID == 275 ~ 589,
    mfgID == 154 ~ 586,
    TRUE ~ as.numeric(mfgID)
  ))

# 1 Minute #
COPO.01<- COPO.act %>%
  group_by(mfgID, bin_01, recvName, port) %>%
  mutate(sd_sig = sd(sig),
         avg_sig = mean(sig),
         num_recs = n()) %>%
  ungroup() %>%
  select(date_Local, mfgID, bin_01, sd_sig, avg_sig, recvName, port, antBearing, num_recs) %>%
  distinct()

# 5 Minutes #
COPO.05<- COPO.act %>%
  group_by(mfgID, bin_05, recvName, port) %>%
  mutate(sd_sig = sd(sig),
         avg_sig = mean(sig),
         num_recs = n()) %>%
  ungroup() %>%
  select(date_Local, mfgID, bin_05, sd_sig, avg_sig, recvName, port, antBearing, num_recs) %>%
  distinct()

# 10 Minutes #
COPO.10<- COPO.act %>%
  group_by(mfgID, bin_10, recvName, port) %>%
  mutate(sd_sig = sd(sig),
         avg_sig = mean(sig),
         num_recs = n()) %>%
  ungroup() %>%
  select(date_Local, mfgID, bin_10, sd_sig, avg_sig, recvName, port, antBearing, num_recs) %>%
  distinct()

# 15 Minutes #
COPO.15<- COPO.act %>%
  group_by(mfgID, bin_15, recvName, port) %>%
  mutate(sd_sig = sd(sig),
         avg_sig = mean(sig),
         num_recs = n()) %>%
  ungroup() %>%
  select(date_Local, mfgID, bin_15, sd_sig, avg_sig, recvName, port, antBearing, num_recs) %>%
  distinct()

##Assign Activity States##
COPO.05a<- COPO.05 %>%
  mutate(active = case_when(
    num_recs < 20 ~ "Unknown",
    num_recs >= 20 & sd_sig <= 2 ~ "Inactive",
    num_recs >= 20 & sd_sig > 2 ~ "Active"))

COPO.10a<- COPO.10 %>%
  mutate(active = case_when(
    num_recs < 20 ~ "Unknown",
    num_recs >= 20 & sd_sig <= 2 ~ "Inactive",
    num_recs >= 20 & sd_sig > 2 ~ "Active"))

COPO.15a<- COPO.15 %>%
  mutate(active = case_when(
    num_recs < 20 ~ "Unknown",
    num_recs >= 20 & sd_sig <= 2 ~ "Inactive",
    num_recs >= 20 & sd_sig > 2 ~ "Active"))
