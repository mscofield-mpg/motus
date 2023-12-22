##Transforming detection data into Activity Values##

#Start with cleaned detection data#
NSWO.detect.keep<-read.csv("./NSWO Migration/Detection files FILTERED/Keepers.NSWO.AllYears_112123.csv")

#Filter to year or time of year of interest
NSWO.2020.detect<- NSWO.detect.keep %>%
  filter(ts <= "2021-01-01 06:00:00" & ts >= "2020-08-15 06:00:00")

#Convert timezone from UTC to local time
NSWO.2020.detect <- NSWO.2020.detect %>%
  mutate(
    ts_LocalTime = with_tz(ts, "America/Denver"),
    date_Local = as.Date(format(ts_LocalTime, tz = "America/Denver")),
    tagDeployStart_Local = with_tz(tagDeployStart, "America/Denver")
  ) %>%
  mutate(hour = hour(ts_LocalTime))

NSWO.2020.detect<- NSWO.2020.detect %>%
  arrange(motusTagID, ts_LocalTime, port)

#Select variables of interest
NSWO.2020.sht <- NSWO.2020.detect %>%
  select("runID", "motusTagID", "ts_LocalTime", "date_Local", "hour", "sig", "runLen", "port", "antBearing", "antType",
         "tagDeployStart_Local", "tagDepLat", "tagDepLon", "recvName",  "recvDeployLat", "recvDeployLon",
         "year", "month")

#double check filter for longer runs
NSWO.2020.sht<- NSWO.2020.sht %>%
  filter(runLen >= 4)

#A couple owls have duplicate records with unique runID's, to simplify we combine duplicates and reassign each detection
#with a new unique detectID
NSWO.sumA<- NSWO.2020.sht %>%
  select(-runID) %>%
  distinct() %>%
  mutate(detectID = seq(1000, length.out = n(), by = 1)) %>%
  select(detectID, everything())

#Assign each detectID into a time_bin (could be hour, 30 mins, 15 mins, 5 mins etc)
NSWO.sumA<- NSWO.sumA %>%
  mutate(time_bin05 = floor_date(ts_LocalTime, "5 mins"),
         time_bin10 = floor_date(ts_LocalTime, "10 mins"),
         time_bin15 = floor_date(ts_LocalTime, "15 mins"),
         time_bin30 = floor_date(ts_LocalTime, "30 mins"))

#For each tag, in each time_bin, at each receiver, at each antenna we want take the standard deviation of signal strength
#then plot against time
NSWO.sum.5min<- NSWO.sumA %>%
  group_by(motusTagID,time_bin05, recvName, port ) %>%
  summarize(sd_sig = sd(sig))

NSWO.sum.10min <- NSWO.sumA %>%
  group_by(motusTagID, time_bin10, recvName, port) %>%
  summarize(sd_sig = sd(sig))

NSWO.sum.15min <- NSWO.sumA %>%
  group_by(motusTagID, time_bin15, recvName, port) %>%
  summarize(sd_sig = sd(sig))

NSWO.sum.30min <- NSWO.sumA %>%
  group_by(motusTagID, time_bin30, recvName, port) %>%
  summarize(sd_sig = sd(sig))

#Assign activity states 
  #the suggested threshold is sd(sig) >= 2 indicates a tag moving significantly within the beam of the antenna
  #if we look at each tag at each time bin, and find at least one antenna with a sd(sig) >= 2, we can assign ACTIVE
  #if no antenna has a sd(sig) >= 2, we can assign INACTIVE
    #*if no detection was recorded during that time bin, then we assign NO DETECT* <-Still working on this

NSWO.sum.5min<- NSWO.sum.5min %>%
  group_by(motusTagID, time_bin05) %>%
  mutate(activity_state = case_when(
    any(sd_sig >= 2) ~ "1", #active
    all(sd_sig < 2) ~ "0" #inactive
  ))

NSWO.sum.10min<- NSWO.sum.10min %>%
  group_by(motusTagID, time_bin10) %>%
  mutate(activity_state = case_when(
    any(sd_sig >= 2) ~ "1",
    all(sd_sig < 2) ~ "0"
  ))

NSWO.sum.15min<- NSWO.sum.15min %>%
  group_by(motusTagID, time_bin15) %>%
  mutate(activity_state = case_when(
    any(sd_sig >= 2) ~ "1",
    all(sd_sig < 2) ~ "0"
  ))

NSWO.sum.30min<- NSWO.sum.30min %>%
  group_by(motusTagID, time_bin30) %>%
  mutate(activity_state = case_when(
    any(sd_sig >= 2) ~ "1",
    all(sd_sig < 2) ~ "0"
  ))
