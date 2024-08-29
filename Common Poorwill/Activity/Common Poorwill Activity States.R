####Common Poorwill Breeding Season Lines####
  #Created 6.18.2024#
  #Updated 8.7.2024#
  #Updated 8.29.24#
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

#filter detections to time period of interest, and combine tags that
  #represent the same bird through the season
COPO.act<- COPO.detect %>%
  filter(ts_LocalTime >= "2024-05-01 00:00:00") %>%
  mutate(mfgID = case_when(
    mfgID == 148 ~ 598,
    mfgID == 275 ~ 589,
    mfgID == 154 ~ 586,
    TRUE ~ as.numeric(mfgID)
  ))

# Define Activity_Threshold
  #Modify as needed
Activity_Threshold <- function(sd_sig) {
  case_when(
    sd_sig <= 2 ~ "Inactive",
    sd_sig > 2 ~ "Active"
  )
}

#Import sunset/sunrise data
#code out an import line!!!!
~/Library/CloudStorage/Egnyte-mpgcloud/Private/mscofield/R/Motus/Lunar/Moon Data/2024/Moon Times 2024.csv

#Ensure correct timezone
Moon_Times_2024 <- Moon_Times_2024 %>%
  mutate(
    Night_Start = force_tz(Night_Start, "America/Denver"),
    Night_End = force_tz(Night_End, "America/Denver"),
    sunset = force_tz(sunset, "America/Denver"),
    dusk = force_tz(dusk, "America/Denver"),
    nauticalDusk = force_tz(nauticalDusk, "America/Denver"),
    nauticalDawn = force_tz(nauticalDawn, "America/Denver"),
    dawn = force_tz(dawn, "America/Denver"),
    sunrise = force_tz(sunrise, "America/Denver"))


#Define Activity_Function with proper time interval handling
  #Modify bin length to desired time in minutes
Activity_Function <- function(COPO.act, Bin_Length = "1 min", Activity_Threshold, Moon_Times_2024) {
  
  activity_table <- COPO.act %>%
    mutate(time_bin = floor_date(ts_LocalTime, unit = Bin_Length)) %>%
    group_by(mfgID, time_bin, recvName, port) %>%
    mutate(sd_sig = sd(sig),
           avg_sig = mean(sig),
           num_recs = n()) %>%
    ungroup() %>%
    select(mfgID, date_Local, time_bin, sd_sig, avg_sig, port, recvName, antBearing, num_recs) %>%
    distinct() %>%
    group_by(mfgID, time_bin, recvName, port) %>%
    mutate(State = Activity_Threshold(sd_sig)) %>%
    ungroup()
  
 
  activity_table <- activity_table %>%
    fuzzy_left_join(Moon_Times_2024,
                    by = c("time_bin" = "Night_Start", "time_bin" = "Night_End"),
                    match_fun = list(`>=`, `<=`)) %>%
    select(mfgID, Night_ID, time_bin, everything(), -date_Local, -date, -Night_Start, -Night_End)
  
 
  activity_table <- activity_table %>%
    mutate(Temp = case_when(
      (time_bin >= sunset  & time_bin < sunrise ) ~ "Night",
      (time_bin >= sunrise  | time_bin < sunset ) ~ "Day"
    )) %>%
    distinct()
  
  #summary table
  activity_table.fil <- activity_table %>%
    group_by(mfgID, time_bin) %>%
    filter(avg_sig == max(avg_sig)) %>%
    ungroup() %>%
    filter(!is.na(State))
  
  bin_duration <- as.numeric(sub(" min", "", Bin_Length))
  
  activity_sum <- activity_table.fil %>%
    group_by(mfgID, Night_ID, Temp, State) %>%
    summarise(
      Total_Minutes = n() *  bin_duration
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = State, values_from = Total_Minutes, values_fill = 0)
  
  return(list(activity_table = activity_table, activity_sum = activity_sum))
}

#Run the function
results <- Activity_Function(COPO.act, Bin_Length = "1 min", Activity_Threshold, Moon_Times_2024)

#Extract results
Activity_Table.1 <- results$activity_table
Activity_Summary.1 <- results$activity_sum


####Visualize#####

###PLOT 1###
  #this visualizes night time activity and day time activity by night for each tag
  #Use the summary from above with desired time-bin
#create vector
mfg_ids <- unique(Activity_Summary.10$mfgID)

#Loop through vector and save each plot
for (ID in mfg_ids) {
  plot_data <- filter(Activity_Summary.10, mfgID == ID)
  
  p <- ggplot(plot_data, aes(x = Night_ID)) +
    geom_line(aes(y = Active, color = Temp, group = Temp), size = 1.2) +
    geom_point(aes(y = Active, color = Temp), size = 3) +
    labs(
      title = paste("Minutes Active per Night COPO:", ID),
      subtitle = "Bin Length = 10 min",
      x = "Night ID",
      y = "Minutes Active",
      color = "Time Period"
    ) +
    scale_color_manual(values = c("Day" = "orange", "Night" = "blue")) +
    theme_bw() +
    theme(text = element_text(family = "serif", size = 12))

  
  ggsave(filename = paste0("Minutes Active by Night_COPO ", ID, ".png"), 
         path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures/Summer Activity/8.28.24/10 min", 
         width = 7, 
         height = 5, 
         units = "in")
  
}

####

####Visualize Overview
Activity_Overview<- Activity_Summary.10 %>%
  mutate(Bin_Length = 10)

Activity_Summary.9b<- Activity_Summary.9 %>%
  mutate(Bin_Length = 9)

Activity_Overview<- Activity_Overview %>%
  bind_rows(Activity_Summary.9b, Activity_Summary.8b, Activity_Summary.7b, Activity_Summary.6b,
            Activity_Summary.5b, Activity_Summary.4b, Activity_Summary.3b, Activity_Summary.2b,
            Activity_Summary.1b)

#Let's just look at nighttime activity
Activity_Overview.sht<- Activity_Overview %>%
  filter(Temp != "Day")

###PLOT 2###
  #This visualizes all of the binned night activity together on the same plot
  #You can see the variation in the bins and the affect on activity levels
#create vector
mfg_ids <- unique(Activity_Overview$mfgID)

#Loop through vector and save each plot
for (ID in mfg_ids) {
  plot_data <- filter(Act.test, mfgID == ID)

  p <- ggplot(plot_data, aes(x = Night_ID)) +
    geom_line(aes(y = Active, color = as.factor(Bin_Length))) +
    labs(
      title = paste("Minutes Active per Night by Bin Length for COPO:", ID),
      x = "Night",
      y = "Minutes Active",
      color = "Bin Length"
    ) +
    scale_color_viridis_d() +  
    theme_bw() +
    theme(text = element_text(family = "serif", size = 12))
  
  ggsave(filename = paste0("Minutes Active by Night_Overview_COPO ", ID, ".png"), 
         path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures/Summer Activity/8.28.24/Overview", 
         width = 7, 
         height = 5, 
         units = "in")
}


####



####PAST VERSIONS####
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
