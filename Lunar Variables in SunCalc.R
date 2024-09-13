###### CALLING LUNAR VARIABLES AND JOINING NUMERIC TABLES#######


#######Load Packages##########
library(suncalc)
library(dplyr)
library(lubridate)

#######LOAD FUNCTIONS###########

#pull moon rise/set times and sun rise/set times
get_moon_times<- function(Date) {
  
  moon.time<-getMoonTimes(date = Date, 
                          lat = 46.7, lon = -113.9, 
                          data = NULL, 
                          keep = c("rise", "set"),
                          tz = "America/Denver",
                          inUTC = FALSE)
  
  sun.time<-getSunlightTimes(date = Date, 
                             lat = 46.7,
                             lon = -113.9,
                             data = NULL,
                             keep = c("sunset", "dusk","nauticalDusk", "nauticalDawn", 
                                      "dawn", "sunrise"),
                             tz = "America/Denver")
  
  Night.dat<-moon.time %>%
    left_join(sun.time, by = "date")
  
  
  Night.dat<-rename(Night.dat, moonrise = "rise", moonset = "set")
  
  return(Night.dat)
}

#pull moon illumination and position variables and join with moon/sun time variables
get_moon_variables <- function(Date.time) {
  
  moon.ill <- getMoonIllumination(date = Date.time, keep = c("fraction", "phase", "angle"))
  moon.pos <- getMoonPosition(date = Date.time, lat = 46.7, lon = -113.9, data = NULL, keep = c("altitude", "azimuth", "parallacticAngle"))
  
  moon.dat <- moon.ill %>%
    left_join(moon.pos, by = "date") %>%
    mutate(time_bin = date) %>%
    select(-date, -lat, -lon)
  
  moon.dat <- moon.dat %>%
    mutate(time_bin = force_tz(time_bin, tz = "America/Denver")) %>%
    left_join(Night.dat, join_by(closest(time_bin >= sunset))) %>%
    filter(!is.na(Night_ID))
  
  moon.dat <- moon.dat %>%
    arrange(Night_ID, time_bin) %>%
    group_by(Night_ID) %>%
    mutate(tb_position = row_number() - 1) %>%
    ungroup()
  
  moon.dat <- moon.dat %>%
    select(time_bin, Night_ID, tb_position, everything()) %>%
    select(-date)
  
  return(moon.dat)
}

#calculate moon availability and moon duration
moon_info <- function(df, time_bin_duration_minutes) {
  # Initialize columns
  df$moon_available <- 0
  df$moon_duration <- 0
  
  # Loop through each row in the data frame
  for (i in 1:nrow(df)) {
    moonrise_time <- df$moonrise[i]
    moonset_time <- df$moonset[i]
    time_bin_start <- df$time_bin[i]
    time_bin_end <- time_bin_start + (time_bin_duration_minutes * 60)
    
    # Check if moon is available during the time_bin
    if (!is.na(moonrise_time) && !is.na(moonset_time)) {
      if ((time_bin_start >= moonrise_time && time_bin_end <= moonset_time) ||
          (time_bin_start <= moonrise_time && time_bin_end >= moonrise_time) ||
          (time_bin_start <= moonset_time && time_bin_end >= moonset_time)) {
        df$moon_available[i] <- 1
      }
      
      # Calculate moon duration within the time_bin
      start_time <- max(time_bin_start, moonrise_time)
      end_time <- min(time_bin_end, moonset_time)
      duration_minutes <- as.numeric(difftime(end_time, start_time, units = "mins"))
      duration_minutes <- max(0, duration_minutes)
      
      # Assign the calculated duration to the moon_duration column
      df$moon_duration[i] <- duration_minutes
    }
  }
  
  return(df)
}


######## NIGHT DATA MANIPULATION#########

#set Date factor used in get_moon_times function
Date<-as.Date(as.Date("2023-08-15"):as.Date("2024-01-02"), origin = "1970-01-01")

#function call
Night.dat<-get_moon_times(Date)

#save as csv
write.csv(Night.dat, "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/Motus/Project 213 Data/Projects/COPO_Lunar Breeding Season/Data Tables/Moon Times 2024.csv", row.names = FALSE)

#Pulled into excel, minor manipulations I haven't figured out how to do in R yet
  #added Night_ID
  #adjusted sunrise dates to align with previous sunset (Night_start = sunset, Night_end = following sunrise)
  #fixed moonrise/moonset times to align with start_time as sunset instead of midnight
#save as csv

#Read in adjusted Night variables after Excel modifications
Night.dat<- read_csv("~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/Motus/Project 213 Data/Projects/COPO_Lunar Breeding Season/Data Tables/Moon Times 2024.csv")

#reformat date time variables
Night.dat <- Night.dat %>%
  mutate(across(c(moonrise, moonset, sunset, dusk, nauticalDusk, nauticalDawn, dawn, sunrise),
                ~ as.POSIXct(trimws(.), format = "%m/%d/%y %H:%M"))) 

Night.dat$date<-as.Date(Night.dat$date, format = "%m/%d/%y")

#calculate length of "Night" in minutes (dusk-dawn)
Night.dat<- Night.dat %>%
  mutate(night_mins = abs(as.numeric(difftime(dusk, dawn, tz = "America/Denver", units = "mins"))))

############ LUNAR ILLUMINATION AND POSITION VARIABLES ############

#set Date.time vector used in get_moon_variables function
  #be sure to adjust the time bin as needed
Date.time <- seq(ISOdate(2023, 8, 15), ISOdate(2024, 01, 01), "15 mins")

#call function
moon.dat <- get_moon_variables(Date.time)

#call second function
  #be sure to adjust time_bin_duration to match the Date.time vector
moon.dat <- moon_info(moon.dat, time_bin_duration_minutes = 30)

#add time_bin_ID (numeric)
moon.dat<- moon.dat %>%
  arrange(time_bin) %>%
  mutate(tb_ID = row_number()) 

#select time_bin and numeric fields for future joining with activity/detection table
moon.numeric<- moon.dat %>%
  select(Night_ID, time_bin, tb_ID, tb_position, night_mins, moon_available, moon_duration, fraction, phase, angle,
         altitude, azimuth, parallacticAngle)

#save both tables as csv
write.csv(moon.dat, "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/Motus/Project 213 Data/Projects/COPO_Lunar Breeding Season/Data Tables/30min Moon Variables 2024.csv", row.names = FALSE)
write.csv(moon.numeric, "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/Motus/Project 213 Data/Projects/COPO_Lunar Breeding Season/Data Tables/30min Numeric Moon Variables 2024.csv", row.names = FALSE)













####OUTDATED VERSIONS BELOW#####
##Pulling lunar variables using SunCalc package##

setwd()
library(suncalc)
library(lubridate)
library(dplyr)

#lunar and solar rise and set times#
#change the date vector to reflect desired date range
get_moon_times<- function() {
  Date<-as.Date(as.Date("2020-08-15"):as.Date("2021-01-01"), origin = "1970-01-01")

  moon.time<-getMoonTimes(date = Date, 
                          lat = 46.7, lon = -113.9, 
                          data = NULL, 
                          keep = c("rise", "set"),
                          tz = "America/Denver",
                          inUTC = FALSE)

  sun.time<-getSunlightTimes(date = Date, 
                                lat = 46.7,
                                lon = -113.9,
                                data = NULL,
                                keep = c("sunset", "dusk","nauticalDusk", "nauticalDawn", 
                                         "dawn", "sunrise"),
                                tz = "America/Denver")

  Night.dat<-moon.time %>%
  left_join(sun.time, by = "date")


  Night.dat<-rename(Night.dat, moonrise = "rise", moonset = "set")
  
  return(Night.dat)
}
Night.dat<-get_moon_times()

write.csv(Night.dat, "./Lunar/Moon Data/2020/Moon Times 2020.csv", row.names = FALSE)
#Pulled into excel, minor manipulation I haven't figured out how to do in R yet
#re-loaded into R, convert all date time variables back into POSIXct format

Night.dat<- read_csv("./Lunar/Moon Data/2023/Moon Times 2023.csv")

Night.dat <- Night.dat %>%
  mutate(across(c(moonrise, moonset, sunset, dusk, nauticalDusk, nauticalDawn, dawn, sunrise),
                ~ as.POSIXct(trimws(.), format = "%m/%d/%y %H:%M"))) 
Night.dat$date<-as.Date(Night.dat$date, format = "%m/%d/%y")

#Minor edits, needed to split these again so I can manipulate a few moon variables
Night.moon<- Night.dat %>%
  select(date, moonrise, moonset)

Night.time<- Night.dat %>%
  select(Night_ID, sunset, dusk, nauticalDusk, nauticalDawn, dawn, sunrise)

Night.time<- Night.time %>%
  mutate(night_mins = abs(as.numeric(difftime(dusk, dawn, tz = "America/Denver", units = "mins"))))

##Function to pull numeric lunar variables and join with time variables in time_bins
#Function for illumination and position variables
#Change time_bin (5 mins, 10 mins, 15 mins, 30 mins, hours etc) as needed in Date.time vector
get_moon_variables <- function() {
  Date.time <- seq(ISOdate(2020, 8, 15), ISOdate(2021, 01, 01), "05 mins")
  
  moon.ill <- getMoonIllumination(date = Date.time, keep = c("fraction", "phase", "angle"))
  moon.pos <- getMoonPosition(date = Date.time, lat = 46.7, lon = -113.9, data = NULL, keep = c("altitude", "azimuth", "parallacticAngle"))
  
  moon.dat <- moon.ill %>%
    left_join(moon.pos, by = "date") %>%
    mutate(time_bin = date) %>%
    select(-date, -lat, -lon)
  
  moon.dat <- moon.dat %>%
    mutate(time_bin = force_tz(time_bin, tz = "America/Denver")) %>%
    left_join(Night.time, join_by(closest(time_bin >= sunset))) %>%
    filter(time_bin >= sunset & time_bin <= sunrise) %>%
    filter(!is.na(Night_ID))
  
  moon.dat <- moon.dat %>%
    left_join(Night.moon, join_by(closest(time_bin >= date)))
  
  moon.dat <- moon.dat %>%
    arrange(Night_ID, time_bin) %>%
    group_by(Night_ID) %>%
    mutate(tb_position = row_number() - 1) %>%
    ungroup()
  
  moon.dat <- moon.dat %>%
    select(time_bin, Night_ID, tb_position, everything()) %>%
    select(-date)
  
  return(moon.dat)
}


moon.dat <- get_moon_variables()

##Function for calculating Lunar Availability variables

moon_info <- function(df, time_bin_duration_minutes) {
  # Initialize columns
  df$moon_available <- 0
  df$moon_duration <- 0
  
  # Loop through each row in the data frame
  for (i in 1:nrow(df)) {
    moonrise_time <- df$moonrise[i]
    moonset_time <- df$moonset[i]
    time_bin_start <- df$time_bin[i]
    time_bin_end <- time_bin_start + (time_bin_duration_minutes * 60)
    
    # Check if moon is available during the time_bin
    if (!is.na(moonrise_time) && !is.na(moonset_time)) {
      if ((time_bin_start >= moonrise_time && time_bin_end <= moonset_time) ||
          (time_bin_start <= moonrise_time && time_bin_end >= moonrise_time) ||
          (time_bin_start <= moonset_time && time_bin_end >= moonset_time)) {
        df$moon_available[i] <- 1
      }
      
      # Calculate moon duration within the time_bin
      start_time <- max(time_bin_start, moonrise_time)
      end_time <- min(time_bin_end, moonset_time)
      duration_minutes <- as.numeric(difftime(end_time, start_time, units = "mins"))
      duration_minutes <- max(0, duration_minutes)
      
      # Assign the calculated duration to the moon_duration column
      df$moon_duration[i] <- duration_minutes
    }
  }
  
  return(df)
}

# Example usage with a time_bin duration of 5 minutes
#change time_bin_duration_minutes to length of time_bin (5, 10, 15, or 30 mins)
moon.dat <- moon_info(moon.dat, time_bin_duration_minutes = 05)

#Lastly, add numeric row_numbers, then select only numeric fields and save for modeling!
moon.dat<- moon.dat %>%
  arrange(time_bin) %>%
  mutate(tb_ID = row_number()) 

moon.numeric<- moon.dat %>%
  select(Night_ID, tb_ID, tb_position, night_mins, moon_available, moon_duration, fraction, phase, angle,
         altitude, azimuth, parallacticAngle)

write.csv(moon.dat, "./Lunar/Moon Data/2020/5min_Moon Variables.csv", row.names = FALSE)
write.csv(moon.numeric, "./Lunar/Moon Data/2020/5min_Numeric_Moon Variables.csv", row.names = FALSE)






#Function for illumination and position variables
#Change time_bin (5 mins, 10 mins, 15 mins, 30 mins, hours etc) as needed in Date.time vector
get_moon_variables <- function() {
  Date.time <- seq(ISOdate(2023, 8, 15), ISOdate(2024, 01, 01), "10 mins")
  
  moon.ill <- getMoonIllumination(date = Date.time, keep = c("fraction", "phase", "angle"))
  moon.pos <- getMoonPosition(date = Date.time, lat = 46.7, lon = -113.9, data = NULL, keep = c("altitude", "azimuth", "parallacticAngle"))
  
  moon.dat <- moon.ill %>%
    left_join(moon.pos, by = "date") %>%
    mutate(time_bin = date) %>%
    select(-date, -lat, -lon)
 
  moon.dat <- moon.dat %>%
    mutate(time_bin = force_tz(time_bin, tz = "America/Denver")) %>%
    left_join(Night.dat, join_by(closest(time_bin >= dusk))) %>%
    filter(!is.na(Night_ID))
  
  moon.dat<- moon.dat %>%
    select(time_bin, Night_ID, everything()) %>%
    select(-date)
  
  return(moon.dat)
}

moon.dat <- get_moon_variables()

write.csv(moon.dat, "./Lunar/Moon Data/2023/10min_Moon Variables.csv", row.names = FALSE)


#If we want to filter for just the night hours (between dusk and dawn)
moon.dat2<- moon.dat %>%
  filter(time_bin >= dusk & time_bin <= dawn)


#######
get_moon_variables <- function() {
  Date.time <- seq(ISOdate(2023, 8, 15), ISOdate(2024, 01, 01), "10 mins")
  
  moon.ill <- getMoonIllumination(date = Date.time, keep = c("fraction", "phase", "angle"))
  moon.pos <- getMoonPosition(date = Date.time, lat = 46.7, lon = -113.9, data = NULL, keep = c("altitude", "azimuth", "parallacticAngle"))
  
  moon.dat <- moon.ill %>%
    left_join(moon.pos, by = "date") %>%
    mutate(date_Local = as.Date(date),
           time_bin = date)
  
  moon.dat <- moon.dat %>%
    inner_join(Night.dat, by = "date_Local") %>%
    select(date_Local, time_bin, Night_ID, everything()) %>%
    select(-lat, -lon, -date)
  
  return(moon.dat)
}

moon.dat <- get_moon_variables()


#create a vector of dates during time of interest, enter desired time bins (i.e. 'hours")
date<-seq(ISOdate(2023,6,1), ISOdate(2023,6,30), "hours")
##OR##
date<-as.Date(as.Date("2023-06-01"):as.Date("2023-07-31"), origin = "1970-01-01")

#Lunar variables for phase and illumination
moon.ill<-getMoonIllumination(date = date, keep = c("fraction", "phase", "angle"))
  #"fraction": illuminated fraction of the moon; varies from 0.0(new moon) to 1.0(full moon)
  #"phase": moon phase; varies from 0.0 (new moon) to 1.0(new moon)
                  #0 = new moon
                  #waxing crescent
                  #0.25 = first quarter
                  # waxing gibbous
                  #0.5 = full moon
                  #waning gibbous
                  #0.75 = last quarter
                  #waning crescent
  #"angle": midpoint angle in radians of the illuminated limb of the moon reckoned
      #eastward from the north point of the disk 
      #the moon is waxing if the angle is negative
      #the moon is waning if the angle is positive


#Lunar variables for position in the sky
moon.pos<-getMoonPosition( date = date, lat = 46.7, lon = -113.9, data = NULL, keep = c("altitude",
                                                                              "azimuth",
                                                                              "distance",
                                                                              "parallacticAngle"))
  #"altitude": moon altitude above the horizon in radians
  #"azimuth": moon azimuth in radians
  #"distance": distance to moon in kilometers
  #"parallacticAngle": parallactic angle of the moon in radians


##Using the second date vector##
#Lunar variables for moonrise and moonset
moon.time<-getMoonTimes(date = date, lat = 46.7, lon = -113.9, data = NULL, keep = c("rise",
                                                                                     "set",
                                                                                     "alwaysUp",
                                                                                     "alwaysDown"),
                        tz = "UTC", inUTC = FALSE)
#"rise": moonrise time
#"set": moonset time
#"alwaysUp": TRUE if the moon never rises or sets and is always above the horizon during the day
#"alwaysDown": TRUE if the moon is always below the horizon


#Solar varibles for sunrise, sunset and twilight times
sun.dat<-getSunlightTimes(date = date, lat = 46.7, lon = -113.9, data = NULL, keep = c("sunset", "dusk", "nauticalDusk", 
                                                                                        "night", "nadir", "nightEnd", "nauticalDawn", "dawn"),
                          tz = "UTC")

##Join variables into one dataframe##
moon.dat<-moon.ill %>%
left_join(moon.pos, by = "date") 

moon.time<-moon.time %>%
left_join(sun.dat, by ="date")

#save dataframes as csv just in case
write.csv(moon.dat, file = "./Moonlight_hourly_DATES.csv", row.names = FALSE)
write.csv(moon.time, file = "./Moon_daily_time_DATES.csv", row.names = FALSE)



###Lunar Functions###

Date.2020<-as.Date(as.Date("2020-08-15"):as.Date("2020-12-31"), origin = "1970-01-01")

moon.time.2020<-getMoonTimes(date = Date.2020, lat = 46.7, lon = -113.9, data = NULL, keep = c("rise",
                                                                                               "set"),
                             tz = "UTC",
                             inUTC = TRUE)
sun.time.2020<-getSunlightTimes(date = Date.2020, 
                                lat = 46.7,
                                lon = -113.9,
                                data = NULL,
                                keep = c("sunset", "dusk", "dawn", "sunrise"),
                                tz = "UTC")
Night.dat<-moon.time.2020 %>%
  left_join(sun.time.2020, by = "date")

Night.dat<-rename(Night.dat, moonrise = "rise", moonset = "set")
Night.dat.sel<-Night.dat%>%
  select("date", "moonrise", "moonset", "sunset", "dusk", "dawn", "sunrise")

# Create a function to check if the moon is available during the night
moon_available <- function(night_start, night_end, moonrise, moonset) {
  # Check if the moon is up at the start of the night or rises during the night
  moon_up_at_start <- !is.na(moonrise) && moonrise <= night_start && (is.na(moonset) || moonset >= night_start)
  
  # Check if the moon is up at any point during the night
  moon_up_during_night <- !is.na(moonrise) && moonrise >= night_start && moonrise <= night_end
  
  # Check if the moon is still up at the end of the night or sets during the night
  moon_up_at_end <- !is.na(moonset) && moonset <= night_end && (is.na(moonrise) || moonrise <= night_end)
  
  # The moon is available if any of the above conditions are met
  return(moon_up_at_start || moon_up_during_night || moon_up_at_end)
}


# Create a function to assign night IDs and moon availability
create_night_variables <- function(row) {
  dusk <- row$dusk
  dawn <- row$dawn
  
  # Assign a unique night ID based on the date
  night_id <- as.numeric(difftime(row$date, min(Night.dat.sel$date), units = "days")) + 1
  
  # Check if the moon is available during the night
  moon_avail <- moon_available(dusk, dawn, row$moonrise, row$moonset)
  
  return(data.frame(Night_ID = night_id, Moon_Availability = moon_avail))
}

# Apply the function to each row of the data frame
Night.dat.new <- Night.dat.sel %>%
  rowwise() %>%
  do(create_night_variables(.))

Night.dat.sel<-cbind(Night.dat.sel, Night_ID = Night.dat.new$Night_ID, Moon_Availability = Night.dat.new$Moon_Availability)
