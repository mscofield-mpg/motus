##Pulling lunar variables using SunCalc package##

setwd()
library(suncalc)
Sys.setenv(TZ = "UTC")

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
