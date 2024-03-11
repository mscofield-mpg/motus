##UPDATED 3.8.2024##
##Function for SQL extraction##

library(tidyverse)
library(motus)
library(dplyr)
library(lubridate)
library(rnaturalearth)
library(ggmap)
library(DBI)
library(RSQLite)
library(sf)
library(viridis)
library(rgdal) #Look into PROJ and GDAL
library(rgeos) #will want to use library(geos) instead
Sys.setenv(TZ = "UTC")
source("./Scripts/useful_functions.R") #this file should be loaded from your wd

#Load SQL from existing wd. Load species vector with desired codes
sqldb<-tagme(projRecv = 213, new = FALSE, update = FALSE) 
sp.list<-c() 
##COPO = 7750, CONI = 7720, NSWO = 7680, LEOW = 7620, FLOW = 7290, LEWO = 10030, NOFL = 10460

#Function for Extraction
Detection_Extraction<- function(sqldb, sp.list) {
  start_time <- Sys.time()
  #tag metadata
  tagmet<-tbl(sqldb, "tagDeps")
  tags.meta<- tagmet %>%
    select(deployID, tagID, projectID, tsStart, tsEnd, deferSec, speciesID, markerNumber, latitude, longitude, fullID, comments) %>%
    filter(projectID == 213 ) %>%
    filter(speciesID %in% sp.list) %>%
    collect() %>%
    as.data.frame() %>%
    mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
           tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01")) %>%
    distinct() %>%
    filter(!is.na(longitude)) %>%
    group_by(tagID) %>%
    mutate(n = n()) %>%
    ungroup()
  #ambiguous tags
  allambigs<-tbl(sqldb, "allambigs")
  ambig<-allambigs %>%
    collect() %>%
    as.data.frame()
  tags.ambigs<-ambig %>%
    group_by(ambigID) %>%
    mutate(inds = ifelse(any(motusTagID %in% tags.meta$tagID), "yes", "no")) %>%
    filter(inds == "yes") %>%
    ungroup() %>%
    rename(tagID = motusTagID)
  #tag list
  tag.list<-tags.meta %>%
    select(tagID) %>%
    bind_rows(., tags.ambigs %>% select(tagID)) %>%
    distinct() %>% pull()
  #detection extraction
  alltag<- tbl(sqldb, "alltags")
  tags.detect<-alltag %>%
    filter(tagProjID == 213 & motusTagID %in% tag.list & speciesID %in% sp.list) %>%
    select(hitID, runID, batchID, ts, sig, port, noise, freqsd, motusTagID, 
           ambigID, runLen, tagProjID, tagDeployID, tagDeployStart, tagDeployEnd, 
           tagDepLat, tagDepLon, deviceID, recvDeployID, recv,
           speciesID, markerNumber, mfgID, motusFilter, antBearing, antType, antHeight) %>%
    collect() %>%
    as.data.frame() %>%
    mutate(ts = as_datetime(ts),
           tagDeployStart = as_datetime(tagDeployStart),
           tagDeployEnd = as_datetime(tagDeployEnd))
  #receiver metadata
  recvmet<-tbl(sqldb, "recvDeps")
  recv.meta<-recvmet %>%
    collect() %>%
    as.data.frame() %>%
    mutate(tsStart = as_datetime(tsStart),
           tsEnd = as_datetime(tsEnd))
  recv.meta<-recv.meta %>%
    rename(recvDeployID = deployID, recvProjectID = projectID) %>%
    distinct() %>%
    filter(!is.na(longitude)) %>%
    group_by(deviceID) %>%
    mutate(n = n()) %>%
    ungroup()
  #join tables
  recvs<-tags.detect %>%
    select(receiverID = recv, recvDeployID) %>%
    filter(!is.na(recvDeployID)) %>% 
    distinct() %>%
    left_join(., recv.meta) %>%
    select (receiverID, recvDeployID, recvProjectID, stationName, 
            latitude, longitude, receiverType, tsStart, tsEnd, isMobile) %>%
    filter(!is.na(longitude))
  tags.detect<-left_join(tags.detect, recvs %>%
                           select(recv = receiverID, recvDeployID, receiverType, 
                                  recvName = stationName, recvDeployLat = latitude, recvDeployLon = longitude,
                                  isMobile, recvProjID = recvProjectID) %>%
                           distinct())
  #preliminary filters
  tags.detect<-tags.detect %>%
    mutate(date = as.Date(ts), 
           year = as.numeric(format(ts, '%Y')),
           month = as.numeric(format(ts, '%m'))) %>%
    filter(ts>=tagDeployStart & ts <= tagDeployEnd,
           !is.na(recvDeployLat))
  tags.detect<-tags.detect %>%
    mutate(motusFilter = ifelse(freqsd > 0.1, 0, motusFilter))
  tags.detect<-tags.detect %>%
    group_by(motusTagID) %>%
    mutate(af = sum(motusFilter)) %>%
    filter(af > 0) %>%
    ungroup() %>%
    select(-af)
  tag.list.fil<-tags.detect %>%
    select(motusTagID) %>%
    distinct() %>% pull()
  tags.meta<-tags.meta %>%
    filter(tagID %in% tag.list)
  
  return(list(tags.detect = tags.detect, tags.meta = tags.meta, tag.list.fil = tag.list.fil, tags.ambig = tags.ambigs))
  end_time <- Sys.time()
  
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  cat("Elapsed Time: ", format(round(elapsed_time, 2), nsmall = 2), " minutes\n")
  }


Result<- Detection_Extraction(sqldb, sp.list)

Detections <- Result$tags.detect
Tag_metadata<- Result$tags.meta
Filtered_tag_list <- Result$tag.list.fil
Ambiguous_tags <- Result$tags.ambig


write.csv(Detections, file = "Processed Data/SPECIES_tag_detections_UNFILTERED_DATE.csv", row.names = FALSE)
write.csv(Tag_metadata, file = "Processed Data/SPECIES_tag_dep_metadata_DATE.csv", row.names = FALSE)

#manually check deployments
deployments<-Detections %>% select(motusTagID, tagDeployID) %>%
  distinct() %>%
  group_by(motusTagID) %>%
  mutate(n = n()) %>%
  ungroup()
any(deployments$n>1)
#any tags with more than one deployment, you will need to use tagDeployID instead of motusTagID


#Secondary Filtering
Secondary_Filter<- function(Detections) {
  tags.detpro<-Detections
  
  tags.detpro<-tags.detpro %>%
    group_by(date, recvName, motusTagID) %>%
    mutate(n = n(),
           ntrue = sum(motusFilter),
           ptrue = ntrue/n) %>%
    ungroup()
  
  tags.filt<-tags.detpro %>%
    mutate(ftemp = ifelse(ptrue > 0.50, 1, 0),
           motusFilter = ftemp) %>%
    filter(ftemp == 1)
  
  tags.filt.list <- unique(tags.filt$motusTagID)
  
  transit.check<- tags.filt %>%
    do(add_deploy_loc(.)) %>%
    do(site_transit_min(.))
  
  transit.check.suspect<-transit.check %>%
    filter(suspect.transit == "suspect")
  
  tc<-transit.check %>%
    mutate(state = ifelse(dist.min == 0 | rate >= 5, "connected", "not_connected")) %>%
    filter(state == "connected") %>%
    select(motusTagID, ts.x, lat.x, lon.x, lat.y, lon.y)
  
  return(list(tags.filt.list = tags.filt.list, transit.check.suspect = transit.check.suspect, tc = tc,
              tags.filt = tags.filt))
  
}

Filtered_Result<- Secondary_Filter(Detections)

Filtered_Detections <- Filtered_Result$tags.filt
Filtered_tag_list<- Filtered_Result$tags.filt.list
Transit_check<- Filtered_Result$transit.check.suspect
Connections<- Filtered_Result$tc

write.csv(Filtered_Detections, file = "./Processed Data/SPECIES_tag_detections_FILTERED_DATE.csv", row.names = FALSE)


##Further manual filtering still needed for possible false ppositives##






##PREVIOUS VERSION##
#This script is accessing detection data from the Motus SQL


##Download or update the motus SQL for project 213-IWC Birds##

setwd()
library(motus)
library(RSQLite)
Sys.setenv(TZ = "UTC")

#Download the SQL for the first time to your working directory#
  #this will take several hours, it is recommended to have it run overnight
sql.motus <- tagme(projRecv = 213, new = TRUE, update = TRUE)
#enter motus.org username
#enter motus.org password

#To update an existing SQL in your working directory#
sqldb<-tagme(projRecv = 213, new = FALSE, update = TRUE, dir = "./sql-databases")
#enter motus.org username
#enter motus.org password

#To load an existing SQL into your environment from your working directory without updating#
sqldb<-tagme(projRecv = 213, new = FALSE, update = FALSE, dir = "./sql-databases")



##Extract detection data from the SQL##
  #once the SQL is loaded into your environment we can now pull detection data and project metadata

library(tidyverse)

#collect species codes from the species table in the sql
species<-tbl(sqldb, "species") %>%
  collect() %>%
  as.data.frame() 
view(species)

#look up the 'speciesID' for the species of interest for your extraction 
  #enter the numeric code into c(), separate multiple codes with commas
sp.list<-c() 
species<-tbl(sqldb, "species") %>%
  filter(id %in% sp.list) %>% 
  collect() %>%
  as.data.frame()

#now that we have our species data extracted, we can retrieve metadata for those tags
tagmet<-tbl(sqldb, "tagDeps") 

#convert to a dataframe with the species we extracted above
tags.meta<- tagmet %>%
  select(deployID, tagID, projectID, tsStart, tsEnd, deferSec, speciesID, markerNumber, latitude, longitude, fullID, comments) %>%
  filter(projectID == 213 ) %>%
  filter(speciesID %in% sp.list) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(tsStart = as_datetime(tsStart, tz = "UTC", origin = "1970-01-01"),
         tsEnd = as_datetime(tsEnd, tz = "UTC", origin = "1970-01-01")) %>%
  distinct() %>%
  filter(!is.na(longitude)) %>%
  group_by(tagID) %>%
  mutate(n = n()) %>%
  ungroup()

##ambiguous tags##
#we run this check to make sure there are no other tags with the same burst and pulse ID
  #these tags when detected will mirror each other, and detections for both tags will be 
  #recorded. We need to know if any of our tags have ambiguous pairs
allambigs<-tbl(sqldb, "allambigs")
ambig<-allambigs %>%
  collect() %>%
  as.data.frame()
#this will produce a tibble with hopefully no observations. If any ambiguous tags are found
  #they will be listed in the ambig tibble

#if there are ambigs, run this code to check if those tags have associated detection data
tags.ambigs<-ambig %>%
  group_by(ambigID) %>%
  mutate(inds = ifelse(any(motusTagID %in% tags.meta$tagID), "yes", "no")) %>%
  filter(inds == "yes") %>%
  ungroup() %>%
  rename(tagID = motusTagID)

##extract detection data!##
#first make a list of our interested tags using the metadata we pulled earlier
tag.list<-tags.meta %>%
  select(tagID) %>%
  bind_rows(., tags.ambigs %>% select(tagID)) %>%
  distinct() %>% pull()
#you can now subset if you want to filter even more by creating a vector of values using c()

#now we make our alltags view
alltag<- tbl(sqldb, "alltags")

##IMPORTANT: the following step can take several hours especially if the dataset is large
  #always plan ahead, or use small chunks of 5-10 tags at a time
tags.detect<-alltag %>%
  filter(tagProjID == 213 & motusTagID %in% tag.list & speciesID %in% sp.list) %>%
  select(hitID, runID, batchID, ts, sig, port, noise, freqsd, motusTagID, 
         ambigID, runLen, tagProjID, tagDeployID, tagDeployStart, tagDeployEnd, 
         tagDepLat, tagDepLon, deviceID, recvDeployID, recv,
         speciesID, markerNumber, mfgID, motusFilter, antBearing, antType, antHeight) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(ts = as_datetime(ts),
         tagDeployStart = as_datetime(tagDeployStart),
         tagDeployEnd = as_datetime(tagDeployEnd))

#load receiver metadata for all our tag detections
recvmet<-tbl(sqldb, "recvDeps")
recv.meta<-recvmet %>%
  collect() %>%
  as.data.frame() %>%
  mutate(tsStart = as_datetime(tsStart),
         tsEnd = as_datetime(tsEnd))

#then rename the fields and remove any missing locations
recv.meta<-recv.meta %>%
  rename(recvDeployID = deployID, recvProjectID = projectID) %>%
  distinct() %>%
  filter(!is.na(longitude)) %>%
  group_by(deviceID) %>%
  mutate(n = n()) %>%
  ungroup()

#then join with recv info in alltags table
recvs<-tags.detect %>%
  select(receiverID = recv, recvDeployID) %>%
  filter(!is.na(recvDeployID)) %>% 
  distinct() %>%
  left_join(., recv.meta) %>%
  select (receiverID, recvDeployID, recvProjectID, stationName, 
          latitude, longitude, receiverType, tsStart, tsEnd, isMobile) %>%
  filter(!is.na(longitude))

#join again
tags.detect<-left_join(tags.detect, recvs %>%
                         select(recv = receiverID, recvDeployID, receiverType, 
                                recvName = stationName, recvDeployLat = latitude, recvDeployLon = longitude,
                                isMobile, recvProjID = recvProjectID) %>%
                         distinct())

#double check that our tags.detect object correctly joined
view(tags.detect)

##preliminary filters##
  #remove detections that occur outside of deployment time window or if no recv location
  #add some useful variables
tags.detect<-tags.detect %>%
  mutate(date = as.Date(ts), 
         year = as.numeric(format(ts, '%Y')),
         month = as.numeric(format(ts, '%m'))) %>%
  filter(ts>=tagDeployStart & ts <= tagDeployEnd,
         !is.na(recvDeployLat))

#update filter
  #detections with freqsd >0.1 (SG recvs only) as these are generally false
tags.detect<-tags.detect %>%
  mutate(motusFilter = ifelse(freqsd > 0.1, 0, motusFilter))

#filter out birds that have only detections that are likely false
tags.detect<-tags.detect %>%
  group_by(motusTagID) %>%
  mutate(af = sum(motusFilter)) %>%
  filter(af > 0) %>%
  ungroup() %>%
  select(-af)

#update list of tags after basic filter
tag.list<-tags.detect %>%
  select(motusTagID) %>%
  distinct() %>% pull()

#filter tag metadata to only remaining after basic filtering
tags.meta<-tags.meta %>%
  filter(tagID %in% tag.list)

#save a copy of the raw tag detections
  #this saves the file in a folder named Processed Data in your wd
  #modify this file path to fit your directory
write.csv(tags.detect, file = "Processed Data/SPECIES_tag_detections.csv", row.names = FALSE)

#save a copy of the tag metadata
write.csv(tags.meta, file = "Processed Data/SPECIES_tag_dep_metadata.csv", row.names = FALSE)

#the resulting csv of detections is only lightly filtered and will need further manipulation to clean up



##Continue filtering for cleaner detection file##

#check for tags deployed more than once
dtag<-tags.detect %>% select(motusTagID, tagDeployID) %>%
  distinct() %>%
  group_by(motusTagID) %>%
  mutate(n = n()) %>%
  ungroup()
any(dtag$n>1)
#if FALSE, then no tags have been deployed more than once so we can use motusTagID as ID
#if TRUE, need to use a different variable other than motusTagID as an identifier, probably tagDeployID

#calculate proportion of detections classified as 'likely true' by the filter for each tag-recv-day combination
tags.detect<-tags.detect %>%
  group_by(date, recvName, motusTagID) %>%
  mutate(n = n(),
         ntrue = sum(motusFilter),
         ptrue = ntrue/n) %>%
  ungroup()

#list of unique motusTagIDs. Make sure the number of tags matches what you expect
SPECIES.tags<-tags.detect %>% select(motusTagID) %>% arrange(motusTagID) %>% distinct() %>% pull()

##filter out false positives##
#remove detections where less than half considered 'likely true' by filter for each tag-recv-day
  #the 0.50 threshold is suggested but not required, and can be modified to fit project/analysis needs
tags.detect.filt<-tags.detect %>%
  mutate(ftemp = ifelse(ptrue > 0.50, 1, 0),
         motusFilter = ftemp) %>%
  filter(ftemp == 1)

#list of tags that remain after the above filter
tags.filt.list<-tags.detect.filt %>%
  select(motusTagID) %>%
  arrange(motusTagID) %>%
  distinct() %>%
  pull()
#if you lose tags here, make note of that tagID so you can double check manually

##Visualize detection data for final filtering##
  #this step is not necessary, but can help with double checking that detections are biologically possible 

library(rnaturalearth)
library(ggmap)
library(sf)
library(viridis)
library(PROJ)
library(geos)
#a file containing useful functions, a copy of which is stored on the cloud
source("./Scripts/useful_functions.R") #this file should be loaded from your wd

#setting the theme for mapping with ggmap
theme_set(theme_bw())
world<-ne_countries(scale = "medium", returnclass = "sf")

#to download a shapefile for the first time use this code
  #this directory is set to download into a folder named Map Data in your wd
  #you can change or modify this directory as needed
lakes<-ne_download(scale = "medium", type = 'lakes', category = 'physical',
                   returnclass = "sf", destdir = "./Map Data/lakes")

#to load an already downloaded shapefile use this code
lakes<-ne_load(type = "lakes", scale = "medium" , category = 'physical',
               returnclass = "sf",
               destdir = paste0("./Map Data/lakes"))

##calculate site transitions##
#check for birds with weird movements (e.g. too fast, 'wrong direction')
transit.check<- tags.detect.filt %>%
  do(add_deploy_loc(.)) %>%
  do(site_transit_min(.))

transit.check.suspect<-transit.check %>%
  filter(suspect.transit == "suspect")
#if any records show up in this df, these are movements the computer thinks could be suspicious
#those would need to be manually removed or inspected

#classify transitions as connected, or not_connected
tc<-transit.check %>%
  mutate(state = ifelse(dist.min == 0 | rate >= 5, "connected", "not_connected")) %>%
  filter(state == "connected") %>%
  select(motusTagID, ts.x, lat.x, lon.x, lat.y, lon.y)
#we have now filtered out transitions that are suspicious or not_connected

##diagnostic visualizations##
#map filtered detections and save maps for each bird/tag as a png
#set limits to map based on locations of detections
#simplify detection data for plotting
tags.path.filt<-fun.getpath(tags.detect.filt)
for( i in 1:length(tags.filt.list)){
  bird<- tags.path.filt %>% filter(motusTagID == tags.filt.list[i])
  pbird<-tc %>% filter(motusTagID == tags.filt.list[i]) %>%
    distinct()
  xmin<-min(bird$recvDeployLon, bird$tagDepLon, na.rm = TRUE) - 2
  xmax<-max(bird$recvDeployLon, bird$tagDepLon, na.rm = TRUE) + 2
  ymin<-min(bird$recvDeployLat, bird$tagDepLat, na.rm = TRUE) - 1
  ymax<-max(bird$recvDeployLat, bird$tagDepLat, na.rm = TRUE) + 1
  png(filename = paste0("./Figures/Diagnostic Plots/SPECIES/", bird$motusTagID, "-","map.png"),
      width = 8, height = 8, units = "in", res = 600)
  print(ggplot(data = world) +
          geom_sf() +
          geom_sf(data =lakes, fill = "white") +
          coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)+
          geom_point(data = bird, aes(recvDeployLon, recvDeployLat, colour = month), alpha = 0.5, size = 5) +
          geom_segment(data = pbird, aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y),
                       linetype = "dashed",
                       arrow = arrow(angle = 18, type = "closed", length = unit(0.1, "inches"))) +
          geom_point(data = bird,
                     aes(tagDepLon, tagDepLat), colour = "red",
                     shape = 8, size = 4) +
          scale_color_gradientn(colors = rev(viridis_pal()(12)), limits = c(1, 12)) +
          ggtitle(paste0("motusTagID:", bird$motusTagID[1])))
  dev.off()
}
rm(i)

#make signal strength plots
for (i in 1:length(tags.filt.list)){
  png(filename = paste0("./Figures/Diagnostic Plots/SPECIES/", "sigstrength-",
                        tags.filt.list[i], ".png"),
      width = 11, height = 7.5, units = "in", res=600)
  print(plotTagSig_mod(tags.detect.filt, motusTagID = tags.filt.list[i]))
  dev.off()
}
rm(i)
#these plots when viewed side by side work to quickly show odd detections that may have
  #missed by the above filters


#to zoom in closer on a detection at a single station (especially one that appears false)
#we are looking for a plausible detection curve, where we can see the signal strength
  #arc as the tag passes the station
plotTagSig_mod(tags.detect.filt %>% filter(recvName == "ENTER NAME HERE"), motusTagID = ENTER TAG ID HERE)
#EXAMPLE: plotTagSig_mod(tags.detect.filt %>% filter(recvName == "Barb Garten-MT"), motusTagID = 47481)
#EXAMPLE: plotTagSig_mod(tags.detect.filt %>% filter(recvName == "Minidoka National Wildlife Refuge, ID"), motusTagID = 60855)

#remove false positives identified by the above diagnostic visualizations
tags.filt2<-tags.detect.filt %>%
  filter(!recvName == "ENTER recv NAME HERE")

#write csv to keep track of issues spotted via visual inspection
#this file is for writing notes about false positives in excel
write.csv(tags.detect.filt %>% select(motusTagID) %>% distinct() %>% arrange(motusTagID),
          file = "./Figures/Diagnostic Plots/filtering_notes_SPECIEStags_round1.csv", row.names = FALSE)

##SAVE YOUR CLEAN DETECTION DATA##
#once you've done your manual inspection and final filtering, save your clean data for analysis
#write rds file of clean detection data after the basic filter has been run
saveRDS(tags.filt2, file = "./Processed Data/SPECIES-tag-detections-clean.rds")
write.csv(tags.filt2, file = "./Processed Data/ SPECIES-tag-detections-FILTERED.csv", row.names = FALSE) #recommended



