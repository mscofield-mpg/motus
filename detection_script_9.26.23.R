##NSWO Migration Exploration##
  #initial pull and organization#

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
library(geos) #will want to use library(geos) instead
Sys.setenv(TZ = "UTC")

sqldb<-tagme(projRecv = 213, new = FALSE, update = FALSE, dir = "./sql-databases")

species<-tbl(sqldb, "species") %>%
  collect() %>%
  as.data.frame() 
sp.list<-c(7680)#NSWO only 

species<-tbl(sqldb, "species") %>%
  filter(id %in% sp.list) %>% 
  collect() %>%
  as.data.frame()

tagmet<-tbl(sqldb, "tagDeps") #retrieve tag metadata
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
rm(tags.ambigs)
rm(allambigs, ambig)


tag.list<-tags.meta %>%
  select(tagID) %>%
  bind_rows(., tags.ambigs %>% select(tagID)) %>%
  distinct() %>% pull() #complete list of NSWO tags in database under our project
tag.list

#updated as of 10/6/23
fall.migrants.NSWO<-c(60869, 60871, 60872, 47486, 47487, 47488, 60879, 47489, 47456, 47457, 47458, 47459, 47460,
                      47461, 47462, 47463,47464, 47465, 60891, 47466, 60892, 47467, 47468, 60894, 47469, 47470, 
                      60902, 60903, 60904, 60909, 60912, 60913, 54585, 54586, 54587, 54589, 54592, 54595, 54596,
                      54598, 54599, 54600, 54601, 54602, 54605, 54606, 54607, 54579, 54580, 54218, 54581, 54213,
                      54582, 54583, 55127, 55129, 55131, 55133, 55135, 55136, 55138, 55139, 55140, 55142, 55143,
                      55146, 55148, 55149, 55150, 55151, 55152, 55153, 55154, 55155, 54251, 36406, 55102, 55105,
                      55109, 55120, 55110, 55117, 55123, 55124, 47491, 47492, 47493, 47494, 47495, 47496, 47497,
                      47498, 47499, 47500, 47501, 47502, 47503, 47504, 47505, 47506, 47507, 71255, 47508, 47509, 
                      71257, 47511, 71260, 47512, 47513, 47514, 71263, 71264, 71265, 71266, 71267, 71268, 71269,
                      71273, 71295, 71296, 71300, 59153, 59154, 59173, 59155, 59158, 59159, 76511, 76512, 59272,
                      76515, 59172, 59176, 59273, 59274, 58874, 58875, 59275, 58876, 58877, 58878, 59276, 58880, 
                      75812)

overwinter.migrants.NSWO<-c(47457,47469, 54586, 54606, 54213, 54579, 55152, 36406, 55109, 55117, 47491, 47492, 
                            47495, 47498, 47502, 47505, 59173, 59156, 58875, 58876, 58877, 47513,71263, 59176)

spring.migrants.NSWO<-c(60894, 60912, 54586, 54596, 54599, 54602, 54605, 54606, 54582, 55136, 55146, 55152, 
                        55102, 55109, 55123, 47497, 47502, 47506, 71255, 47512, 47514, 71266, 71273, 59173,
                        59176, 58875, 59275, 58876, 58877, 47491,47495, 47505,47513, 71263, 59156)

summer.detections.NSWO<-c(47469, 60902, 54596, 54602, 54606, 54579, 54218, 55152, 55102, 55123, 47491, 47513,
                          71263, 71266, 59161, 76508, 58875, 58877)
year.resident.NSWO<-c(54603, 47513, 47514)
#use these tag lists for each group, parsed by type of movement detections we have

alltag<- tbl(sqldb, "alltags")
fall.migrant.detections<-alltag %>%
  filter(tagProjID == 213 & motusTagID %in% fall.migrants.NSWO & speciesID %in% sp.list) %>%
  select(hitID, runID, batchID, ts, sig, port, noise, freqsd, motusTagID, 
         ambigID, runLen, tagProjID, tagDeployID, tagDeployStart, tagDeployEnd, 
         tagDepLat, tagDepLon, deviceID, recvDeployID, recv,
         speciesID, markerNumber, mfgID, motusFilter, antBearing, antType, antHeight) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(ts = as_datetime(ts),
         tagDeployStart = as_datetime(tagDeployStart),
         tagDeployEnd = as_datetime(tagDeployEnd))

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

recvs<-fall.migrant.detections %>%
  select(receiverID = recv, recvDeployID) %>%
  filter(!is.na(recvDeployID)) %>% 
  distinct() %>%
  left_join(., recv.meta) %>%
  select (receiverID, recvDeployID, recvProjectID, stationName, 
          latitude, longitude, receiverType, tsStart, tsEnd, isMobile) %>%
  filter(!is.na(longitude))

fall.migrant.detections<-left_join(fall.migrant.detections, recvs %>%
                         select(recv = receiverID, recvDeployID, receiverType, 
                                recvName = stationName, recvDeployLat = latitude, recvDeployLon = longitude,
                                isMobile, recvProjID = recvProjectID) %>%
                         distinct())
view(fall.migrant.detections)

write.csv(fall.migrant.detections, "./NSWO Migration/Fall.migration.detections.unfiltered_092623.csv", row.names =  FALSE)

rm(allambigs, alltag, ambig, recv.meta, recvmet, recvs, tagmet, tags.ambigs, tags.meta)
##prelim filters##

fall.migrant.detections<-fall.migrant.detections %>%
  mutate(date = as.Date(ts), 
         year = as.numeric(format(ts, '%Y')),
         month = as.numeric(format(ts, '%m'))) %>%
  filter(ts>=tagDeployStart & ts <= tagDeployEnd,
         !is.na(recvDeployLat))

fall.migrant.detections<-fall.migrant.detections %>%
  mutate(motusFilter = ifelse(freqsd > 0.1, 0, motusFilter))

fall.migrant.detections<-fall.migrant.detections %>%
  group_by(motusTagID) %>%
  mutate(af = sum(motusFilter)) %>%
  filter(af > 0) %>%
  ungroup() %>%
  select(-af)

fall.migrants.NSWO.filtered<-fall.migrant.detections %>%
  select(motusTagID) %>%
  distinct() %>% pull()

write.csv(fall.migrant.detections, "./NSWO Migration/Fall.migration.detections.FILTERED_092723.csv", row.names =  FALSE)

##Secondary filtering##

deploy.tags.NSWO<-fall.migrant.detections %>% select(motusTagID, tagDeployID) %>%
  distinct() %>%
  group_by(motusTagID) %>%
  mutate(n = n()) %>%
  ungroup()
any(deploy.tags.NSWO$n>1)

fall.migrant.detections.FILT<-fall.migrant.detections %>%
  group_by(date, recvName, motusTagID) %>%
  mutate(n = n(),
         ntrue = sum(motusFilter),
         ptrue = ntrue/n) %>%
  ungroup()

fall.NSWO.tags<-fall.migrant.detections.FILT %>% 
  select(motusTagID) %>% 
  arrange(motusTagID)%>% 
  distinct() %>% pull()

fall.migrant.detections.FILT<-fall.migrant.detections.FILT %>%
  mutate(ftemp = ifelse(ptrue > 0.50, 1, 0),
         motusFilter = ftemp) %>%
  filter(ftemp == 1)

fall.NSWO.tags<-fall.migrant.detections.FILT %>%
  select(motusTagID) %>%
  arrange(motusTagID) %>%
  distinct() %>%
  pull()

##we lost no tags to secondary filters##
#let's see if we can visualize some of these tags#

transit.check<- fall.migrant.detections.FILT %>%
  do(add_deploy_loc(.)) %>%
  do(site_transit_min(.))

tc<-transit.check %>%
  mutate(state = ifelse(dist.min == 0 | rate >= 5, "connected", "not_connected")) %>%
  filter(state == "connected") %>%
  select(motusTagID, ts.x, lat.x, lon.x, lat.y, lon.y)


migrant.path.filt<-fun.getpath(fall.migrant.detections.FILT)
for( i in 1:length(fall.NSWO.tags)){
  bird<- migrant.path.filt %>% filter(motusTagID == fall.NSWO.tags[i])
  pbird<-tc %>% filter(motusTagID == fall.NSWO.tags[i]) %>%
    distinct()
  xmin<-min(bird$recvDeployLon, bird$tagDepLon, na.rm = TRUE) - 2
  xmax<-max(bird$recvDeployLon, bird$tagDepLon, na.rm = TRUE) + 2
  ymin<-min(bird$recvDeployLat, bird$tagDepLat, na.rm = TRUE) - 1
  ymax<-max(bird$recvDeployLat, bird$tagDepLat, na.rm = TRUE) + 1
  png(filename = paste0("./Figures/Diagnostic Plots/NSWO/", fall.NSWO.tags[i], "-", "map.png"),
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
for (i in 1:length(fall.NSWO.tags)){
  png(filename = paste0("./Figures/Diagnostic Plots/NSWO/", "sigstrength-",
                       fall.NSWO.tags[i], ".png"),
      width = 11, height = 7.5, units = "in", res=600)
  print(plotTagSig_mod(fall.migrant.detections.FILT, motusTagID = fall.NSWO.tags[i]))
  dev.off()
}
rm(i)

##ok let's go through the plots and manually check for any false detections we need to remove##
#i see a couple#

plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "ShuswapBandLands"), motusTagID = 54218)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "GB-Tadenac"), motusTagID = 47459)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "GB-Tadenac"), motusTagID = 47458)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "Jackfish Lake"), motusTagID = 47468)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "GB-Go Home"), motusTagID = 47486)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "Nashlyn"), motusTagID = 54579)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "GFAFB - Lagoons"), motusTagID = 54589)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "Portage Mountain Quarry"), motusTagID = 54605)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "Hansen-Paterson Farm"), motusTagID = 55146)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "Newgate - Ambrose's"), motusTagID = 58877)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "Torwood"), motusTagID = 59173)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "Newgate - Ambrose's"), motusTagID = 60912)
plotTagSig_mod(fall.migrant.detections.FILT %>% filter(recvName == "ColonyFarmRegionalPark"), motusTagID = 71269)

NSWO.54218<-fall.migrant.detections.FILT %>%
  filter(motusTagID == 54218) %>%
  filter(recvName == "ShuswapBandLands")
##Likely True##

NSWO.47459<- fall.migrant.detections.FILT %>%
  filter(motusTagID == 47459) %>%
  filter(recvName == "GB-Tadenac")
##I'm leaning likely false##
#it has a high noise value and a shorter runLen value, and the signal arc doesn't give me much confidence


NSWO.47458<- fall.migrant.detections.FILT %>%
  filter(motusTagID == 47458) %>%
  filter(recvName == "GB-Tadenac")
##Might actually be true##
#this has higher runLen values and slightly lower noise values and the signal arc looks more similar to 
  #likely true detections than the 47459 arc...

NSWO.47468<-fall.migrant.detections.FILT %>%
  filter(motusTagID == 47468) %>%
  filter(recvName == "Jackfish Lake")
##I really don't know what to say##
#this has a high noise value and a three weird runLens... the arc doesnt look right either...

NSWO.47486<-fall.migrant.detections.FILT %>%
  filter(motusTagID == 47486) %>%
  filter(recvName == "GB-Go Home")
##Sheesh, this is tough, looks like the some of these could be true and some could be false##
#the runLen value is higher, but the signal arc is weird! and the noise value is moderate...
#there are also a couple freqsd values greater than 0.1 which makes me wonder if this could be false

NSWO.54579<-fall.migrant.detections.FILT %>%
  filter(motusTagID == 54579) %>%
  filter(recvName == "Nashlyn")
##I think this is likely false##
#short runLen, high noise value, and no signal arc

NSWO.54589<-fall.migrant.detections.FILT %>%
  filter(motusTagID == 54589) %>%
  filter(recvName == "GFAFB - Lagoons")
##This is definitely false##

NSWO.54605<-fall.migrant.detections.FILT %>%
  filter(motusTagID == 54605) %>%
  filter(recvName == "Portage Mountain Quarry")
##Looks true##

NSWO.55146<- fall.migrant.detections.FILT %>%
  filter(motusTagID == 55146) %>%
  filter(recvName == "Hansen-Paterson Farm")
##likely true##

NSWO.58877<-fall.migrant.detections.FILT %>%
  filter(motusTagID == 58877) %>%
  filter(recvName == "Newgate - Ambrose's")
##likely true##

NSWO.59173<- fall.migrant.detections.FILT %>%
  filter(motusTagID == 59173) %>%
  filter(recvName == "Torwood")
##likely true##

NSWO.60912<-fall.migrant.detections.FILT %>%
  filter(motusTagID == 60912) %>%
  filter(recvName == "Newgate - Ambrose's")
##Likely true##

NSWO.71269<-fall.migrant.detections.FILT %>%
  filter(motusTagID == 71269) %>%
  filter(recvName == "ColonyFarmRegionalPark")
##Likely false##

##I will be conservative and take out anything I'm not confident in. The detections will be saved 
  #in the filtered dataset from 092923
fall.migrant.detections.FILT<-fall.migrant.detections.FILT %>%
  filter(!recvName ==  "GFAFB - Lagoons") %>%
  filter(!recvName == "ColonyFarmRegionalPark") %>%
  filter(!recvName == "Nashlyn") %>%
  filter(!recvName == "GB-Tadenac") %>%
  filter(!recvName == "GB-Go Home") %>%
  filter(!recvName == "Jackfish Lake")


write.csv(fall.migrant.detections, "./NSWO Migration/Fall.migration.detections.unfiltered_092923.csv", row.names = FALSE)
write.csv(fall.migrant.detections.FILT, "./NSWO Migration/Fall.migration.detections.FILTERED_100623.csv", row.names = FALSE)




