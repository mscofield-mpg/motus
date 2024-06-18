####Common Poorwill Manual Filtering#####
  ##updated 6.17.2024##

#Load signal strength plotting function#
source("./Scripts/useful_functions.R") 

#Load detections from Extraction Function or CSV format#
COPO.detect<- Filtered_Detections
#OR#
COPO.detect<- read_csv("./Processed Data/COPO/6.17.24/COPO_tag_detections_FILTERED_6.17.24.csv")

#Create list of unique tags#
COPO.tags<-c(unique(COPO.detect$motusTagID))

##Manual Review of Signal Strength Plots for False Positives##

#motusTagID 47473
plotTagSig_mod(COPO.detect %>% filter(recvName == "North Spring Valley"), motusTagID = 47473)
  #True detection -- Keep

#motusTagID 47481
c47481<- COPO.detect %>%
  filter(motusTagID == 47481,
         recvName == "Minidoka National Wildlife Refuge, ID")
unique(c47481$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Minidoka National Wildlife Refuge, ID"), motusTagID = 47481)
  #runLen = 5, freqsd is good, ptrue = 1, signal arc is ok
  #possibly true -- keep

#motusTagID 47482
c47482<-COPO.detect %>%
  filter(motusTagID == 47482,
         recvName == "GPCD Soapstone")
plotTagSig_mod(COPO.detect %>% filter(recvName == "GPCD Soapstone"), motusTagID = 47482)
  #Signal arc is fairly flat but there is a peak.
  #freqsd and runLen look like they could be true... 
  #Seems likely true -- Keep


#motusTagID 47483
c47483<-COPO.detect %>%
  filter(motusTagID == 47483,
         recvName == "Camas National Wildlife Refuge - ID")
plotTagSig_mod(COPO.detect %>% filter(recvName == "Camas National Wildlife Refuge - ID"), motusTagID = 47483)
  #detections on three different antennas... true detections -- keep
  #freqsd is good, runLens >= 7, and ptrue = 1


#motusTagID 54242
c54242<- COPO.detect %>%
  filter(motusTagID == 54242)
plotTagSig_mod(COPO.detect %>% filter(recvName == "GPCD SPLT"), motusTagID = 54242)
  #likely positive -- keep
#two antennas, short runLens (2 and 3) then two longer ones (6 and 9)... ptrue = 0.75
#Possibly true, possibly false... Unknown what to do -- 3.15.24

#motusTagID 54248
c54248<-COPO.detect %>%
  filter(motusTagID == 54248,
         recvName == "North Spring Valley")
unique(c54248$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Cleve Creek"), motusTagID = 54248)
  #two really short runs (runLen = 2), and one run of 8... same antenna
  #freqsd is sus... ptrue = 0.66
#Possibly true but also, could be false... I would probably filter out 
plotTagSig_mod(COPO.detect %>% filter(recvName == "North Spring Valley"), motusTagID = 54248)
  #two antennas, mostly short runLens < 5... freqsd is ok, ptrue = 0.64
#Possibly true but leaning towards false... I would probably filter this out

#motusTagID 54249
c54249<- COPO.detect %>%
  filter(motusTagID == 54249)
unique(c54249$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Minidoka National Wildlife Refuge, ID"), motusTagID = 54249)
  #true detection -- keep


#motusTagID 54254
c54254<- COPO.detect %>%
  filter(motusTagID == 54254,
         recvName == "Market Lake WMA, Roberts, ID")
unique(c54254$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Minidoka National Wildlife Refuge, ID"), motusTagID = 54254)
  #true detection -- Keep
#runLens of 5 and 6, ptrue = 1, signal arc looks ok...
#Above diagnostics stand with 3.15.24 update
plotTagSig_mod(COPO.detect %>% filter(recvName == "Cleve Creek"), motusTagID = 54254)
  #true detection -- keep
#runLens of 7 and 9, ptrue = 0.889, signal arc looks good...
#Above diagnostics stand with 3.15.24 update
plotTagSig_mod(COPO.detect %>% filter(recvName == "Camas National Wildlife Refuge - ID"), motusTagID = 54254)
  #true detection -- keep
#runLens of 2, 5 and 7, two antennas, ptrue = 0.894, signal arc looks ok
#Above diagnostics stand with 3.15.24 update
plotTagSig_mod(COPO.detect %>% filter(recvName == "Market Lake WMA, Roberts, ID"), motusTagID = 54254)
  #runLens of 2, 3 and 6... ptrue = 0.428, freqsd looks ok, signal arc is a little sus....
  #I think I would filter out the really short runs (2 and 3), and even then I would be cautious
#Possibly worth filtering out (ADDED with 3.15.24 update)

#motusTagID 54258
c54258<- COPO.detect %>%
  filter(motusTagID == 54258)
unique(c54258$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Deer Flat NWR, Nampa ID"), motusTagID = 54258)
  #likely true -- keep


#motusTagID 55121
c55121<- COPO.detect %>%
  filter(motusTagID == 55121,
         recvName == "Nashlyn")
plotTagSig_mod(COPO.detect %>% filter(recvName == "Nashlyn"), motusTagID = 55121)
  #very flat, no arc -- likely false -- FILTER OUT
  #runLen of 4, ptrue = 1, freqsd is ok but I would still filter this one out



#motusTagID 55122
c55122<- COPO.detect %>%
  filter(motusTagID == 55122,
         recvName == "Red Tail Hill")
plotTagSig_mod(COPO.detect %>% filter(recvName == "Red Tail Hill"), motusTagID = 55122)
  #likely true -- keep
  #two really short runs, but one long one (runLen = 29), signal arc looks good, ptrue = 0.942


#motusTagID 55125
c55125<-COPO.detect %>%
  filter(motusTagID == 55125,
         recvName == "Niagara Springs WMA-ID")
unique(c55125$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Ash Meadows NWR"), motusTagID = 55125)
  #likely true -- keep
  #one run of 8, freqsd looks good, ptrue = 1, signal arc looks good
plotTagSig_mod(COPO.detect %>% filter(recvName == "Billingsley Creek WMA-ID"), motusTagID = 55125)
  #true detection -- keep
  #one run of 17, freqsd looks good, ptrue = 1, signal arc looks good
plotTagSig_mod(COPO.detect %>% filter(recvName == "Niagara Springs WMA-ID"), motusTagID = 55125)
  #likely true -- keep
  #one run of 12, freqsd looks good, ptrue = 1, signal arc looks good

#motusTagID 55132
c55132<- COPO.detect %>%
  filter(motusTagID == 55132,
         recvName == "Long Canyon")
ggplot(c55132, aes(ts, sig, color = port)) +
  geom_point()
plotTagSig_mod(COPO.detect %>% filter(recvName == "Red Tail Hill"), motusTagID = 55132)
  #ooh tricky
  #two different October detections... both have a combo of short runLens (2-5) and longer runs (10-16)
  #ptrue values between 0.7 - 1, and freqsd ok...
#when isolated, the 2021 detections look possibly true
#when isolated, the 2022 detections look suss...
plotTagSig_mod(COPO.detect %>% filter(recvName == "Long Canyon"), motusTagID = 55132)
  #one run of 22, with good freqsd, and a signal arc, and ptrue = 0.814
  #likely true (longer run)... keep

#motusTagID 55137
c55137<- COPO.detect %>%
  filter(motusTagID == 55137,
         recvName == "Billingsley Creek WMA-ID")
unique(c55137$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Billingsley Creek WMA-ID"), motusTagID = 55137)
  #one runLen of 4, good freqsd, ptrue = 1, and generally a trustworthy station
  #likely true -- keep

#motusTagID 55141
c55141<- COPO.detect %>%
  filter(motusTagID == 55141,
         recvName == "Market Lake WMA, Roberts, ID")
unique(c55141$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Market Lake WMA, Roberts, ID"), motusTagID = 55141)
  #tricky...
  #a few short runs (2, 4 and 6), with a ptrue = 0.7, and a fairly flat signal arc
  #possibly true, possibly false... 

#motusTagID 60849
c60849<-COPO.detect %>%
  filter(motusTagID == 60849,
         recvName == "Gauge House")
unique(c60849$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Gauge House"), motusTagID = 60849)
  #seems likely false... FILTER OUT

#motusTagID 60853
c60853<- COPO.detect %>%
  filter(motusTagID == 60853,
         recvName == "Whitfield Hamilton Grain Elevator")
unique(c60853$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Ensenada - Estero Beach Hotel"), motusTagID = 60853)
  #two runs, one runLen = 11 and two short runs of 3.. weird signal arc, ptrue = 0.647
  #seems likely false... FILTER OUT
plotTagSig_mod(COPO.detect %>% filter(recvName == "GFAFB - Lagoons"), motusTagID = 60853)
  #two run, one runLen = 6, the other = 2, freqsd has some that surpass 0.1, and ptrue = 0.6875
  #seems likely false... FILTER OUT
plotTagSig_mod(COPO.detect %>% filter(recvName == "Whitfield Hamilton Grain Elevator"), motusTagID = 60853)
  #really short runLens, weird signal arc, ptrue = 0.56
  #likely false... FILTER OUT

#motusTagID 60855
c60855<-COPO.detect %>%
  filter(motusTagID == 60855)
unique(c60855$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Minidoka National Wildlife Refuge, ID"), motusTagID = 60855)
  #true detection -- keep

#motusTagID 60857
c60857<-COPO.detect %>%
  filter(motusTagID == 60857,
         recvName == "Grève de Tadoussac")
unique(c60857$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Kuna-ID"), motusTagID = 60857)
  #likely true based on arc, freqsd, and runLen, ptrue = 1 -- keep
plotTagSig_mod(COPO.detect %>% filter(recvName == "Grève de Tadoussac"), motusTagID = 60857)
  #likely false... FILTER OUT

#motusTagID 60859
c60859<-COPO.detect %>%
  filter(motusTagID == 60859,
         recvName == "Kuna-ID")
plotTagSig_mod(COPO.detect %>% filter(recvName == "Kuna-ID"), motusTagID = 60859)
  #likely true based on arc, freqsd, and runLen (two short runs of 6), ptrue = 1 -- keep

#motusTagID 60861
c60861<- COPO.detect %>%
  filter(motusTagID == 60861,
         recvName == "428_testStation")
unique(c60861$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "MonoLakeNW1"), motusTagID = 60861)
  #true detection -- keep
  #runLen = 10, signal arc looks good, freqsd is good, ptrue = 1
#4.3.24 -> MonoLakeNW1 is 428_testStation in this iteration, they are the same station
          #according to motus website... not sure where this name change came from?


#motusTagID 60911
c60911<- COPO.detect %>%
  filter(motusTagID == 60911,
         recvName == "Billingsley Creek WMA-ID")
unique(c60911$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Camp Cady"), motusTagID = 60911)
  #one short run of 8 (and two 2's but those will be filtered), freqsd looks ok, signal arc looks good
  #ptrue = 0.667....
  #possibly true, possibly false
plotTagSig_mod(COPO.detect %>% filter(recvName == "Ash Meadows NWR"), motusTagID = 60911)
  #true detection -- keep
  #one runLen = 9, good signal arc, freqsd is good, ptrue = 1
plotTagSig_mod(COPO.detect %>% filter(recvName == "Billingsley Creek WMA-ID"), motusTagID = 60911)
  #one runLen = 8, a couple of 2's, good freqsd, signal arc looks ok, ptrue = 0.667
  #possibly true, possibly false
plotTagSig_mod(COPO.detect %>% filter(recvName == "Palo Verde"), motusTagID = 60911)
  #true detection -- keep

c71181<- COPO.detect %>%
  filter(motusTagID == 71181,
         recvName == "Heron Crossing-MT")
unique(c71181$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "San Jacinto"), motusTagID = 71181)
  #short runLen, good freqsd, signal arc is a little sus, ptrue = 1
  #possibly true -- keep
plotTagSig_mod(COPO.detect %>% filter(recvName == "Ash Meadows NWR"), motusTagID = 71181)
  #true detection -- keep
plotTagSig_mod(COPO.detect %>% filter(recvName == "Heron Crossing-MT"), motusTagID = 71181)
  #weird signal arc, not good freqsd, ptrue = 1
  #likely false... FILTER OUT

c71183<- COPO.detect %>%
  filter(motusTagID == 71183,
         recvName == "Indian Joe Springs")
unique(c71183$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Indian Joe Springs"), motusTagID = 71183)
  #true detection -- keep

c71184<- COPO.detect %>%
  filter(motusTagID == 71184,
         recvName == "San Felipe Creek")
unique(c71184$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "San Felipe Creek"), motusTagID = 71184)
  #likely true -- keep
plotTagSig_mod(COPO.detect %>% filter(recvName == "Imperial"), motusTagID = 71184)
  #true detection -- keep

c75797<- COPO.detect %>%
  filter(motusTagID == 75797)
unique(c75797$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "San Felipe Creek"), motusTagID = 75797)
  #likely true -- keep

c75804<- COPO.detect %>%
  filter(motusTagID == 75804,
         recvName == "Merced NWR")
unique(c75804$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Merced NWR"), motusTagID = 75804)
  #signal arc looks good, freqsd is a little sus, ptrue = 1 
  #likely true -- keep

c75807<- COPO.detect %>%
  filter(motusTagID == 75807)
unique(c75807$recvName)
plotTagSig_mod(COPO.detect %>% filter(recvName == "Laguna_Peak"), motusTagID = 75807)
  #true detection -- keep
COPO.detect<-Filtered_Detections

#As of 6.17.24
COPO.detect.filt<-COPO.detect %>%
  filter(!(recvName == "Nashlyn" |
             recvName == "Ensenada - Estero Beach Hotel" |
             recvName == "GFAFB - Lagoons" |
             recvName == "Whitfield Hamilton Grain Elevator" |
             (recvName == "GPCD SPLT" & motusTagID == 54242) |
             (recvName == "Cleve Creek" & motusTagID == 54248) |
             (recvName == "North Spring Valley" & motusTagID == 54248) |
             (recvName == "Market Lake WMA, Roberts, ID" & motusTagID == 54254) |
             (recvName == "Market Lake WMA, Roberts, ID" & motusTagID == 55141) |
             (recvName == "Gauge House" & motusTagID == 60849) |
             (recvName == "Grève de Tadoussac" & motusTagID == 60857) |
             (recvName == "Heron Crossing-MT" & motusTagID == 71181)))

write.csv(COPO.detect.filt, file = "./Processed Data/COPO/6.17.24/COPO_tag_detections_FILTERED_6.17.24.csv", row.names = FALSE)
