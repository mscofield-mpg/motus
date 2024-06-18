####LEWO Manual Filtering#####
  ##Updated 6.18.2024##

#Load signal plotting function
source("./Scripts/useful_functions.R") 

#Load Detections
LEWO.detect<- Filtered_Detections
#OR
LEWO.detect<-read_csv("./Processed Data/LEWO/6.17.24/LEWO_tag_detections_FILTERED_6.17.24.csv")

##Manual Review of Signal Strength Plots for False Positives##

#motusTagID 
