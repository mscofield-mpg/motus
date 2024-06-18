####Common Nighthawk Manual Filtering####
  ##Updated 6.18.2024##

#Load signal plotting function
source("./Scripts/useful_functions.R") 

#Load Detections
CONI.detect<- Filtered_Detections
#OR
CONI.detect<-read_csv("./Processed Data/CONI/6.17.24/CONI_tag_detections_FILTERED_6.17.24.csv")

##Manual Review of Signal Strength Plots for False Positives##

#motusTagID 
