####COPO Migration Update####
##05.01.2024##
#added to GitHub on 05.01.24#


library(tidyverse)
library(ggplot2)
library(viridis)

####Data Prep####
depart.key<- read_csv("./Lunar/COPO/Migration Movement/4.16.24/COPO_tag key_4.16.24.csv")

depart.key<-depart.key %>%
  mutate(Ranch_DepartureDate = as.Date(Ranch_DepartureDate, format = "%m/%d/%y"),
         Valley_DepartureDate = as.Date(Valley_DepartureDate, format = "%m/%d/%y"),
         tagDeployStart_Local = as.POSIXct(tagDeployStart_Local, 
                                           format = "%m/%d/%y %H:%M",
                                           tz = "America/Denver"),
         Year = as.numeric(Year),
         EventStart = as.POSIXct(EventStart,
                                 format = "%m/%d/%y %H:%M",
                                 tz = "America/Denver"),
         EventEnd = as.POSIXct(EventEnd,
                               format = "%m/%d/%y %H:%M",
                               tz = "America/Denver"),
         hitTime = as.POSIXct(hitTime,
                              format = "%m/%d/%y %H:%M",
                              tz = "America/Denver"))



short.key<- depart.key %>%
  select(motusTagID, Year, tagDeployStart_Local, Ranch_DepartureDate, EventStart,
         EventEnd, hitTime, Valley_DepartureDate, Age, Sex)

short.key<- short.key %>%
  mutate(preDepartMins = as.numeric(hitTime - EventStart) / 60,
         EventLength = as.numeric(EventEnd - EventStart),
         julian_Rday = as.POSIXlt(Ranch_DepartureDate)$yday + 1,
         julian_Vday = as.POSIXlt(Valley_DepartureDate)$yday + 1,
         Rday = format(short.key$Ranch_DepartureDate, "%m-%d"),
         Vday = format(short.key$Valley_DepartureDate, "%m-%d"))


####Departure by Date####
#Ranch#
Rday.sum <- short.key %>%
  filter(!is.na(Rday)) %>%
  group_by(Rday) %>%
  summarise(count = n())

Rday.sum2 <- short.key %>%
  filter(!is.na(Rday)) %>%
  group_by(Rday) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         Percentage = count/total * 100)

Rday.sum3<- short.key %>%
  filter(!is.na(Rday)) %>%
  group_by(Rday, Year) %>%
  summarise(count = n(), .groups = "drop") %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(YearTotal = sum(count),
         Percentage = count/YearTotal * 100)

#Valley#
Vday.sum <- short.key %>%
  filter(!is.na(Vday)) %>%
  group_by(Vday) %>%
  summarise(count = n())

Vday.sum2 <- short.key %>%
  filter(!is.na(Vday)) %>%
  group_by(Vday) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count),
         Percentage = count/total * 100)

Vday.sum3<- short.key %>%
  filter(!is.na(Vday)) %>%
  group_by(Vday, Year) %>%
  summarise(count = n(), .groups = "drop") %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(YearTotal = sum(count),
         Percentage = count/YearTotal * 100)

Vday_to_add<- c("09-08", "09-09", "09-10", "09-11", "09-15", "09-18", "09-22",
                "09-30", "10-02", "10-03", "10-05")
new_rows <- cbind(Vday_to_add, matrix(NA, nrow = length(Vday_to_add), ncol = ncol(Vday.sum4) - 1))
new_rows_df <- as.data.frame(new_rows, stringsAsFactors = FALSE)
colnames(new_rows_df) <- colnames(Vday.sum4)
new_rows_df$Year<-as.numeric(new_rows_df$Year)
new_rows_df$count<- as.numeric(new_rows_df$count)
new_rows_df$YearTotal<- as.numeric(new_rows_df$YearTotal)
new_rows_df$Percentage<- as.numeric(new_rows_df$Percentage)

Vday.sum4 <- rbind(Vday.sum4, new_rows_df) %>%
  arrange(Vday) %>%
  distinct() %>%
  mutate(Year = case_when(
    Year == 2020 ~ "2020 (n = 4)",
    Year == 2021 ~ "2021 (n = 19)",
    Year == 2022 ~ "2022 (n = 21)",
    Year == 2023 ~ "2023 (n = 19)"
  ))

####PLOT 1####
ggplot(Vday.sum4, aes(Vday, Percentage, color = factor(Year))) +
  geom_point(size = 4) +
  scale_colour_viridis_d(na.value = "transparent") +
  scale_x_discrete(breaks = c("09-08", "09-15", "09-22", "09-29", "10-06")) +
  theme_bw() +
  labs(x = "Departure Day",
       y = "Percent",
       title = "Common Poorwill Valley Departure by Date",
       color = "Year") +
  theme(text = element_text(family = "serif", size = 12))

ggsave("Valley Departure by Year_COPO.jpeg", 
       path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures", 
       width = 7.5, 
       height = 4.75, 
       units = "in")

####Departure from Sunset####
Sun.2020<- read_csv("./Lunar/Moon Data/2020/Moon Times 2020_B.csv")
Sun.2020<- Sun.2020 %>%
  mutate(across(c(moonrise, moonset, sunset, dusk, nauticalDusk, nauticalDawn, dawn, sunrise),
                ~ as.POSIXct(trimws(.), format = "%m/%d/%y %H:%M"))) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  select(date, sunset, dusk, nauticalDusk)

Sun.2021<- read_csv("./Lunar/Moon Data/2021/Moon Times 2021.csv")
Sun.2021<- Sun.2021 %>%
  mutate(across(c(moonrise, moonset, sunset, dusk, nauticalDusk, nauticalDawn, dawn, sunrise),
                ~ as.POSIXct(trimws(.), format = "%m/%d/%y %H:%M"))) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  select(date, sunset, dusk, nauticalDusk)

Sun.2022<- read_csv("./Lunar/Moon Data/2022/Moon Times 2022.csv")
Sun.2022<- Sun.2022 %>%
  mutate(across(c(moonrise, moonset, sunset, dusk, nauticalDusk, nauticalDawn, dawn, sunrise),
                ~ as.POSIXct(trimws(.), format = "%m/%d/%y %H:%M"))) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  select(date, sunset, dusk, nauticalDusk)

Sun.2023<- read_csv("./Lunar/Moon Data/2023/Moon Times 2023.csv")
Sun.2023<- Sun.2023 %>%
  mutate(across(c(moonrise, moonset, sunset, dusk, nauticalDusk, nauticalDawn, dawn, sunrise),
                ~ as.POSIXct(trimws(.), format = "%m/%d/%y %H:%M"))) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  select(date, sunset, dusk, nauticalDusk)

depart.2020<- short.key %>%
  filter(Year == 2020) %>%
  select(motusTagID, Year, Ranch_DepartureDate, EventStart, EventEnd, hitTime, 
         preDepartMins, EventLength, Rday) %>%
  rename(date = Ranch_DepartureDate) %>%
  left_join(Sun.2020, by = "date")

depart.2021<- short.key %>%
  filter(Year == 2021,
         !is.na(Ranch_DepartureDate)) %>%
  select(motusTagID, Year, Ranch_DepartureDate, EventStart, EventEnd, hitTime,
         preDepartMins, EventLength, Rday) %>%
  rename(date = Ranch_DepartureDate) %>%
  left_join(Sun.2021, by = "date")

depart.2022<- short.key %>%
  filter(Year == 2022,
         !is.na(Ranch_DepartureDate)) %>%
  select(motusTagID, Year, Ranch_DepartureDate, EventStart, EventEnd, hitTime,
         preDepartMins, EventLength, Rday)%>%
  rename(date = Ranch_DepartureDate) %>%
  left_join(Sun.2022, by = "date")

depart.2023<- short.key %>%
  filter(Year == 2023,
         !is.na(Ranch_DepartureDate)) %>%
  select(motusTagID, Year, Ranch_DepartureDate, EventStart, EventEnd, hitTime,
         preDepartMins, EventLength, Rday)%>%
  rename(date = Ranch_DepartureDate) %>%
  left_join(Sun.2023, by = "date")

depart.AllYears<- depart.2020 %>%
  bind_rows(depart.2021, depart.2022, depart.2023)

depart.Sunset<- depart.AllYears %>%
  mutate(minsFromSunset = as.numeric(hitTime - sunset),
         minsFromDusk = as.numeric(hitTime - dusk),
         minsFromNDusk = as.numeric(hitTime - nauticalDusk)) %>%
  select(Year, date, Rday, minsFromSunset, minsFromDusk, 
         minsFromNDusk, preDepartMins, EventLength)

depart.Sunset <- depart.Sunset %>%
  filter(date != "2021-08-24") %>%
  arrange(date)

####Plot 2####
ggplot(depart.Sunset, aes(minsFromSunset)) +
  geom_histogram(bins = 25, color = "black", fill = "grey") +
  labs(x = "Time from Sunset",
       y = "Common Poorwill Count") +
  theme_minimal() +
  geom_vline(xintercept = 0,
             linetype = "solid", color = "red") +
  geom_vline(xintercept = 40, 
             linetype = "dashed", color = "orange") +
  geom_vline(xintercept = 80,
             linetype = "dashed", color = "blue") +
  scale_x_continuous(breaks = c(0, 40, 80),
                     labels = c("Sunset", " Civil Dusk", "Nautical Dusk")) +
  theme(text = element_text(family = "serif", size = 12))

ggsave("Departure Times from Sunset_COPO.jpeg", 
       path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures", 
       width = 7.5, 
       height = 4.75, 
       units = "in")


####Activity before Departure####
depart.act <- depart.Sunset %>%
  mutate(ActLength = case_when(
    preDepartMins == 0 ~ "0",
    preDepartMins > 0 & preDepartMins <= 10 ~ "0-10",
    preDepartMins > 10 & preDepartMins <= 20 ~ "11-20",
    preDepartMins > 20 & preDepartMins <= 30 ~ "21-30",
    preDepartMins > 30 & preDepartMins <= 40 ~ "31-40",
    preDepartMins > 40 & preDepartMins <= 50 ~ "41-50",
    preDepartMins > 50 ~ "50+"
  )) %>%
  select(date, ActLength)

####Plot 3####
ggplot(depart.act, aes(ActLength)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Activity Length in Minutes",
       y = "Common Poorwill Count") +
  theme(text = element_text(family = "serif", size = 12))

ggsave("Activity Length in Minutes before Departure_COPO.jpg", 
       path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures", 
       width = 7.5, 
       height = 4.75, 
       units = "in")

mean(depart.Sunset$minsFromSunset)

####Example Departure####
c60855<- COPO.depart.table2 %>%
  filter(motusTagID == 60855,
         ts_LocalTime >= "2023-10-04 21:34:19")

c60855$recvName<- factor(c60855$recvName)
c60855$port<- factor(c60855$port)

c60855<- c60855 %>%
  mutate(recvName = case_when(
    recvName == "Curlew Ridge-MT" ~ "Curlew Ridge",
    recvName == "Sunset Bench-MT" ~ "Sunset Bench",
    recvName == "Teller Refuge-MT" ~ "Teller Refuge",
    TRUE ~ recvName
  ))

####Plot 4####
ggplot(c60855, aes(ts_LocalTime, sig, color = recvName))+
  geom_point(size = 3) +
  scale_color_manual(values = viridis_pal()(length(unique(c60855$recvName))),
                     breaks = unique(c60855$recvName)) +
  theme_minimal() +
  labs(title = "Common Poorwill Migration Departure",
       x = "Time",
       y = "Signal Strength",
       color = "Receiver") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(family = "serif", size = 12))


ggsave("Valley Departure Plot_COPO.jpeg", 
       path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures", 
       width = 7.5, 
       height = 4.75, 
       units = "in")

####Plot 5####
c60855T<- c60855 %>%
  filter(recvName == "Teller Refuge")
viridis(1, alpha = 1, direction = -1)

ggplot(c60855T, aes(ts_LocalTime, sig)) +
  geom_point(size = 3, color = "#FDE725FF") +
  theme_minimal() +
  labs(title = "Passing Teller Refuge Receiving Station",
       x = "Time",
       y = "Signal Strength") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(family = "serif", size = 12))


ggsave("Passing Teller Refuge_COPO.jpeg", 
       path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures", 
       width = 5, 
       height = 3.25, 
       units = "in")

####Onset of Departure####

c60845<- COPO.depart.table2 %>%
  filter(motusTagID == 60845,
         ts_LocalTime >= "2023-10-06 15:10:41") %>%
  mutate(recvName = case_when(
    recvName == "Curlew Ridge-MT" ~ "Curlew Ridge",
    recvName == "South Baldy Ridge-MT" ~ "South Baldy Ridge",
    recvName == "Teller Refuge-MT" ~ "Teller Refuge",
    recvName == "North Floodplain-MT" ~ "North Floodplain",
    recvName == "Marmot-MPG" ~ "Marmot",
    TRUE ~ recvName
  ))

####Plot 6####
ggplot(c60845, aes(ts_LocalTime, sig, color = factor(recvName))) +
  geom_point(size = 3) +
  scale_color_manual(values = viridis_pal()(length(unique(c60845$recvName))),
                     breaks = unique(c60845$recvName)) +
  theme_minimal() +
  labs(x = "Time",
       y = "Signal Strength",
       color = "Receiver") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(family = "serif", size = 12))


ggsave("Onset of Departure_COPO.jpeg", 
       path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures", 
       width = 7.5, 
       height = 4.75, 
       units = "in")

####Plot 7####
c60845A<- c60845 %>%
  filter(recvName == "South Baldy Ridge") %>%
  mutate(port = case_when(
    port == 5 ~ "North",
    port == 6 ~ "East",
    port == 4 ~ "Southeast"
  ))


ggplot(c60845A, aes(ts_LocalTime, sig, color = factor(port))) +
  geom_point(size = 3) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(x = "Time",
       y = "Signal Strength",
       color = "Antenna Bearing") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(family = "serif", size = 12))

ggsave("Signals by Antenna_COPO.jpeg", 
       path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures", 
       width = 7.5, 
       height = 4.75, 
       units = "in")

####Activity Signals Contrast####
c75807<- COPO.depart.table2 %>%
  filter(motusTagID == 75807,
         ts_LocalTime >= "2023-10-04 19:52:25",
         recvName == "Ambrose Hill")
viridis(1, direction = 1)

####Plot 8####
ggplot(c75807, aes(ts_LocalTime, sig)) +
  geom_point(size = 3, color = "#440154FF") +
  theme_minimal() +
  labs(x = "Time",
       y = "Signal Strength") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(family = "serif", size = 12)) 


ggsave("Activity Signals Contrast_COPO.jpeg", 
       path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures", 
       width = 7.5, 
       height = 4.75, 
       units = "in")

####Activity Signals Contrast Breeding Season####
br.ac<- COPO.detect %>%
  filter(year == 2023)
unique(br.ac$motusTagID)  

c60855.aug<- br.ac %>%
  filter(motusTagID == 60855,
         ts_LocalTime >= "2023-08-20 21:37:51",
         ts_LocalTime <= "2023-08-21 04:54:05")

####Plot 9####
ggplot(c60855.aug, aes(ts_LocalTime, sig)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(x = "Time",
       y = "Signal Strength") +
  theme(text = element_text(family = "serif", size = 12))

ggsave("Activity Variation_COPO 148.jpeg", 
       path = "~/Library/CloudStorage/Egnyte-mpgcloud/Shared/Workspace/Teams/Avian Science/COPO/R data/Figures", 
       width = 7.5, 
       height = 4.75, 
       units = "in")
