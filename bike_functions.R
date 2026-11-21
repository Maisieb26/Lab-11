bike_data <- read.csv("~/Desktop/STAT 1560/Data/sample_bike.csv")

#Clean data 
bike_data$end_station <- as.character(bike_data$end_station)
bike_data$start_station <- as.character(bike_data$start_station)
bike_data$start_time <- as.POSIXct(bike_data$start_time, format = c("%Y-%m-%d %H:%M:%OS"))
bike_data$end_time <- as.POSIXct(bike_data$end_time, format = c("%Y-%m-%d %H:%M:%OS"))

#Pivot to event table
event_table <- bike_data %>% 
  pivot_longer(cols = c("start_time", "end_time"), 
               names_to = "time label",
               values_to = "times") %>%
  pivot_longer(cols = c("start_station", "end_station"),
               names_to = "stations", values_to = "station") %>%
  mutate(inventory_change = ifelse(stations == "start_station", -1, 1)) %>%
  mutate(event_type = ifelse(station == "R", "R", "C")) %>%
  select(c(inventory_change, station, times, event_type)) %>%
  arrange(times)

#Creating inventory count 
stations <- unique(event_table$station)
cumulative_inventory <- rep(0, 23) #There are 23 stations, thus 23 inventory sums 
names(cumulative_inventory) <- stations 

event_table$cumulative_inventory <- 0 #Start at zero 
for(i in 1:nrow(event_table)) { #For every row in the event table
  s <- event_table$station[i] #Select the observation's station
  #Add the inventory of that observation to the cumulative inventory 
  cumulative_inventory[s] <- cumulative_inventory[s] + event_table$inventory_change[i]
  #Replace the observation's cumulative inventory with the new cumulative inventory 
  event_table$cumulative_inventory[i] <- cumulative_inventory[s]
}

head(event_table)

#For lab 11
bike_data_lab11 <- read.csv("~/Desktop/STAT 1560/Data/sample_bike.csv")
bike_data_lab11
bike_data_lab11$end_station <- as.character(bike_data$end_station)
bike_data_lab11$start_station <- as.character(bike_data$start_station)
bike_data_lab11$start_time <- format(as.POSIXct(bike_data$start_time, format = "%Y-%m-%d %H:%M:%OS"), "%H:%M")
bike_data_lab11$end_time <- format(as.POSIXct(bike_data$end_time, format = "%Y-%m-%d %H:%M:%OS"), "%H:%M")


#First calculate x, the number of observed customers departures from first station to second station
bike_data_lab11 <- bike_data_lab11 %>% group_by(start_station, end_station, 
                                                start_time, end_time) %>% 
  summarize(x=n()) %>% mutate(x_hat = x/6)
bike_data_lab11


#Creating inventory for only start station
event_table <- bike_data %>% 
  pivot_longer(cols = c("start_time", "end_time"), 
               names_to = "time label",
               values_to = "times") %>%
  pivot_longer(cols = c("start_station", "end_station"),
               names_to = "stations", values_to = "station") %>%
  mutate(inventory_change = ifelse(stations == "start_station", -1, 1)) %>%
  mutate(event_type = ifelse(station == "R", "R", "C")) %>%
  select(c(inventory_change, stations, station, times, event_type)) %>%
  arrange(times)
event_table

#Creating inventory count 
stations <- unique(event_table$station)
cumulative_inventory <- rep(0, 23) #There are 23 stations, thus 23 inventory sums 
names(cumulative_inventory) <- stations 

event_table$cumulative_inventory <- 0 #Start at zero 
for(i in 1:nrow(event_table)) { #For every row in the event table
  s <- event_table$station[i] #Select the observation's station
  #Add the inventory of that observation to the cumulative inventory 
  cumulative_inventory[s] <- cumulative_inventory[s] + event_table$inventory_change[i]
  #Replace the observation's cumulative inventory with the new cumulative inventory 
  event_table$cumulative_inventory[i] <- cumulative_inventory[s]
}

head(event_table)
