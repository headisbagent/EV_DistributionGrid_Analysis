# All code as of 10/22/20 written by Jake Highleyman, M.S. Energy Systems
#   contact: jhighleyman@gmail.com

# This script simply loads and merges some EVMT data, and saves the resulting more
# useful data files as csvs for use by the main code. EVMT data originally from Matt Favetti


library(data.table)
library(ggplot2)
library(reshape2)
#library(arsenal)


######## Load EVMT data ######
bevs_with_home_distances <- fread("data/EVMT/chg.locations.bev.csv")
phevs_with_home_distances <- fread("data/EVMT/chg.locations.phev.csv")

# Load all charging event data (already merged using code at end of document)
all_events <- fread("data/EVMT/all_charging_events.csv")

# TODO: clean up merge criteria
bev_merged_events <- merge(all_events,bevs_with_home_distances,by="id")
phev_merged_events <- merge(all_events,phevs_with_home_distances,by="id")

#use home_distance <= .1 to be "home charging"
#TODO does this assumption need questioning?
bev_merged_events[,home_charge:= +(distance_from_home <= 0.1)]
phev_merged_events[,home_charge:= +(distance_from_home <= 0.1)]

#add charging events for BEVs at home 
# exlude events where "valid.session" is 0 and 
# exlude events where duration is longer than 1000 minutes
good_bev_events <- bev_merged_events[home_charge==1 & valid.session==1 & bev.event.duration.minutes <= 1000]
good_phev_events <- phev_merged_events[home_charge==1 & valid.session==1 & phev.event.duration.mins <= 1000]

# add distinction for "long range" and "short range" vehicles
# long range for BEV means > 200 miles; long range for PHEV means battery-only > 25 miles
# (There aren't very many different models in the CVRP data)
good_bev_events[car_model=="Leaf"|car_model=="RAV4 EV",bev_range_type:="short"]
good_bev_events[car_model=="Model S",bev_range_type:="long"]
good_phev_events[car_model=="C-Max-Fusion"|car_model=="Prius Plug-in",phev_range_type:="short"]
good_phev_events[car_model=="Volt",phev_range_type:="long"]

# save data
fwrite(good_bev_events,"data/events_cleaned/good_bev_events.csv")
fwrite(good_phev_events,"data/events_cleaned/good_phev_events.csv")
