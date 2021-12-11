# # All code as of 10/22/20 written by Jake Highleyman, M.S. Energy Systems
#   contact: jhighleyman@gmail.com
# 
# This script loads CVRP Electric Vehicle purchasing data and allows for analysis
# Specifically, it allows the user to define what years should count as representative for 
# current trends. The main reason I used this was to calculate the percent of 
# BEVs and PHEVs that are "Long range" and "short range" because their charging behaviors are different.
# However, this code could be modified to extract other useful info from the CVRP data.
# CVRP data came from Alan Jenn, but I believe it's publicly available.


library(data.table)
library(ggplot2)


#Define what years should be used for representing current trends
years_for_range_test <- c("2017","2018","2019")

#CVRP data for weighting charge behavior based on market trends
cvrp_data <- fread("data/cvrp/CVRPStats.csv")
cvrp_data$year <- str_sub(cvrp_data$`Application Date`,start = -4)
cvrp_data$quantity <- 1
cvrp_bev_data <- cvrp_data[`Vehicle Category`=="BEV"]
cvrp_phev_data <- cvrp_data[`Vehicle Category`=="PHEV"]

# define "long range BEVs" (range > 200 mi) and "long range PHEVs" (electric range > 25 mi)
# for use to calibrate BEV type
cvrp_bev_data$bev_range_type <- "short"
cvrp_bev_data[`Vehicle Make`%in% c("Chevrolet","Hyundai","Jaguar","Tesla","Kia"),bev_range_type:="long"]
cvrp_phev_data$phev_range_type <- "short"
cvrp_phev_data[`Vehicle Make`%in% c("Chevrolet","Honda","Cadillac","Hyundai","Kia","Chrysler","BMW"),phev_range_type:="long"]

#calculate percent BEV long range

percent_long_bevs <- cvrp_bev_data[year%in% years_for_range_test & bev_range_type=="long",.N]/ cvrp_bev_data[year%in% years_for_range_test,.N]
percent_long_phevs <- cvrp_phev_data[year%in% years_for_range_test & phev_range_type=="long",.N]/ cvrp_phev_data[year%in% years_for_range_test,.N]

# View plot of CVRP sales by range type
# BEVs
bev_types_sum <- ggplot(data = cvrp_bev_data, aes(fill=bev_range_type, y = quantity,x=year))+
    geom_bar(position = 'stack',stat='identity')+
    scale_fill_manual(values = as.vector(alphabet(20)))
bev_types_sum

#PHEVs
phev_types_sum <- ggplot(data = cvrp_phev_data, aes(fill=phev_range_type, y = quantity,x=year))+
    geom_bar(position = 'stack',stat='identity')+
    scale_fill_manual(values = as.vector(alphabet(20)))+
    ylim(0,47000)
phev_types_sum

print(paste0("Fraction of BEVs that are long range: ",round(percent_long_bevs,3)))
print(paste0("Fraction of PHEVs that are long range: ",round(percent_long_phevs,3)))

