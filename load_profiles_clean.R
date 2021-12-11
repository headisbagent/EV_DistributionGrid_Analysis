# All code as of 10/22/20 written by Jake Highleyman, M.S. Energy Systems
#   contact: jhighleyman@gmail.com
# 
# This script allows for generating daily EV load profile based on real empirical
# charging data from the UC Davis PHEV Center's EVMT project.
# There are many ways to extend this code to include different assumptions.
# The profiles generated here are based on the empirical data only--there are no
#   travel demand assumptions or anything like that. Therefore, this is a 
#   representation of how people in this EVMT study (early EV adopters) happen to 
#   charge. The BEVs are mostly Nissan Leafs, with some Teslas and Bolts. The PHEVs
#   are mostly Volts, C-Max Fusions, and Prius Plug ins. The EVMT data can be updated,
#   but as on 10/22/20 it's data from charge events from 2015 - 2018 in California.
#   
#   This script is used with other parts of the model to generate load profiles by Utility Feeder.
#   It may need to be updated in order to be accessed by other programs 
#       (I'm still working on the way that the different code chunks interact, 
#       and this is a shortened version of something that used to be way longer)
#   

# Not all of these packages are actually needed for this script...
#library(gdxtools)
library(data.table)
#library(tidycensus) #TODO fix on mac
#library(tidyverse)  #TODO fix on mac
library(ggplot2)
library(reshape2)
library(tibble)
library(purrr)
library(stringr)
library(RColorBrewer)
library(pals)
library(Hmisc)
library(gsubfn) 

#a few universals
start_at_8am <- c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,0,1,2,3,4,5,6,7)
peak_time <- c(16,17,18,19,20,21,22) # 4 to 10pm

# Load evmt cleaned charge event data
# These are the main data sources that the "sampleLoads" function uses
# Note: the main output variable of interest is `energy[delta_soc][avg_kW].y`,
#   which is the average power used over the duration of a charge event
good_bev_events <- fread("data/events_cleaned/good_bev_events.csv")
good_phev_events <- fread("data/events_cleaned/good_phev_events.csv")

###### Summary charts ######

## Starting time of charging ##
forPlot.startHour <- rbind(good_bev_events[,.(start.hour=bev.event.start.hour.index)],good_phev_events[,.(start.hour=phev.event.start.hour.index)])

plotSave <- ggplot(data=forPlot.startHour,aes(x=start.hour))+
    geom_histogram(bins=24)+
    xlab('Start hour of charging')+
    ylab('Count of charging events')+
    theme_bw()
ggsave(plotSave,file='figures/summary_startHour.pdf',height=4.5,width=6)
ggsave(plotSave,file='figures/summary_startHour.png',height=4.5,width=6)

## Average energy charged per event ##
forPlot.chrgEnergy <- rbind(good_bev_events[,.(energy=`energy[delta_soc][total_energy].y`,type='BEV')],good_phev_events[,.(energy=`energy[delta_soc][total_energy].y`,type='PHEV')])

plotSave <- ggplot(data=forPlot.chrgEnergy,aes(x=energy,color=type))+
    geom_density()+
    xlim(0,60)+
    xlab('Energy per charging event [kWh]')+
    ylab('Frequency')+
    scale_color_discrete(name='Type')+
    theme_bw()
ggsave(plotSave,file='figures/summary_chrgEnergy.pdf',height=4.5,width=6)
ggsave(plotSave,file='figures/summary_chrgEnergy.png',height=4.5,width=6)

## Length of charging ##
forPlot.chrgLength <- rbind(good_bev_events[,.(min=bev.event.duration.minutes,type='BEV')],good_phev_events[,.(min=phev.event.duration.mins,type='PHEV')])

plotSave <- ggplot(data=forPlot.chrgLength,aes(x=min,color=type))+
    geom_density()+
    xlab('Charge event duration [minutes]')+
    ylab('Frequency')+
    scale_color_discrete(name='Type')+
    theme_bw()
ggsave(plotSave,file='figures/summary_chrgLength.pdf',height=4.5,width=6)
ggsave(plotSave,file='figures/summary_chrgLength.png',height=4.5,width=6)


###### Calculate charge factors by type of vehicle ####
# Notes: vehicle types are divided into BEV longrange, BEV shortrange, PHEV longrange, PHEV shortrange
# define "long range BEVs" (range > 200 mi) and "long range PHEVs" (electric range > 25 mi)
# get average BEV charge factor (charge events / vehicle / day active)
# this means looking at the data PER VEHICLE, not per charge event
#TODO shorten or perhaps eliminate this code and replace with hard inputs?
# Case 1: BEV long range
bev_long_by_vehicle <- good_bev_events[bev_range_type=="long",.(.N,start.date = min(as.Date(bev.event.start.ymd,origin = "1899-12-30")),end.date=max(as.Date(bev.event.start.ymd,origin = "1899-12-30"))), by= vehicle_name.x]
bev_long_by_vehicle$num.days = bev_long_by_vehicle$end.date - bev_long_by_vehicle$start.date +1
# charge factor is number of charge events for one individual car divided by the total number of days between first and last charge event. AKA, a measure of charge events / day
bev_long_by_vehicle$charge_factor <- bev_long_by_vehicle$N / as.numeric(bev_long_by_vehicle$num.days)
bev.long.charge.factor <- mean(bev_long_by_vehicle$charge_factor)

# Case 2: BEV short range
bev_short_by_vehicle <- good_bev_events[bev_range_type=="short",.(.N,start.date = min(as.Date(bev.event.start.ymd,origin = "1899-12-30")),end.date=max(as.Date(bev.event.start.ymd,origin = "1899-12-30"))), by= vehicle_name.x]
bev_short_by_vehicle$num.days = bev_short_by_vehicle$end.date - bev_short_by_vehicle$start.date +1
bev_short_by_vehicle$charge_factor <- bev_short_by_vehicle$N / as.numeric(bev_short_by_vehicle$num.days)
bev.short.charge.factor <- mean(bev_short_by_vehicle$charge_factor)

# Case 3: PHEV long range
phev_long_by_vehicle <- good_phev_events[phev_range_type=="long",.(.N,start.date = min(as.Date(phev.event.start.ymd,origin = "1899-12-30")),end.date=max(as.Date(phev.event.start.ymd,origin = "1899-12-30"))), by= vehicle_name.x]
phev_long_by_vehicle$num.days = phev_long_by_vehicle$end.date - phev_long_by_vehicle$start.date +1
phev_long_by_vehicle$charge_factor <- phev_long_by_vehicle$N / as.numeric(phev_long_by_vehicle$num.days)
phev.long.charge.factor <- mean(phev_long_by_vehicle$charge_factor)

# Case 4: PHEV short range
phev_short_by_vehicle <- good_phev_events[phev_range_type=="short",.(.N,start.date = min(as.Date(phev.event.start.ymd,origin = "1899-12-30")),end.date=max(as.Date(phev.event.start.ymd,origin = "1899-12-30"))), by= vehicle_name.x]
phev_short_by_vehicle$num.days = phev_short_by_vehicle$end.date - phev_short_by_vehicle$start.date +1
phev_short_by_vehicle$charge_factor <- phev_short_by_vehicle$N / as.numeric(phev_short_by_vehicle$num.days)
phev.short.charge.factor <- mean(phev_short_by_vehicle$charge_factor)



#SampleLoads Function
# This function produced load profiles by sampling "good_bev_events" and "good_phev_events" 
# It takes several other inputs that can be used to look at different scenarios
# Note: This code is not super efficient and I'm sure it could be a lot better

sampleLoads <- function(num_trials, num_BEVs, percent_long_BEV=.5, num_PHEVs, percent_long_PHEV=.5, days_of_week = "all", utility = "all", month = "all", charging_level = "all"){
    
    #weighted averages of the charge factors, based on what percent of vehicles in sample are "long range" or "short range"
    bev.charge.factor <- (percent_long_BEV * bev.long.charge.factor) + ((1 - percent_long_BEV) * bev.short.charge.factor)
    phev.charge.factor <- (percent_long_PHEV * phev.long.charge.factor) + ((1 - percent_long_PHEV) * phev.short.charge.factor)
    
    
    max_loads <- data.table()
    all_loads <- data.table()
    #set.seed(5434)
    
    

    multiday_charge_prof <- data.table(tenmin_index=c(0:719))
    
    #filter BEV events based on input criteria (month, utility, weekend/weekday status,etc)
    # from datatable "good_bev_events" to "possible_bev_events"
    possible_bev_events <- good_bev_events
    possible_phev_events <- good_phev_events
    if(days_of_week== "Weekdays"){
        possible_bev_events <- possible_bev_events[bev.wkday.wkend=="Weekday"]
        possible_phev_events <- possible_phev_events[phev.wkday.wkend=="Weekday"]
    }
    if(days_of_week== "Weekends"){
        possible_bev_events <- possible_bev_events[bev.wkday.wkend=="Weekend"]
        possible_phev_events <- possible_phev_events[phev.wkday.wkend=="Weekend"]
    }
    if(month!= "all"){
        possible_bev_events <- possible_bev_events[bev.month %in% month]
        possible_phev_events <- possible_phev_events[phev.month %in% month]
    }
    if(utility!= "all"){
        possible_bev_events <- possible_bev_events[`location[utility]` %in% utility]
        possible_phev_events <- possible_phev_events[`location[utility]` %in% utility]
    }
    if(charging_level!= "all"){
        possible_bev_events <- possible_bev_events[`energy[charge_type][type].x` %in% charging_level]
        possible_phev_events <- possible_phev_events[`energy[charge_type][type].x` %in% charging_level]
    }
    
    print(paste("bev possible events: ",nrow(possible_bev_events)))
    print(paste("phev possible events: ",nrow(possible_phev_events)))
    
    #function to get sum of all BEV loads for a specific 10-min time bin
    get_bev_loads <- function(new_time_bin, event_sample){
        return(sum(event_sample[new_time_bin >= bev.event.start.10min.index & new_time_bin < (bev.event.start.10min.index + round(bev.event.duration.minutes / 10.0,digits = 0)),`energy[delta_soc][avg_kW].y`]))
    }
    #parallel function to get sum of all PHEV loads for a specific 10-min time bin
    get_phev_loads <- function(new_time_bin, event_sample){
        return(sum(event_sample[new_time_bin >= phev.event.start.10min.index & new_time_bin < (phev.event.start.10min.index + round(phev.event.duration.mins / 10.0,digits = 0)),`energy[delta_soc][avg_kW].y`]))
    }
    
    for (i in c(1:num_trials)){     
        
        # Charge events are sampled. Number of events sampled based on number of cars
        #   and charge factor for that kind of car (average charge events per day)
        
        # Get sampled charge events for Long Range BEVs
        longrange_bev_events <- possible_bev_events[bev_range_type=='long'][sample(1:nrow(possible_bev_events[bev_range_type=='long']), (num_BEVs *percent_long_BEV* bev.long.charge.factor), replace = T)]
        
        # Get sampled charge events for Short Range BEVs
        shortrange_bev_events <- possible_bev_events[bev_range_type=='short'][sample(1:nrow(possible_bev_events[bev_range_type=='short']), (num_BEVs * (1-percent_long_BEV)* bev.short.charge.factor), replace = T)]
        
        # Get sampled charge events for Long Range PHEVs
        longrange_phev_events <- possible_phev_events[phev_range_type=='long'][sample(1:nrow(possible_phev_events[phev_range_type=='long']), (num_PHEVs * percent_long_PHEV* phev.long.charge.factor), replace = T)]
        
        # Get sampled charge events for Short Range PHEVs
        shortrange_phev_events <- possible_phev_events[phev_range_type=='short'][sample(1:nrow(possible_phev_events[phev_range_type=='short']), (num_PHEVs *(1-percent_long_PHEV)* phev.short.charge.factor), replace = T)]
        
        multiday_charge_prof$bev_load_longrange <- map_dbl(multiday_charge_prof$tenmin_index, get_bev_loads, event_sample = longrange_bev_events)
        multiday_charge_prof$bev_load_shortrange <- map_dbl(multiday_charge_prof$tenmin_index, get_bev_loads, event_sample = shortrange_bev_events)
        multiday_charge_prof$phev_load_longrange <- map_dbl(multiday_charge_prof$tenmin_index, get_phev_loads, event_sample = longrange_phev_events)
        multiday_charge_prof$phev_load_shortrange <- map_dbl(multiday_charge_prof$tenmin_index, get_phev_loads, event_sample = shortrange_phev_events)
        # deal with events that span two days
        multiday_charge_prof$mod_index <- multiday_charge_prof$tenmin_index %% 144
        charge_prof <- multiday_charge_prof[, .(bev_longrange_load = sum(bev_load_longrange),bev_shortrange_load=sum(bev_load_shortrange), phev_longrange_load = sum(phev_load_longrange), phev_shortrange_load = sum(phev_load_shortrange)), by = mod_index]
        charge_prof$all_bev_load <- charge_prof$bev_longrange_load + charge_prof$bev_shortrange_load
        charge_prof$all_phev_load <- charge_prof$phev_longrange_load + charge_prof$phev_shortrange_load
        charge_prof$all_load <- charge_prof$bev_longrange_load + charge_prof$bev_shortrange_load + charge_prof$phev_longrange_load + charge_prof$phev_shortrange_load
        
        #debug check:;
        #print(paste("max of all_phev_load: ",max(charge_prof$all_phev_load)))
        #print(paste("max of all_bev_load: ",max(charge_prof$all_bev_load)))
        
        #get max load by hour
        charge_prof$hour <- floor(charge_prof$mod_index / 6)
        hourly_prof <- charge_prof[,.(bev_load_long=max(bev_longrange_load),bev_load_short=max(bev_shortrange_load),phev_load_long=max(phev_longrange_load),phev_load_short=max(phev_shortrange_load),load_total=max(all_load)),by = hour]
        max_loads<-rbind(max_loads, max(hourly_prof$load_total))
        all_loads<-rbind(all_loads,hourly_prof)
        print(paste("Trial ",i,"of ",num_trials))
    }
    #set attributes for use later
    setattr(all_loads,"num_BEVs",num_BEVs)
    return(all_loads)
}
# End of this hella long function "sampleLoads"


######## TEST ONE INDIVIDUAL FEEDER-LEVEL EXPECTED PEV LOAD PROFILE############

BEVs. <- 50
PHEVs. <- 50
trials. <- 30
percent.long.bevs <- .84 #is 0.77 based on 2017-2019 CVRP data
percent.long.phevs <- .58 #is 0.56 based on 2017-2019 CVRP data
days.of.week <- "all"
month. <- "all"
utility. <- "all"
charging.level <- "all"
#test_load <- NA
test_load <- sampleLoads(num_trials=trials., num_BEVs=BEVs., percent_long_BEV=percent.long.bevs, num_PHEVs=PHEVs., percent_long_PHEV=percent.long.phevs, days_of_week = days.of.week, utility = utility., month = month.,charging_level = charging.level)

test_load$hourly_load_per_car <- test_load$load_total / (BEVs.+PHEVs.)
kwh_per_day <- sum(test_load[,mean(hourly_load_per_car),by=hour]$V1) #average energy used per car in model

p_boxplot <- ggplot(test_load,aes(x=hour,y=load_total,group=hour))+
    geom_boxplot()+
    ggtitle(paste(" trials =", trials.,"BEVs =",BEVs.," PHEVs =",PHEVs.," fraction longrange BEVs =",percent.long.bevs, " fraction longrange PHEV =",percent.long.phevs," Days =",days.of.week," month =",month.))
p_boxplot
#
#graph an "average" vehicle's profile based on above criteria
p_lineplot <- ggplot(test_load,aes(x=hour,y=hourly_load_per_car,group = ))+
    geom_point(alpha=.3)+
    #stat_smooth(size=1.5)+
    geom_smooth(stat = "summary",alpha=.4,fill='red',color='red',fun.data = median_hilow,fun.args = list(conf.int=.5))+
    ggtitle(paste(" trials =", trials.,"BEVs =",BEVs.," PHEVs =",PHEVs.," fraction longrange BEVs =",percent.long.bevs, " fraction longrange PHEV =",percent.long.phevs," Days =",days.of.week," month =",month.))

p_lineplot    

# get a stacked area plot of AVERAGE load by type of vehicle
test_load_reshaped <- melt(test_load[, !c("load_total","hourly_load_per_car")],id.vars = "hour") #make datatable long, remove total
test_load_means <- test_load_reshaped[,.(average_load_kW = mean(value)),by=.(hour,variable)]
test_load_means$variable <- factor(test_load_means$variable,rev(levels(test_load_means$variable)))
p_disag <- ggplot(test_load_means,aes(x=hour,y=average_load_kW,fill=variable))+
    geom_area(position = 'stack')+
    ggtitle(paste(" trials =", trials.,"BEVs =",BEVs.," PHEVs =",PHEVs.," fraction longrange BEVs =",percent.long.bevs, " fraction longrange PHEV =",percent.long.phevs," Days =",days.of.week," month =",month.))

p_disag

####### NICE GRAPHS######
#get baseline load profile data for analysis. each of these blank datatables will be displayed in a graph
#Note: currently only works for num_trials = 1 ...the goal is not uncertainty analyis 
compare_bevs_1 <- data.table("hour"=c(0:23))
compare_phevs_1 <- data.table("hour"=c(0:23))
compare_ratios_1 <- data.table("hour"=c(0:23))
compare_weekday <- data.table("hour"=c(0:23))
compare_month <- data.table("hour"=c(0:23))
compare_season <- data.table("hour"=c(0:23))
compare_utility <- data.table("hour"=c(0:23))
compare_charge_level <- data.table("hour"=c(0:23))

big_num <-10000


#BEVs only
compare_bevs_1$`100% long-range BEVs` <- sampleLoads(num_trials=1, num_BEVs=big_num, percent_long_BEV=1, num_PHEVs=0, percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total /big_num
compare_bevs_1$`Modeled (77% long-range BEVs)` <- sampleLoads(num_trials=1, num_BEVs=big_num, percent_long_BEV=percent.long.bevs, num_PHEVs=0, percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total /big_num
compare_bevs_1$`Original (40% long-range BEVs)` <- sampleLoads(num_trials=1, num_BEVs=big_num, percent_long_BEV=.403, num_PHEVs=0, percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total /big_num
compare_bevs_1$`0% long-range BEVs` <- sampleLoads(num_trials=1, num_BEVs=big_num, percent_long_BEV=0, num_PHEVs=0, percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total /big_num

p_compare_bevs_1 <- ggplot(melt(compare_bevs_1,id.vars = "hour",variable.name = "BEV type scenario"),aes(x=ordered(as.factor(hour)),y=value,color=`BEV type scenario`,group=`BEV type scenario`))+
    #geom_point()+
    geom_line(size=1.3,alpha=.8)+
    xlab("Hour of day")+
    ylab("Mean hourly load per vehicle per day (kW)")+
    theme_bw()+
    theme(legend.position='bottom')+
    guides(color=guide_legend(nrow=2))

p_compare_bevs_1
ggsave(p_compare_bevs_1,file='figures/simulated_loadProfiles_bev.pdf',height=6,width=9)
ggsave(p_compare_bevs_1,file='figures/simulated_loadProfiles_bev.png',height=6,width=9)

#get a measure of daily energy used (kwh)
round(compare_bevs_1[,!'hour'][,colSums(.SD)],digits = 1)


#PHEVs only
compare_phevs_1$`100% long-range PHEVs` <- sampleLoads(num_trials=1, num_BEVs=0, percent_long_BEV=percent.long.bevs, num_PHEVs=big_num, percent_long_PHEV=1, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total /big_num
compare_phevs_1$`Modeled (56% long-range PHEVs)` <- sampleLoads(num_trials=1, num_BEVs=0, percent_long_BEV=percent.long.bevs, num_PHEVs=big_num, percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total /big_num
compare_phevs_1$`Original (47% long-range PHEVs)` <- sampleLoads(num_trials=1, num_BEVs=0, percent_long_BEV=percent.long.bevs, num_PHEVs=big_num, percent_long_PHEV=.468, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total /big_num
compare_phevs_1$`0% long-range PHEVs` <- sampleLoads(num_trials=1, num_BEVs=0, percent_long_BEV=percent.long.bevs, num_PHEVs=big_num, percent_long_PHEV=0, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total /big_num

#graph it!
p_compare_phevs_1 <- ggplot(melt(compare_phevs_1,id.vars = "hour",variable.name = "PHEV type scenario"),aes(x=ordered(as.factor(hour)),y=value,color=`PHEV type scenario`,group=`PHEV type scenario`))+
    #geom_point()+
    geom_line(size=1.3,alpha=.8)+
    xlab("Hour of day")+
    ylab("Mean hourly load per vehicle per day (kW)")+
    theme_bw()+
    theme(legend.position='bottom')+
    guides(color=guide_legend(nrow=2))

p_compare_phevs_1
ggsave(p_compare_phevs_1,file='figures/simulated_loadProfiles_phev.pdf',height=6,width=9)
ggsave(p_compare_phevs_1,file='figures/simulated_loadProfiles_phev.png',height=6,width=9)

#get a measure of daily energy used (kwh)
round(compare_phevs_1[,!'hour'][,colSums(.SD)],digits = 1)

# Now show profiles based on different BEV / PHEV ratios
compare_ratios_1$`90% BEVs` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.9), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.9), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total / big_num
compare_ratios_1$`75% BEVs` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total / big_num
compare_ratios_1$`60% BEVs` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.6), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.6), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total / big_num
compare_ratios_1$`45% BEVs` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.45), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.45), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total / big_num

#graph it!
p_compare_ratios_1 <- ggplot(melt(compare_ratios_1,id.vars = "hour",variable.name = "BEV share of total PEVs scenario"),aes(x=ordered(as.factor(hour)),y=value,color=`BEV share of total PEVs scenario`,group=`BEV share of total PEVs scenario`))+
    #geom_point()+
    geom_line(size=1.3,alpha=.8)+
    xlab("Hour of day")+
    ylab("Mean hourly load per vehicle per day (kW)")+
    theme_bw()+
    theme(legend.position='bottom')+
    guides(color=guide_legend(nrow=2))

p_compare_ratios_1
ggsave(p_compare_ratios_1,file='figures/simulated_loadProfiles_ratio.pdf',height=6,width=9)
ggsave(p_compare_ratios_1,file='figures/simulated_loadProfiles_ratio.png',height=6,width=9)

#get a measure of daily energy used (kwh)
round(compare_ratios_1[,!'hour'][,colSums(.SD)],digits = 1)

# Check Weekend / Weekday (75% BEVs)
compare_weekday$`All days` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total / big_num
compare_weekday$`Weekdays only` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "Weekdays", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total / big_num
compare_weekday$`Weekends only` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "Weekends", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total / big_num

#graph weekends/weekdays
p_compare_weekday <- ggplot(melt(compare_weekday,id.vars = "hour",variable.name = "Weekday / Weekend scenario"),aes(x=ordered(as.factor(hour)),y=value,color=`Weekday / Weekend scenario`,group=`Weekday / Weekend scenario`))+
    geom_point()+
    geom_line(size=1.3,alpha=.8)+
    xlab("Hour of day")+
    ylab("Mean hourly load per vehicle per day (kW)")+
    theme_bw()+
    theme(legend.position='bottom')+
    guides(color=guide_legend(nrow=2))

p_compare_weekday
ggsave(p_compare_weekday,file='figures/simulated_loadProfiles_weekday.pdf',height=6,width=9)
ggsave(p_compare_weekday,file='figures/simulated_loadProfiles_weekday.png',height=6,width=9)

round(compare_weekday[,!'hour'][,colSums(.SD)],digits = 1)

# compare the months
compare_month$`Jan` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "January", charging_level = "all")$load_total / big_num
compare_month$`Feb` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "February", charging_level = "all")$load_total / big_num
compare_month$`Mar` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "March", charging_level = "all")$load_total / big_num
compare_month$`Apr` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "April", charging_level = "all")$load_total / big_num
compare_month$`May` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "May", charging_level = "all")$load_total / big_num
compare_month$`June` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "June", charging_level = "all")$load_total / big_num
compare_month$`July` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "July", charging_level = "all")$load_total / big_num
compare_month$`Aug` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "August", charging_level = "all")$load_total / big_num
compare_month$`Sep` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "September", charging_level = "all")$load_total / big_num
compare_month$`Oct` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "October", charging_level = "all")$load_total / big_num
compare_month$`Nov` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "November", charging_level = "all")$load_total / big_num
compare_month$`Dec` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "December", charging_level = "all")$load_total / big_num

#Graph the months data
p_compare_month <- ggplot(melt(compare_month,id.vars = "hour",variable.name = "Month"),aes(x=ordered(as.factor(hour)),y=value,color=Month,group=Month))+
    geom_point()+
    geom_line(size=1,alpha=.8)+
    #scale_x_discrete(limits=peak_time)+
    xlab("Hour of day")+
    ylab("Mean hourly load per vehicle per day (kW)")+
    theme_bw()+
    theme(legend.position='bottom')+
    guides(color=guide_legend(nrow=2))


p_compare_month
ggsave(p_compare_month,file='figures/simulated_loadProfiles_month.pdf',height=6,width=9)
ggsave(p_compare_month,file='figures/simulated_loadProfiles_month.png',height=6,width=9)
round(compare_month[,!'hour'][,colSums(.SD)],digits = 1)

# Compare Seasons
compare_season$Winter <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = c("December","January","February"), charging_level = "all")$load_total / big_num
compare_season$Spring <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = c("March","April","May"), charging_level = "all")$load_total / big_num
compare_season$Summer <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = c("June","July","August"), charging_level = "all")$load_total / big_num
compare_season$Fall <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = c("September","October","November"), charging_level = "all")$load_total / big_num

#Graph the Seasons data
p_compare_season <- ggplot(melt(compare_season,id.vars = "hour",variable.name = "Season"),aes(x=ordered(as.factor(hour)),y=value,color=Season,group=Season))+
    geom_point()+
    geom_line(size=1,alpha=.8)+
    #scale_x_discrete(limits=peak_time)+
    xlab("Hour of day")+
    ylab("Mean hourly load per vehicle per day (kW)")+
    theme_bw()+
    theme(legend.position='bottom')+
    guides(color=guide_legend(nrow=2))

p_compare_season
ggsave(p_compare_season,file='figures/simulated_loadProfiles_season.pdf',height=6,width=9)
ggsave(p_compare_season,file='figures/simulated_loadProfiles_season.png',height=6,width=9)
round(compare_season[,!'hour'][,colSums(.SD)],digits = 1)

#Compare level 1 vs level 2

compare_charge_level$`Level 1 and Level 2` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total / big_num
compare_charge_level$`Level 2 only` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "LEVEL_2")$load_total / big_num
compare_charge_level$`Level 1 only` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "LEVEL_1")$load_total / big_num

#Graph the charging level data
p_compare_charge_level <- ggplot(melt(compare_charge_level,id.vars = "hour",variable.name = "Charging level"),aes(x=ordered(as.factor(hour)),y=value,color=`Charging level`,group=`Charging level`))+
    geom_point()+
    geom_line(size=1,alpha=.8)+
    scale_x_discrete(limits=peak_time)+
    xlab("Hour of day")+
    ylab("Mean hourly load per vehicle per day (kW)")+
    theme_bw()+
    theme(legend.position='bottom')+
    guides(color=guide_legend(nrow=2))


p_compare_charge_level
ggsave(p_compare_charge_level,file='figures/simulated_loadProfiles_chrglvl.pdf',height=6,width=9)
ggsave(p_compare_charge_level,file='figures/simulated_loadProfiles_chrglvl.png',height=6,width=9)
round(compare_charge_level[,!'hour'][,colSums(.SD)],digits = 1)

#Compare by utility
compare_utility$`PG&E` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")$load_total / big_num
compare_utility$`SCE` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Southern California Edison", month = "all", charging_level = "all")$load_total / big_num
compare_utility$`SDG&E` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "San Diego Gas & Electric", month = "all", charging_level = "all")$load_total / big_num
compare_utility$`SMUD` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Sacramento Municipal Utility District", month = "all", charging_level = "all")$load_total / big_num
compare_utility$`LADWP` <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Los Angeles Department of Water & Power", month = "all", charging_level = "all")$load_total / big_num
compare_utility$Others <- sampleLoads(num_trials=1, num_BEVs=big_num*(.75), percent_long_BEV=percent.long.bevs, num_PHEVs=big_num*(1-.75), percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = c("Modesto Irrigation District","Silicon Valley Power","City of Healdsburg Electric Department","City of Cerritos","Alameda Power & Telecom","Roseville Electric","Azusa Light & Power","City of Anaheim Public Utilities Department","City of Palo Alto"), month = "all", charging_level = "all")$load_total / big_num
#Graph utility data
p_compare_utilities <- ggplot(melt(compare_utility,id.vars = "hour",variable.name = "Service Provider"),aes(x=ordered(as.factor(hour)),y=value,color=`Service Provider`,group=`Service Provider`))+
    geom_point()+
    geom_line(size=1,alpha=.8)+
    #scale_x_discrete(limits=peak_time)+
    xlab("Hour of day")+
    ylab("Mean hourly load per vehicle per day (kW)")+
    theme_bw()+
    theme(legend.position='bottom')+
    guides(color=guide_legend(nrow=2))


p_compare_utilities
round(compare_utility[,!'hour'][,colSums(.SD)],digits = 1)
ggsave(p_compare_utilities,file='figures/simulated_loadProfiles_utility.pdf',height=6,width=9)
ggsave(p_compare_utilities,file='figures/simulated_loadProfiles_utility.png',height=6,width=9)