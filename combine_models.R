#This code should work on a mac as long as the working directory is set to the source file, and the appropriate input data files are in the right places.
#This code was written by Jake Highleyman (questions: jhighleyman@gmail.com)
# Many apologies for the subpar organization.


library(data.table)
#library(tidycensus) #TODO install for mac
#library(tidyverse) #TODO install for mac
library(ggplot2)
library(reshape2)
library(tibble)
library(purrr)
library(stringr)
library(RColorBrewer)
library(pals)
library(Hmisc)
library(gsubfn) 

######COPY STUFF FROM LOAD_PROFILES_CLEAN.R ######


#a few universals
start_at_8am <- c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,0,1,2,3,4,5,6,7)
peak_time <- c(16,17,18,19,20,21,22) # 4 to 10pm

# Load evmt cleaned charge event data
# These are the main data sources that the "sampleLoads" function uses
# Note: the main output variable of interest is `energy[delta_soc][avg_kW].y`,
#   which is the average power used over the duration of a charge event
good_bev_events <- fread("data/events_cleaned/good_bev_events.csv")
good_phev_events <- fread("data/events_cleaned/good_phev_events.csv")


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



# Copied version of sampleLoads from "load_profiles_clean.R". need to check for bugs.
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







#### TAKE EV TOOLBOX DATA ######

toolbox_data <- fread("data/ev_toolbox_data/pev_blockgroup_scenarios.csv")
uniform_distr_data <- fread("data/ev_toolbox_data/uniform_adoption_data.csv")
bg_to_feeder_pge <- fread("data/blockgroups_to_feeders_pge.csv")

all_toolbox_data <- rbind(uniform_distr_data,toolbox_data,fill=T)
# Import PG&E load data
pge_feeder_loads <- fread("data/load_data/PGE/PGE_feeder_loads.csv")
#add seperate month and hour variables for PG&E
pge_feeder_loads$Month <- as.factor(substr(pge_feeder_loads$MonthHour,1,2))
pge_feeder_loads$Hour <- as.factor(as.double(substr(pge_feeder_loads$MonthHour,4,5)))

#TODO get the pge_peak_loads to include feeders with redacted load--there's still some useful info
pge_intersect_data <- fread("data/pge_intersect_data.csv") #not sure i actually need this
pge_peak_loads <- fread("data/pge_peak_loads.csv")

useful_feeder_info <- unique(pge_peak_loads)

# make Toolbox model data wide for easier modeling
toolbox_total_BEVs_wide <-dcast(all_toolbox_data,GEOID ~scenario,value.var = "BEV")
toolbox_total_PHEVs_wide <-dcast(all_toolbox_data,GEOID ~scenario,value.var = "PHEV")

###### Test graphs for some feeders #####
###### Note: Everything in the if statement is just for testing data analysis for individual or groups of feeders, not the whole data set which is processed below. To skip this section, you can always change the next line to "if(F){"
if(T){
    
    
    scenario_input <- "3M_bev75pct"
    test.feeder <- "DAVIS 1109" # can be any feeder
    input_month <- "08" #TODO: set month to automatically be the peak month
    trials_temp <- 30
    
    merged_toolbox_pge <- merge(all_toolbox_data[scenario==scenario_input],bg_to_feeder_pge,by.x ="GEOID",by.y= "blockgrp",all.x = T) #todo quality control, check for pge only?
    #this is a datatable of the number of BEVs and PHEVs allocated to each Feeder, 
    #according to whichever EV Toolbox scenario was indicated above.
    cars_by_feeder <- merged_toolbox_pge[,.(model_bevs = sum (BEV*bg_to_feeder),model_phevs=sum(PHEV*bg_to_feeder),model_households=sum(households_bg_feeder)),by=FeederID]
    
    cars_by_feeder$model_all_pevs <- cars_by_feeder$model_phevs +cars_by_feeder$model_bevs
    cars_by_feeder$model_cars_per_household <- cars_by_feeder$model_all_pevs /cars_by_feeder$model_households
    cars_by_feeder$model_name <- scenario_input
    
    master_feeder_datatable <- merge(useful_feeder_info,cars_by_feeder,by="FeederID",all.x=T)
    
    input_feeders <- master_feeder_datatable[Feeder_Nam==test.feeder]
    
    new_car_load <- data.table()
    for(i in 1:(nrow(input_feeders))){
        bev_temp <- input_feeders$model_bevs[i]
        phev_temp <- input_feeders$model_phevs[i]
        print(paste("bevs modeled: ",bev_temp,"; phevs modeled: ",phev_temp))
        temp_load <- sampleLoads(num_trials = trials_temp,num_BEVs = bev_temp,num_PHEVs = phev_temp,percent_long_BEV=percent.long.bevs,  percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")
        temp_load$FeederID <- input_feeders$FeederID[i]
        
        new_car_load <- rbind(new_car_load,temp_load)   
        
        print(paste("Feeder ",i,"of ",nrow(input_feeders)))
    }
    
    new_car_load$hour <- as.factor(new_car_load$hour)
    
    combined_load <- merge(new_car_load,pge_feeder_loads,by.x = c("FeederID","hour"),by.y = c("FeederID","Hour"),allow.cartesian=T)
    
    sjLoad <-combined_load
    
    relevant_feeders <- unique(combined_load$FeederID)
    
    # make plots of range of new load mapped on existing high loads
    # 

    plotz <-list()
    for (feeder.i in relevant_feeders){
        one_month_data <- combined_load[Month==input_month & FeederID == feeder.i]
        one_year_data <- combined_load[FeederID==feeder.i]
        capacity <- pge_peak_loads[FeederID==feeder.i,capacity_kW][1]
        
        #TODO check this for bugs
        cool_plot <- ggplot() +

            geom_boxplot(data = one_month_data, aes(x=hour,y=(load_total+High),group=hour))+
            geom_point(data=one_month_data,aes(x=hour,y=High),color='orange')+
            geom_hline(yintercept = capacity,color='red')+
            ggtitle(paste("Scenario: ",scenario_input," Month = ",input_month,"  Feeder =",pge_intersect_data[FeederID==feeder.i,Feeder_Nam][1],"  Feeder capacity =",capacity,"kW","  BEVs =",round(input_feeders[FeederID==feeder.i,model_bevs],digits = 0),"  PHEVs = ",round(input_feeders[FeederID==feeder.i,model_phevs],digits = 0),"  trials =", trials_temp)) +
            ylim(0,capacity+.1*capacity)
        
        #multiple months plot
        line_plot_year <- ggplot(one_year_data,aes(x=as.numeric(hour),y=(load_total+High)))+
            #geom_point(alpha=.3)+
            #stat_smooth(size=1.5)+
            geom_hline(yintercept = capacity,color='blue')+
            geom_smooth(stat = "summary",alpha=.4,fill='darkorchid2',color='darkorchid2',fun.data = median_hilow,fun.args = list(conf.int=.5))+
            geom_line(aes(x=as.numeric(hour),y=High))+
            ggtitle(paste("Scenario: ",scenario_input," Feeder =",pge_intersect_data[FeederID==feeder.i,Feeder_Nam][1],"  Feeder capacity =",capacity,"kW","  BEVs =",round(input_feeders[FeederID==feeder.i,model_bevs],digits = 0),"  PHEVs = ",round(input_feeders[FeederID==feeder.i,model_phevs],digits = 0),"  trials =", trials_temp))+
            facet_wrap(vars(Month))+
            xlab("Hour of Day")+
            ylab("Load (kW)")+
            scale_x_continuous(n.breaks=13)
        
        
        
        
        # make lineplot
        line_plot_temp <- ggplot(one_month_data,aes(x=as.numeric(hour),y=(load_total+High)))+
            geom_point(alpha=.3)+
            #stat_smooth(size=1.5)+
            geom_smooth(stat = "summary",alpha=.4,fill='red',color='red',fun.data = median_hilow,fun.args = list(conf.int=.5))+
            geom_line(aes(x=as.numeric(hour),y=High))+
            ggtitle(paste("Scenario: ",scenario_input," Month = ",input_month,"  Feeder =",pge_intersect_data[FeederID==feeder.i,Feeder_Nam][1],"  Feeder capacity =",capacity,"kW","  BEVs =",round(input_feeders[FeederID==feeder.i,model_bevs],digits = 0),"  PHEVs = ",round(input_feeders[FeederID==feeder.i,model_phevs],digits = 0),"  trials =", trials_temp))+
            xlab("Hour of Day")+
            ylab("Load (kW)")+
            scale_x_continuous(n.breaks=13)
        
        # make lineplot with feeder limit
        line_plot_limit <- ggplot(one_month_data,aes(x=as.numeric(hour),y=(load_total+High)))+
            geom_point(alpha=.3)+
            #stat_smooth(size=1.5)+
            geom_hline(yintercept = capacity,color='blue')+
            geom_smooth(stat = "summary",alpha=.4,fill='darkorchid2',color='darkorchid2',fun.data = median_hilow,fun.args = list(conf.int=.5))+
            geom_line(aes(x=as.numeric(hour),y=High))+
            ggtitle(paste("Scenario: ",scenario_input," Month = ",input_month,"  Feeder =",pge_intersect_data[FeederID==feeder.i,Feeder_Nam][1],"  Feeder capacity =",capacity,"kW","  BEVs =",round(input_feeders[FeederID==feeder.i,model_bevs],digits = 0),"  PHEVs = ",round(input_feeders[FeederID==feeder.i,model_phevs],digits = 0),"  trials =", trials_temp))+
            xlab("Hour of Day")+
            ylab("Load (kW)")+
            scale_x_continuous(n.breaks=13)
        
        
        
        # make lineplot with EV load only
        line_plot_ev <- ggplot(one_month_data,aes(x=as.numeric(hour),y=(load_total)))+
            geom_point(alpha=.2)+
            #stat_smooth(size=1.5)+
            geom_smooth(stat = "summary",alpha=.4,fill='darkorchid2',color='darkorchid2',fun.data = median_hilow,fun.args = list(conf.int=.5))+
            #geom_line(aes(x=as.numeric(hour),y=High))+
            ggtitle(paste("Scenario: ",scenario_input," Month = ",input_month,"  Feeder =",pge_intersect_data[FeederID==feeder.i,Feeder_Nam][1],"  Feeder capacity =",capacity,"kW","  BEVs =",round(input_feeders[FeederID==feeder.i,model_bevs],digits = 0),"  PHEVs = ",round(input_feeders[FeederID==feeder.i,model_phevs],digits = 0),"  trials =", trials_temp))+
            xlab("Hour of day")+
            ylab("Load (kW)")+
            scale_x_continuous(n.breaks=13)
        
        print(line_plot_limit)
        print(line_plot_temp)     
        print(line_plot_ev)
        print(line_plot_year)
        print(cool_plot)
        plotz <- rbind(plotz,cool_plot)
        
    }
    
    print(plotz[2])
    
    
}

######CREATE THE UNIT LOAD PROFILE#######
#this is the median load per BEV and per PHEV from all the charging data
# This does NOT carry the uncertainty with it
percent.long.bevs <- .84 #is 0.77 based on 2017-2019 CVRP data
percent.long.phevs <- .58 #is 0.56 based on 2017-2019 CVRP data
days.of.week = "all"
month. = "all"
divisor <- 100
trials_for_unit_profile <- 30
unit_load_bev <- sampleLoads(num_trials = trials_for_unit_profile,num_BEVs = divisor,num_PHEVs = 0,percent_long_BEV=percent.long.bevs,  percent_long_PHEV=percent.long.phevs, days_of_week = days.of.week, utility = "all", month = month., charging_level = "all")[, .(hour=hour,unit_load = load_total / divisor)]
unit_load_phev <- sampleLoads(num_trials = trials_for_unit_profile,num_BEVs = 0,num_PHEVs = divisor,percent_long_BEV=percent.long.bevs,  percent_long_PHEV=percent.long.phevs, days_of_week = days.of.week, utility = "all", month = month., charging_level = "all")[, .(hour=hour,unit_load = load_total / divisor)]


p_unit_profile <- ggplot(unit_load_bev,aes(x=hour,y=unit_load*1000,group = ))+
    geom_point(alpha=.3)+
    #stat_smooth(size=1.5)+
    geom_smooth(stat = "summary",alpha=.4,fill='red',color='red',fun.data = median_hilow,fun.args = list(conf.int=.5))+
    xlab('Hour of Day')+
    ylab('Charging Load (kW)')+
    scale_x_continuous(n.breaks=13)+
    #ggtitle(paste(" trials =", trials_for_unit_profile,"BEVs (unit) =",divisor," PHEVs =",0," fraction longrange BEVs =",percent.long.bevs, " fraction longrange PHEV =",percent.long.phevs," Days =",days.of.week," month =",month.))
    theme_bw()
p_unit_profile

ggsave(p_unit_profile,file='figures/example_1000veh_loadProfile.pdf',height=6,width=9)
ggsave(p_unit_profile,file='figures/example_1000veh_loadProfile.png',height=6,width=9)

median_unit_load_bev <- unit_load_bev[,.(bev_load = median(unit_load)),by=hour]
median_unit_load_phev <- unit_load_phev[,.(phev_load = median(unit_load)),by=hour]


######### Do some aggregate analysis######
all_feeders_modeled <-data.table()
scenarios_modeled_60pct <- c("basecase","1M_bev60pct","2M_bev60pct","3M_bev60pct","4M_bev60pct","5M_bev60pct","6M_bev60pct")
scenarios_modeled_75pct <- c("basecase","1M_bev75pct","2M_bev75pct","3M_bev75pct","4M_bev75pct","5M_bev75pct","6M_bev75pct")
custom_scenarios <- c()
#set bev/phev ratio
#
input_scenarios <- scenarios_modeled_75pct #SET AS DESIRED

for (scen in input_scenarios){
    merged_toolbox_temp <- merge(all_toolbox_data[scenario==scen],bg_to_feeder_pge,by.x ="GEOID",by.y= "blockgrp",all.x = T) 
    
    cars_by_feeder_temp <- merged_toolbox_temp[,.(model_bevs = sum (BEV*bg_to_feeder),model_phevs=sum(PHEV*bg_to_feeder),model_households=sum(households_bg_feeder),model_name=scen),by=FeederID]
    
    cars_by_feeder_temp$model_all_pevs <- cars_by_feeder_temp$model_phevs +cars_by_feeder_temp$model_bevs
    cars_by_feeder_temp$model_cars_per_household <- cars_by_feeder_temp$model_all_pevs /cars_by_feeder_temp$model_households
    cars_by_feeder_temp$model_name <- scen
    
    master_feeder_datatable_temp <- merge(useful_feeder_info,cars_by_feeder_temp,by="FeederID",all.x=T)
    all_feeders_modeled <- rbind(all_feeders_modeled,master_feeder_datatable_temp)
}

# Start here for big run
#TODO decide whether to model load shape on only PG&E or not

input_feeders <- all_feeders_modeled
#trials_temp <- 30
new_car_load <- data.table()
for(i in 1:(nrow(input_feeders))){
    bev_temp <- input_feeders$model_bevs[i]
    phev_temp <- input_feeders$model_phevs[i]
    print(paste("bevs modeled: ",bev_temp,"; phevs modeled: ",phev_temp))
    #temp_load <- sampleLoads(num_trials = trials_temp,num_BEVs = bev_temp,num_PHEVs = phev_temp,percent_long_BEV=percent.long.bevs,  percent_long_PHEV=percent.long.phevs, days_of_week = "all", utility = "Pacific Gas & Electric Company", month = "all", charging_level = "all")
    #trying out the "median only method" for faster data processing. This says nothing about the uncertainty. Only the median load based on 30 runs of 100,000
    temp_load <- median_unit_load_bev
    temp_load <- merge(temp_load,median_unit_load_phev)
    temp_load$bev_load <- bev_temp * temp_load$bev_load
    temp_load$phev_load <- phev_temp * temp_load$phev_load
    temp_load$FeederID <- input_feeders$FeederID[i]
    temp_load$model_type <- input_feeders$model_name[i]
    temp_load$model_name <- input_feeders$model_name[i]
    
    temp_load$bevs_modeled <- bev_temp
    temp_load$phevs_modeled <- phev_temp
    new_car_load <- rbind(new_car_load,temp_load)   
    
    print(paste("Feeder ",i,"of ",nrow(input_feeders)))
    
}

new_car_load$hour <- as.factor(new_car_load$hour)

combined_load_large <- merge(new_car_load,pge_feeder_loads,by.x = c("FeederID","hour"),by.y = c("FeederID","Hour"),allow.cartesian=T)

all_data <- merge(combined_load_large,pge_peak_loads,by="FeederID") # note, .x endings are for realtime, .y endings are PEAK

all_data$newHighLoad <- all_data$High.x + all_data$bev_load + all_data$phev_load
all_data$loading <- all_data$High.x / all_data$capacity_kW
all_data$newLoading <- all_data$newHighLoad / all_data$capacity_kW
all_data$newHeadroom <- all_data$capacity_kW - all_data$newHighLoad
all_data$oldHeadroom <- all_data$capacity_kW - all_data$High.x



fwrite(all_data,"results/scenario_results/all.csv")

# you can run different scenario combos and save them as you like
# This script should end with writing of the results to file, and analysis of said results
# should be done in results_analysis.R or another file.
