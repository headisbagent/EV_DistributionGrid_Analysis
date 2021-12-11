# This code produces the file "blocks_to_nearest_feeder.csv"
# note--that file is already in the directory, but if you want to change or test methodology you will need to update this.
# Adam wrote most of this snippet, so he can help you parse it out. 
# This code needs to be updated with either correct filepaths for the gdb file, or reworked to work without .gdb
# 

library(sf)
library(tidyverse)
library(data.table)
gdb <- "C:/Users/Jake/AppData/Local/ESRI/conda/envs/SCE map/SCE ICA/Default.gdb"

st_layers("C:/Users/Jake/AppData/Local/ESRI/conda/envs/SCE map/SCE ICA/Default.gdb")

blocks<-read_sf(gdb,"Block_Project")
feeders<-read_sf(gdb,"PGE_Feeders_zoomed_Project")

#buffered_blocks <-st_buffer(blocks,dist=(15))


feeder1 <- feeders %>% 
    slice(1) %>% 
    st_intersection(blocks)

blocks_nearest_feeder <- blocks %>% 
    slice(1:100) %>% 
    st_nearest_feature(feeders)

first_100_blocks <- blocks %>% 
    slice(1:100) %>% 
    mutate(FeederID = feeders$FeederID[blocks_nearest_feeder])

all_blocks_nearest_feeder <- blocks %>%
    st_nearest_feature(feeders)

feeders <- data.table(feeders)
blocks <- data.table(blocks)

feeders$FeederID[block]
blocks$nearest_feeder <- feeders$FeederID[all_blocks_nearest_feeder]

blocks_to_nearest_feeder <- data.table()
blocks_to_nearest_feeder$GEOID <- blocks$GEOID
blocks_to_nearest_feeder$FeederID <- blocks$nearest_feeder
fwrite(blocks_to_nearest_feeder,"blocks_to_nearest_feeder.csv")
