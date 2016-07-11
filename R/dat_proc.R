######
# processing and combining fish/veg data

library(dplyr)
library(tidyr)
library(purrr)
library(maptools)
library(readxl)

source('R/funcs.R')

######
# 
rm(list = ls())
source('R/funcs.R')

# get submersed species only
# id as native/nonnative

data(veg_dat)

# # file of unique species names and counts, for manual correction below
# dat <- select(veg_dat, dow, date, scientific_name, growth_form) %>% 
#   unique
# unis <- data.frame(sort(table(dat$scientific_name)))
# write.table(unis, 'ignore/rec  s.txt', quote = F, row.names = F, sep = '\t')

keys <- read.csv('ignore/recs.csv', stringsAsFactors = F)

veg_rch <- rename(keys, scientific_name = orig) %>%
  select(-Freq) %>% 
  left_join(veg_dat, ., by = 'scientific_name') %>% 
  select(dow, date, growth_form, comb) %>% 
  unique %>% 
  rename(sp = comb) %>% 
  group_by(dow, date) %>% 
  summarise(
    richtot = length(unique(sp)), 
    richsub = sum(growth_form %in% 'S'),
    cd_pres = ifelse('Ceratophyllum demersum' %in% sp, 1, 0),
    pc_pres = ifelse('Potamogeton crispus' %in% sp, 1, 0),
    ms_pres = ifelse('Myriophyllum spicatum' %in% sp, 1, 0),
    richnat = richsub - sum(cd_pres, pc_pres, ms_pres)
    ) %>% 
  ungroup

save(veg_rch, file = 'data/veg_rch.RData', compress = 'xz')



# # all lake utm coordinates in MN, zone 15N
# lk_locs <- foreign::read.dbf('ignore/10k_pts.dbf') %>% 
#   select(MAIN_DOW, UTM_X, UTM_Y) %>% 
#   mutate(dowjn = as.character(as.numeric(as.character(MAIN_DOW)))) %>% 
#   select(-MAIN_DOW) %>% 
#   group_by(dowjn) %>% 
#   summarize(
#     utmx = mean(UTM_X, na.rm = TRUE),
#     utmy = mean(UTM_Y, na.rm = TRUE)
#   )
# 
# # add UTM coords
# dat <- mutate(dat, dowjn = gsub('[0-9][0-9]$', '00', dow)) %>%  
#   left_join(., lk_locs, by = 'dowjn')
# 
# # covariates
# covdat <- read.table('ignore/MNDNRwatersheds.txt', sep = ',', header = T)
# 
# # get dnr ecoregions
# # lakes as spatialpointsdataframe for overlay
# fishveg_pts <- select(fishveg_dat, dow, utmx, utmy) %>% 
#   data.frame
# dow <- fishveg_pts$dow
# fishveg_pts <- with(fishveg_pts, cbind(utmx, utmy))
# rownames(fishveg_pts) <- dow
# fishveg_pts <- na.omit(fishveg_pts)
# fishveg_pts <- SpatialPointsDataFrame(fishveg_pts, data = data.frame(dow = rownames(fishveg_pts)))
# 
# # overlay lake locations with ecore shapefile  
# ecoreg <- over(fishveg_pts, ecoreg) %>% 
#   data.frame(
#     dow = rownames(.), 
#     ecoreg = tolower(.$NA_L1NAME)
#   ) %>% 
#   select(dow, ecoreg) %>% 
#   mutate(dow = as.character(dow)) %>% 
#   na.omit
# 
# # add ecoregion to fishveg_dat
# fishveg_dat <- left_join(fishveg_dat, ecoreg, by = 'dow')
