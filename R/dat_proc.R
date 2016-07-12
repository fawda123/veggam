######
# processing and combining fish/veg data

library(dplyr)
library(tidyr)
library(purrr)
library(maptools)
library(readxl)
library(sp)

######
# process veg data

rm(list = ls())
source('R/funcs.R')

# raw transect data from other project
data(veg_dat)

# # file of unique species names and counts, for manual correction below
# dat <- select(veg_dat, dow, date, scientific_name, growth_form) %>% 
#   unique
# unis <- data.frame(sort(table(dat$scientific_name)))
# write.table(unis, 'ignore/rec  s.txt', quote = F, row.names = F, sep = '\t')

keys <- read.csv('ignore/recs.csv', stringsAsFactors = F)

# summarized veg data, modify this to include p/a for other species
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
    pc = ifelse('Potamogeton crispus' %in% sp, 1, 0),
    ms = ifelse('Myriophyllum spicatum' %in% sp, 1, 0),
    pa = ifelse('Potamogeton amplifolius' %in% sp, 1, 0), 
    cd = ifelse('Ceratophyllum demersum' %in% sp, 1, 0), 
    va = ifelse('Vallisneria americana' %in% sp, 1, 0),
    richnat = richsub - sum(pc, ms)
    ) %>% 
  ungroup

# all lake utm coordinates in MN, zone 15N
lk_locs <- foreign::read.dbf('ignore/10k_pts.dbf') %>% 
  select(MAIN_DOW, UTM_X, UTM_Y) %>% 
  mutate(dowjn = as.character(as.numeric(as.character(MAIN_DOW)))) %>% 
  select(-MAIN_DOW) %>% 
  group_by(dowjn) %>% 
  summarize(
    utmx = mean(UTM_X, na.rm = TRUE),
    utmy = mean(UTM_Y, na.rm = TRUE)
  )
 
# add UTM coords
veg_rch <- mutate(veg_rch, dowjn = gsub('[0-9][0-9]$', '00', dow)) %>%  
  left_join(., lk_locs, by = 'dowjn') %>% 
  select(-dowjn)

# combine with covariates, final cov list tbd, convert all units to metric
# species p/a combined to factor
covdat <- read.table('ignore/MNDNRwatersheds.txt', sep = ',', header = T)
veg_rch <- mutate(veg_rch, DOWNUM = as.integer(gsub('[0-9][0-9]$', '', dow))) %>% 
  left_join(., covdat, by = 'DOWNUM') %>% 
  select(dow, date, richtot, richsub, pc, ms, pa, cd, va, richnat, depthft, LKACRES, MeanTotalP, secchi, alkalinity, utmx, utmy) %>% 
  unite('facs', pc, ms, sep = '', remove = F) %>% 
  mutate(
    Invasive = factor(facs, 
      levels = c('00', '10', '01', '11'), 
      labels = c('none', 'pc', 'ms', 'both')
      ),
    depthft = depthft * 0.3048, # m
    LKACRES = LKACRES * 0.00404686, # km2 
    secchi = secchi * 0.3048 # m
  ) %>% 
  rename(
    depthm = depthft, 
    areakm = LKACRES,
    secchim = secchi, 
    totalp = MeanTotalP
  ) %>% 
  select(-facs)

# get ecoregions
# lakes as spatialpointsdataframe for overlay
veg_rch_pts <- select(veg_rch, dow, utmx, utmy) %>% 
  data.frame %>% 
  unique
dow <- veg_rch_pts$dow
veg_rch_pts <- with(veg_rch_pts, cbind(utmx, utmy))
rownames(veg_rch_pts) <- dow
veg_rch_pts <- na.omit(veg_rch_pts)
veg_rch_pts <- SpatialPointsDataFrame(veg_rch_pts, data = data.frame(dow = rownames(veg_rch_pts)))

# overlay lake locations with ecoreg shapefile  
data(ecoregs)
ecoreg <- over(veg_rch_pts, ecoregs) %>% 
  data.frame(
    dow = rownames(.), 
    ecoreg = .$US_L3NAME
  ) %>% 
  select(dow, ecoreg) %>% 
  mutate(
    dow = as.character(dow),
    ecoreg = as.character(ecoreg), 
    ecoreg = factor(ecoreg, 
      levels = c( 'Driftless Area', 'Lake Agassiz Plain', 'North Central Hardwood Forests', 'Northern Glaciated Plains', 'Northern Lakes and Forests', 'Northern Minnesota Wetlands', 'Western Corn Belt Plains'),
      labels = c('DA', 'LAP', 'NCHF', 'NGP', 'NLF', 'NMW', 'WCBP')
      )
    ) %>% 
  na.omit

# add ecoregion to veg_rch
veg_rch <- left_join(veg_rch, ecoreg, by = 'dow')

save(veg_rch, file = 'data/veg_rch.RData', compress = 'xz')

######

