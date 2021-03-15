# author: gina
# created: 6/1/2020
# purpose: process apsim data (pro_rawapdat.rds)


rm(list = ls())

library(tidyverse)
library(stringr)
library(janitor)
library(readxl)
#remotes::install_github("vanichols/saapsim")
library(saapsim) #--has some conversion things
library(purrr)



#--eliminate the things we've decided not to use
dat <- read_rds("01_proc-raw-outs/pro_rawapdat.rds") %>% 
  select(-date, -annual_rain_mm, -inseason_rain_mm, -drainage_mm, -nitrateflow_mg_l, -nyear, -doy)

#--pull out site from file name
dat2 <- 
  dat %>%  
  separate(file, into = c("site", "rot", "nrate"), sep = "_") %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, stringr::str_trim) 

#--get only real data (not spin up, not soybean, etc.)
dat3 <- 
  dat2 %>% 
  mutate(site_id = stringr::str_sub(site, 1, 4)) %>% 
  select(-site, -rot, -nrate) %>% 
  mutate(crop = ifelse(yield_soy_buac == 0, "corn", "soy")) %>% #--make the crop explicit
  #filter(yield_maize_buac != 0) %>% #--keep only maize data
  #--Heather says spin is 1994-1998. Start in 1999
  #--based on figs, it should start in 2000, not 1999
  filter(year >= 2000) %>% 
  #--remove hoff year 2012
  filter( ! (year == 2012 & site_id == "hoff")) %>% 
  rename(nrate_kgha = n_rate_kgha) %>% 
  select(site_id, year, nrate_kgha, rotation, crop, everything())


#--remove sutherland from analysis (not calibrated correctly, apparently)
dat4 <- 
  dat3 %>% 
  filter(site_id != "suth") 

# this is tricky. I need to separate the leaching from teh other data for this
# create a small dummy set

###---leaching
dleach4 <- 
  dat4 %>% 
  select(site_id, nrate_kgha, rotation, crop, year, pre_leaching_kgha, in_leaching_kgha, post_leaching_kgha)

#--calculate leaching from sowing to sowing
dleach5 <- 
  dleach4 %>% 
  arrange(site_id, rotation, nrate_kgha, year) %>% 
  group_by(site_id, nrate_kgha, rotation) %>% 
  mutate(pre_lead = dplyr::lead(pre_leaching_kgha, n = 1, default = NA),
         nyear_leach_kgha = pre_lead + in_leaching_kgha + post_leaching_kgha) %>% 
  select(-pre_leaching_kgha, -in_leaching_kgha, -post_leaching_kgha, -pre_lead) %>% 
  ungroup()

#--find first year of simulation, if it's soybean we want to eliminate it
dleach6 <- 
  dleach5 %>%
  select(year, site_id, rotation, crop, nyear_leach_kgha, nrate_kgha) %>% 
  group_by(site_id, rotation) %>% 
  mutate(minyear = min(year),
         getridof = ifelse( (minyear == year & crop == "soy"), "x", "keep")) %>% 
  filter(getridof != "x") %>% 
  select(-minyear, -getridof)
  
#--make a year to group by and sum
dleach7 <- 
  dleach6 %>% 
  mutate(yearleach_id = ifelse(crop == "corn", year, year-1)) %>% 
  group_by(yearleach_id, site_id, rotation, nrate_kgha) %>% 
  summarise(nyear_leach_kgha_tot = sum(nyear_leach_kgha)) %>% 
  rename(year = "yearleach_id")
  

dat5 <- 
  dat4 %>%
  left_join(dleach7) %>% 
  select(site_id, rotation, crop, nrate_kgha, year, leaching_kgha, nyear_leach_kgha_tot, everything())
  

write_csv(dat5, "01_proc-raw-outs/pro_apdat.csv")



# make QC-plots ------------------------------------------------------------



#--leaching

leach <- 
  dat4 %>% 
    select(site_id, year, rotation, crop, leaching_kgha, nrate_kgha) %>% 
  arrange(site_id, year, nrate_kgha)


leach %>%
  filter(site_id == 'gent') %>% 
  ggplot(., aes(nrate_kgha, leaching_kgha, color = crop)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(rotation ~ year) +
  labs(title = "leaching") + 
  theme(axis.text.x = element_text(angle = 90))

plots <-
  leach %>%
  split(.$site_id) %>%
  map( ~ (
    ggplot(., aes(nrate_kgha, leaching_kgha, color = crop)) +
      geom_point(size = 2) +
      geom_line() +
      facet_grid(rotation ~ year) +
      labs(title = "leaching") + 
      theme(axis.text.x = element_text(angle = 90))
    
  ))

paths <- stringr::str_c(names(plots), ".png")

pwalk(list(paths, plots), ggsave, path = "01_proc-raw-outs/figs/leach/", width = 10, height = 4)


#--yield

yld <- 
  dat3 %>% 
  select(site_id, year, rotation, yield_maize_buac, nrate_kgha) %>% 
  arrange(site_id, year, nrate_kgha)


yld %>%
  filter(site_id == 'gent') %>% 
  ggplot(., aes(nrate_kgha, yield_maize_buac)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(rotation ~ year) +
  labs(title = "corn") + 
  theme(axis.text.x = element_text(angle = 90))

plots <-
  yld %>%
  split(.$site_id) %>%
  map( ~ (
    ggplot(., aes(nrate_kgha, yield_maize_buac)) +
      geom_point(size = 2) +
      geom_line() +
      facet_grid(rotation ~ year) +
      labs(title = "corn") + 
      theme(axis.text.x = element_text(angle = 90))
    
  ))

paths <- stringr::str_c(names(plots), ".png")

pwalk(list(paths, plots), ggsave, path = "01_proc-raw-outs/figs/ylds/", width = 10, height = 4)

