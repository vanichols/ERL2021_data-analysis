# author: gina
# created: 7/6/2020
# purpose: explore relationship between yield and leaching parms
# last updated: 7/9/2020 fixed coefs
#               8/20/2020 removed sutherland

rm(list = ls())
library(tidyverse)
library(plotly)
library(scales)

rawdat <- read_csv("01_proc-raw-outs/pro_apdat.csv")
yldprms <- read_csv("02_fit-curves/fc_blin-yield-parms-mm.csv")
leach_prms <- read_csv("02_fit-curves/fc_blin-leach-parms-mm.csv")


# xs parm (pivot points)--------------------------------------------------

yld_xs <-
  yldprms %>% 
  pivot_longer(a:xs) %>% 
  filter(name == "xs") %>% 
  rename(yield_xs = value)

#--note leach xs is only specific to rotation
leach_xs <- 
  leach_prms %>%
  pivot_longer(a:c) %>% 
  filter(name == "xs") %>% 
  rename(leach_xs = value) %>% 
  distinct()


# combine them ------------------------------------------------------------

buff <- 
  yld_xs %>% 
  left_join(leach_xs) %>% 
  mutate(yld_to_lch = leach_xs - yield_xs) 


buff %>% 
  group_by(rotation) %>% 
  summarise(mn_buf = mean(yld_to_lch, na.rm = T))


buffer_diff <- 
  tidy(anova(lm(yld_to_lch ~ rotation*site, data = buff))) %>% 
  mutate(p.value = round(p.value, 3),
         thing = "buffer_diff")

buffer_diff %>% write_csv("02_fit-curves/fc_buffer-anova.csv")


