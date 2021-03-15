# author: gina
# created; 7/2/2020
# purpose: fit non-linear curves to yield vs nrate
# last updated: 8/20/2020, running without sutherland, separated from leaching code

rm(list = ls())

# libraries ---------------------------------------------------------------

#remotes::install_github("femiguez/nlraa")
library(nlraa)
library(nlme)
library(dplyr) #--overwrites collapse from nlme
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(emmeans)
library(car) #--overwrites recode in dplyr
library(minpack.lm)
library(janitor)
library(broom)
library(tibble)


# overview ----------------------------------------------------------------

dat <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,
                                  "sc" = "cs"),
         rotation = as.factor(rotation)) %>%
  select(nrate_kgha, everything()) %>% 
  pivot_longer(leaching_kgha:yield_maize_buac)


ylds <- 
  dat %>% 
  filter(name == "yield_maize_buac") %>% 
  filter(crop == "corn")

ylds1 <- 
  ylds %>%
  mutate(eu = paste(site_id, year, rotation, sep = "_")) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id))

yldsG <- groupedData(value ~ nrate_kgha | eu, data = ylds1)
yldsG$rotation <- as.factor(yldsG$rotation)

# fit curves --------------------------------------------------------------


#--fit model to each group
ymodG <- nlsList(value ~ SSlinp(nrate_kgha, a, b, xs), data = yldsG) 
#--a, b, and xs could have random components added
ymod1 <- nlme(ymodG, random = pdDiag(a + b + xs ~ 1))
fxf1 <- fixef(ymod1) #--we need these if we want to add an effect of rotation
#--note this an update, it keeps the random effects, we are just adding a fixed effect
ymod2 <- update(ymod1, 
               fixed = list(a + b + xs ~ rotation),
               start = c(fxf1[1], 0, #--a
                         fxf1[2], 0, #--b
                         fxf1[3], 0)) #--xs
fxf2 <- fixef(ymod2) #--now we have a value for cc and cs
#--FEM does something I don't quite get. 
# instead of modelling the variance, try adding two random effect levels?
ymod3a <- update(ymod2, random = list(site_id = pdDiag(a + b + xs ~ 1),
                                    eu = pdDiag(a + b + xs ~ 1)),
                groups = ~ site_id/eu)

# extract coefficients ----------------------------------------------------

#--ex from leaching
# coef(lmod3a, effects = "random") %>% 
#   rownames_to_column() %>% 
#   as_tibble() %>% 
#   separate(rowname, into = c("site", "x", "year", "rotation")) %>% 
#   pivot_longer(cols = 5:12, names_to = "term", values_to = "estimate") %>% 
#   mutate(term2 = str_sub(term, 1, 2),
#          term2 = str_replace_all(term2, "[[:punct:]]", ""),
#          termrot = ifelse(grepl("cs", term), "csterm", "ccterm")) %>%
#   select(-term) %>%
#   pivot_wider(names_from = termrot,
#               values_from = estimate) %>%
#   mutate(est = ifelse(rotation == "cs", ccterm + csterm, ccterm)) %>%
#   select(-ccterm, -csterm, -x) %>%
#   pivot_wider(names_from = term2, values_from = est)


yld_coefs <- 
  coef(ymod3a, effects = "random") %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  separate(rowname, into = c("site", "x", "year", "rotation")) %>% 
  pivot_longer(cols = 5:ncol(.), names_to = "term", values_to = "estimate") %>% 
  mutate(term2 = str_sub(term, 1, 2),
         term2 = str_replace_all(term2, "[[:punct:]]", ""),
         termrot = ifelse(grepl("cs", term), "csterm", "ccterm")) %>%
  select(-term) %>%
  pivot_wider(names_from = termrot,
              values_from = estimate) %>%
  mutate(est = ifelse(rotation == "cs", ccterm + csterm, ccterm)) %>%
  select(-ccterm, -csterm, -x) %>%
  pivot_wider(names_from = term2, values_from = est)

  
write_csv(yld_coefs, "02_fit-curves/fc_blin-yield-parms-mm.csv")


# try to get data for boxplots? -------------------------------------------

# What is spread in xs estimate?

yld_xs <- 
  yld_coefs %>% select(-a, -b)

write_csv(yld_xs, "02_fit-curves/fc_blin-yield-xs-mm.csv")




# use model to make preds -------------------------------------------------
library(saapsim) #--for bu/ac to kg/ha conversion
saf_buac_to_kgha_corn()

#-make predictions so I can make graphs

nrates <- seq(0, 300)

summary(ymod3a)
intervals(ymod3a)

#--at level of eu
pred_dat <- 
  yldsG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) %>% 
  mutate(preds = predict(ymod3a, newdata = ., level = 2),
         preds = saf_buac_to_kgha_corn(preds)/1000)

pred_dat %>% write_csv("02_fit-curves/fc_yield-preds-eu.csv")


#--at level of site?
pred_dat1 <- 
  yldsG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) %>% 
  mutate(preds = predict(ymod3a, newdata = ., level = 1),
         preds = saf_buac_to_kgha_corn(preds)/1000)

pred_dat %>% write_csv("02_fit-curves/fc_yield-preds-eu.csv")


#--at level of crop rotation
pred_dat2 <- 
  yldsG %>% 
  select(yearF, rotation, site_id, eu) %>% 
  expand_grid(., nrates) %>% 
  rename(nrate_kgha = nrates) %>% 
  mutate(preds = predict(ymod3a, newdata = ., level = 0),
         preds = saf_buac_to_kgha_corn(preds)/1000) %>% 
  select(rotation, nrate_kgha, preds) %>% 
  distinct() %>% 
  mutate(rot = as.character(rotation),
         rot2 = dplyr::recode(rot,
                              "cc" = "Continuous Maize",                        
                              "cs" = "Rotated Maize"))

pred_dat2 %>% write_csv("02_fit-curves/fc_yield-preds-rot.csv")



