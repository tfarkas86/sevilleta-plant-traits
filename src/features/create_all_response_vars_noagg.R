library(readr)
library(dplyr)
library(tidyr)

repo_home = "~/Dropbox/1_Work/1_Research/Whitney-Rudgers Lab/Sev/Plants/sevilleta-plant-traits/"

#### Reshape Raw Data ####
# Input raw data
ab.raw <- read_csv(paste0(repo_home, "data/raw/sev_biomass_28Jan2018.csv"), 
                   col_types=cols(transect = col_character(),
                                  year = col_integer(), 
                                  web = col_character(), 
                                  quad = col_character())) # guessed logical

## create core biomass datas et for all sites, seasons, and species. 
## both allometries are included (BM and BIM)
## 0s are filled for all quads where no plants are recorded.
ab <- ab.raw %>%
  filter(substr(site, 1, 4) == "core", treatment == "C") %>% # core sites only
  mutate(year_char =  paste("y", year, sep="")) %>% # make year values character
  
  # change and aggregate species when kartez change, or are difficult to identify
  mutate(across(kartez, 
                ~ ifelse(.x %in% c("ARDI5", "ARPU9"), "ARIST", 
                         ifelse(.x %in% c("SPCO4", "SPFL2", "SPCR"), "SPORO", 
                                ifelse(.x == "OESU3", "GACO5", 
                                       ifelse(.x == "OESUN", "GASUN", 
                                              ifelse(.x == "DYGR", "CHGR2", 
                                                     ifelse(.x %in% c("ASFE2", "ASNU4", "ASTRA"), "ASTRA", .)))))))) %>%
  mutate(across(site, # recode sites to simplify
                ~recode(.x, "core_black" = "G", "core_creosote" = "C", 
                        "core_blue" = "B", "core_PJ" = "P" ))) %>%
  mutate(across(season, # recode seasons to simplify
                ~ recode(.x, "fall" = "F", "spring" = "S"))) %>%
  mutate(across(web, ~ ifelse(site == "P", transect, .x))) %>%
  mutate(quad_id = paste(site, web, plot, quad, sep="_")) %>%
  select(site, year, year_char, season:quad, quad_id, kartez,
         biomass.BM:biomass.BIM,
         -treatment, -block, -subplot, -transect, -date) %>%
  droplevels() %>%  # reset factor levels
  
  # filter out quads with inconsistent sampling
  semi_join(eval(.) %>%
              filter(season == "F") %>%
              group_by(year, site, web, plot, quad) %>%
              summarize(n_records = n()) %>%
              ungroup() %>%
              complete(year, nesting(site, web, plot, quad), fill=list(n_records = 0)) %>%
              filter(n_records > 0) %>%
              group_by(site, web, plot, quad) %>%
              summarize(min_year = min(year), 
                        max_year = max(year)) %>%
              filter(min_year < 2004, 
                     max_year >= 2017), 
            by=c("site", "web", "plot", "quad")) %>%
  filter(between(year, 2003, 2017)) %>% # restrict sampling years
  
  
  # Due to species pooling (ie, SPORO, ASTRA are multiple species), 
  # sum biomass for each kartez for each quad sampling
  group_by(year, year_char, season, site, web, plot, quad, quad_id, kartez) %>%
  summarize(across(biomass.BM, sum),
            across(biomass.BIM, sum)) %>%
  ungroup() %>%
  
  # fill implicit missing values with explicit 0s, 
  # but only for quads with at least one instance of a species
  # to avoid filling 0s for all years -> CV = NaN
  complete(nesting(year, year_char), 
           nesting(kartez, season, site, web, plot, quad, quad_id), 
           fill=list(biomass.BM=0, biomass.BIM= 0))

#### Calculate "Bounciness" Metrics ####
# for each unique quad-season-kartez

source(paste0(repo_home, "src/features/variance_metric_fxns.R"))

quad_metrics <- ab %>%
  
  # regroup by season, site, web, plot, quad, quad_id, kartez
  ungroup() %>% group_by(season, site, web, plot, quad, quad_id, kartez) %>%
  
  # gather two density measures
  rename(BM=biomass.BM, BIM=biomass.BIM) %>%
  pivot_longer(BM:BIM, names_to = "allometry", values_to="biomass") %>%
  
  # calculate bounciness for all season, kartez, quad combination
  # for each allometry independently
  ungroup() %>% 
  group_by(season, site, web, plot, quad, quad_id, kartez, allometry) %>%
  summarize(var = var(biomass), 
            cv = coeffVar(biomass), # coefficient of variation
            slpCv = slopeMetrics(biomass, "slpCv"), 
            q1090 = quantileRange(biomass), 
            npeaks = slopeMetrics(biomass, "npks"), 
            # ppeaks = slopeMetrics(biomass, "ppeaks"),
            slpMean = slopeMetrics(biomass, "slpMean"), 
            corrSlpMean = slopeMetrics(biomass, "corrSlpMean"),
            absSlpDiff = slopeMetrics(biomass, rtn="absSlpDiff"), 
            corrAbsSlpDiff = slopeMetrics(biomass, "corrAbsSlpDiff"), 
            pctSlpDiff = slopeMetrics(biomass, "pctSlpDiff"), 
            corrPctSlpDiff = slopeMetrics(biomass, "corrPctSlpDiff"), 
            angDiffMean = slopeMetrics(biomass, "angDiffMean"), 
            corrAngDiffMean = slopeMetrics(biomass, "corrAngDiffMean"), 
            angDiffPro = slopeMetrics(biomass, "angDiffPro"), 
            corrAngDiffPro = slopeMetrics(biomass, "corrAngDiffPro"), 
            # autocorrelation of 1-year lag
            acor = acf(biomass, plot=FALSE, lag.max=1,
                       type="correlation")$acf[2], 
            acov = acf(biomass, plot=FALSE, lag.max=1,
                       type="covariance")$acf[2],
            #slpAcor = slopeMetrics(biomass, "slpAcor"),
            #corrSlpAcor = slopeMetrics(biomass, "corrSlpAcor"),
            #acv = acv(biomass),
            n_zero = sum(biomass == 0), # number of zero densities 
            biomass_mean = mean(biomass)) %>% # mean
  filter(n_zero < 15) %>%
  ungroup() %>%
  
  # pivot to long format on metrics
  pivot_longer(var:acov, 
               names_to="metric", values_to="value")

#### Write Out ####
# The above is not very fast, and results in much more data than will be 
# necessary. Write out for filtering at a later step. 

write_csv(quad_metrics, paste0(repo_home, "data/interim/all_response_metrics_noagg.csv"))

