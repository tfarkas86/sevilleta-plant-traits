library(readr)
library(dplyr)
library(here)

#### Input Data ####
quad_metrics <- read_csv(here("data", "interim", "all_response_metrics_noagg.csv"))

# change me to select metrics!
focal_metrics <- c("cv", "acor")

#### Aggregate ####
vars_tidy <- quad_metrics %>%
  
  # filter for focal bounce metrics
  filter(metric %in% focal_metrics) %>%
  
  # group to aggregate quads
  group_by(season, site, kartez, allometry, metric) %>%
  
  # get mean of bounce and other aggregate stats
  summarize(value = mean(value),
            n_zero_mean = mean(n_zero), 
            biomass_mean = mean(biomass_mean), 
            n_quads = n()) %>%
  
  arrange(desc(metric), allometry) %>%
  ungroup() %>%
  
  # concatenate with all-sites and flats-only aggregations
  bind_rows(., 
            
            # all sites
            quad_metrics %>%
              
              # filter for focal bounce metrics
              filter(metric %in% c("cv", "acor")) %>%
              
              # group to aggregate quads
              group_by(season, kartez, allometry, metric) %>%
              
              # get mean of bounce and other aggregate stats
              summarize(value = mean(value),
                        n_zero_mean = mean(n_zero), 
                        biomass_mean = mean(biomass_mean), 
                        n_quads = n()) %>%
              
              # add site = "A"
              mutate(site = "A") %>%
              
              arrange(desc(metric), allometry) %>%
              ungroup(),
            
            # flats only
            quad_metrics %>%
              
              # filter for focal bounce metrics and remove PJ
              filter(metric %in% c("cv", "acor"),
                     site != "P") %>%
              
              # group to aggregate quads
              group_by(season, kartez, allometry, metric) %>%
              
              # get mean of bounce and other aggregate stats
              summarize(value = mean(value),
                        n_zero_mean = mean(n_zero), 
                        biomass_mean = mean(biomass_mean), 
                        n_quads = n()) %>%
              
              # add site == "F"
              mutate(site = "F") %>%
              
              arrange(desc(metric), allometry) %>%
              ungroup())

write_csv(vars_tidy, here("data", "interim", "reponse_vars_spp_tidy.csv"))


# write out widened file for PICs
vars_wide <- vars_tidy %>% 
  
  # cv, Fall, and BM only
  filter(metric %in% c("cv", 'acor'),
         season == "F", 
         allometry == "BM") %>%
  
  # make temp column for spread
  mutate(key = paste(season, site, allometry, metric, sep="_")) %>%
  
  # keep only spreading variables
  select(kartez, key, value) %>%
  
  # widen for 1 metric per column
  spread(key="key", value="value") %>%
  
  arrange(kartez)

#### Write Out ####
write_csv(vars_wide, here("data", "processed", "reponse_vars_spp_raw.csv"))