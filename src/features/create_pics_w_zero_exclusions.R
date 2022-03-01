library(readr)
library(dplyr)
library(here)

all_traits <- read_csv(here("data", "interim", "all_traits_pic_prep.csv"))

spp_tree <- read.tree(here("data", "raw", "sev_tree_101spp.tre"))

# get ranked list of zeros
temp_cv <- read_csv(here("data", "interim", "reponse_vars_spp_tidy.csv"))

zranks <- temp_cv %>%
  filter(site == "F", 
         allometry == "BM", 
         season == "F", 
         metric == "cv") %>%
  mutate(zrank = rank(14-n_zero_mean)) %>%
  left_join(eval(.) %>%
              group_by(zrank) %>%
              summarize(nsp=n()) %>%
              mutate(nexclude1 = cumsum(nsp), 
                     nexclude = lag(nexclude1)) %>%
              mutate(across(nexclude, .f = ~ ifelse(is.na(.x), 0, .x))) %>%
              select(zrank, nexclude), 
            by = "zrank") %>%
  arrange(nexclude) %>%
  select(kartez, nexclude) 

# join in rank to all traits data
all_traits2 <- all_traits %>%
  left_join(zranks) %>%
  select(kartez, nexclude, family:last_col()) %>%
  drop_na(nexclude)

ranks <- unique(zranks$nexclude)[order(unique(zranks$nexclude))]
tree <- spp_tree

inranks <- ranks[1:20]
# loop through species rareness
rare_list <- lapply(inranks, function(rank) {
  
  all_pics_list <- setNames(lapply(names(trans), function(t) {
    
    all_trans <- brl_transform(x=all_traits2 %>% filter(nexclude >= rank), 
                               trait=t, phy=spp_tree,
                               taxa = "taxon.new")
    all_trans[[trans_options[trans[t]]]]
    
  }
  
  ), names(trans))
  
  # join all traits together
  
  all_pics_tib <- lapply(names(all_pics_list), function(t) {
    
    all_pics_list[[t]] %>%
      select(node, 
             !!t:= pic)
  }) %>%
    reduce(full_join, by="node")
  
  return(all_pics_tib)
  
}) %>% setNames(inranks)

saveRDS(rare_list, 
     file= here("data", "interim", "rare_list.rds"))
