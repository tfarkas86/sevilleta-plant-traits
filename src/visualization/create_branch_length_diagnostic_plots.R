library(readr)
library(dplyr)
library(ape)
library(ggplot2)

repo_home = "~/Dropbox/1_Work/1_Research/Whitney-Rudgers Lab/Sev/Plants/sevilleta-plant-traits/"

all_traits <- read_csv(paste0(repo_home, "data/interim/all_traits_pic_prep.csv"))

spp_tree <- read.tree(paste0(repo_home, "data/raw/sev_tree_101spp.tre"))

source(paste0(repo_home, "src/features/pic_fxns.R"))

# get all trait names
trait_names <- all_traits %>%
  select(a_p:F_P_BM_cv) %>%
  names()

# just variance metrics
#trait_names <- all_traits %>%
# select(F_A_BM_acor:F_P_BM_cv) %>%
# names()


# loop through names to create diagnostic transformation plots
for(trait in trait_names) {
  
  print(trait)
  
  # get pics for all transformations
  trait_pics <- brl_transform(x=all_traits, trait=trait, 
                              taxa = "taxon.new",
                              phy=spp_tree)
  
  # bind to single data frame
  trait.pics.df <- do.call(rbind, trait_pics) 
  # filter(!grepl("power", .$trans)) %>% # Pagel only
  
  # get correlations
  trait.cors <- trait.pics.df %>%
    group_by(trans) %>%
    summarize(cor=round(cor(pic_abs, sd), 2))
  
  # create diagnostic plot
  
  trait.pics.df %>%
    ggplot(aes(x=pic_abs, y=sd)) +
    geom_label(aes(label=substr(node, nchar(node) - 2, nchar(node))),
               cex=2.5) +
    facet_wrap(~ trans, scales = "free") + 
    geom_text(data = trait.cors, 
              aes(x=Inf, y=Inf, label=paste("r = ", trait.cors$cor, sep=""),
                  vjust=1, hjust=1),
              cex = 3, col="red")
  
  # write to disk
  ggsave(filename = paste0(repo_home, "reports/figures/branch_length_diagnostics/pic_trans_diagnostics_", trait, ".pdf"),
         device="pdf", height = 14, width=14)
  
}
