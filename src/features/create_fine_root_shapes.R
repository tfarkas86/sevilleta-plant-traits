library(readr)
library(dplyr)

repo_home = "~/Dropbox/1_Work/1_Research/Whitney-Rudgers Lab/Sev/Plants/sevilleta-plant-traits/"

## Root mass data
rmd <- read_excel(paste0(repo_home, "data/raw/fine_root_mass.xlsx")) %>%
  mutate(frdmc = dry / wet) %>%
  mutate_at(vars(code), funs(as.character(.)))

## SmartRoot data
files <- list.files(paste0(repo_home, "data/raw/fine-root-length/"))
codes <- substr(files, 8, 10)

# loop files and concatenate
rd1 <- setNames(lapply(files, function(x) {
  rds <- read.csv(paste0(repo_home,"data/raw/fine-root-length/", x))
  print(rds)
  return(data.frame(length=rds$length, 
                    diam=rds$diameter, 
                    vol=rds$volume))
  rm(rds)
}
), codes)

# a few calculations
rd.raw <- do.call(rbind, lapply(rd1, function(x) {
  
  c(len=sum(x$length), diam=quantile(x$diam, .95), vol=sum(x$vol))
  
})) %>%
  as.data.frame() %>%
  mutate("code" = row.names(.)) 

# write out
write_csv(rd.raw, path = paste0(repo_home, "data/processed/fine_root_shapes_2017-18.csv"))