library(readr)
library(dplyr)
library(here)

## Root mass data
rmd <- read_excel(here("data", "raw", "fine_root_mass.xlsx")) %>%
  mutate(frdmc = dry / wet) %>%
  mutate_at(vars(code), funs(as.character(.)))

## SmartRoot data
files <- list.files(here("data", "raw", "fine-root-length/"))
codes <- substr(files, 8, 10)

# loop files and concatenate
rd1 <- setNames(lapply(files, function(x) {
  rds <- read.csv(here("data", "raw", "fine-root-length", x))
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
write_csv(rd.raw, path = here("data", "processed", "fine_root_shapes_2017-18.csv"))