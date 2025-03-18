# Because I am lazy I pull the pangeo catalog in python and then decided to take 
# look at model coverage in R. 

library(dplyr)
library(ggplot2)
library(tidyr)



# The models that we are interested in 
models <- c("MRI-ESM2-0", "CanESM5", "INM-CM5-0")


here::here("cmip6_processing", "pangeo_catalog.csv") %>% 
  read.csv() %>% 
  filter(source_id %in% models)  -> 
  pangeo_data_for_our_models
  

# Variables requested by Di 
di_vars <- c("tas", "hurs", "rsds")
stitches_exps <- c("ssp370", "ssp119", "ssp460", "ssp434",
                   "ssp245", "ssp585", "ssp534-over", "ssp126", "historical")

pangeo_data_for_our_models %>% 
  filter(variable_id %in% di_vars, 
         experiment_id %in% stitches_exps) %>% 
  select(source_id, experiment_id, member_id, variable_id) %>% 
  distinct() -> 
  models_vars

models_vars %>% 
  mutate(exists = 1) %>% 
  spread(variable_id, exists) %>% 
  na.omit -> 
  complete_stitches_data

complete_stitches_data %>% 
  summarise(n = n_distinct(member_id), .by = c("source_id", "experiment_id"))
 

# Check to see MRI-ESM2-0 space ------ 
model_name <- "MRI-ESM2-0"

complete_stitches_data %>% 
  filter(source_id == model_name) %>% 
  distinct(member_id) -> 
  MRI_members_to_keep

read_csv("data/stitiches-data/matching_archive.csv") %>% 
  filter(model == model_name) %>% 
  filter(ensemble %in% c(MRI_members_to_keep$member_id)) %>% 
  filter(experiment %in% stitches_exps) -> 
  MRI_space

MRI_space %>% 
  ggplot(aes(fx, dx)) + 
  geom_point() + 
  labs(title = model_name)


# Check to see CanESM5 space ---- 
model_name <- "CanESM5"
complete_stitches_data %>% 
  filter(source_id == model_name) %>% 
  pull(member_id) %>% 
  unique() -> 
  Can_members_to_keep

read_csv("data/stitiches-data/matching_archive.csv") %>% 
  filter(model == model_name) %>% 
  filter(ensemble %in% c(Can_members_to_keep)) %>% 
  filter(experiment %in% stitches_exps) -> 
  Can_space

Can_space %>% 
  ggplot(aes(fx, dx)) + 
  geom_point() + 
  labs(title = model_name)


