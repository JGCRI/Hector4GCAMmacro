# Description: Let's run Hector with the best parameter fits and prepare that output 
# to be used in STITCHES. But it will run Hector as the emulator for the different 
# models and then save the gmat time series to be used in the stitching portion. 
# This script also saves a copy of Hector output a handful of other scenarios so that 
# we can compare against ESM results. 

# TODO 
# This is a place holder for the GCAM portion of the workflow.

# 0. Set Up --------------------------------------------------------------------
# Make sure that the correct version of Hector is installed 
source("scripts/A.constants.R")
source("scripts/A.fxns_calibration.R")


# 1. Hector  -------------------------------------------------------------------
# Let's use the same set up for the Hector, which we can then use in the comparison 
# with the ESMs. 
param_fits <- read.csv(file.path("output/model_fits.csv"))

# The scenarios we would like to run should be emission driven since 
# the GCAM-hector run will be emission driven. 
scns <- c("1pctCO2", "abrupt4xCO2", "esm-historical", "esm-ssp119",
          "esm-ssp126", "esm-ssp245", "esm-ssp370", "esm-ssp434",
          "esm-ssp460", "esm-ssp585", "esm-ssp534-over")


# Make all the scenarios we would like to run 
scn_list <- lapply(X = scns, FUN = my_newhc)
hist_hc  <- my_newhc("historical")

# Empty data frame to store the results in 
rslts <- data.frame()

# Use each of the model fits 
for(i in seq_along(param_fits$model)){
  

  model <- param_fits$model[i]
  p <- as.numeric( param_fits[i,-1])
  names(p) <- names(param_fits[i,-1])
  out <- custom_run_hector(p = p, 
                           hc_list = scn_list, 
                           hc_historical = hist_hc, 
                           vars = c(GLOBAL_TAS(), HEAT_FLUX(), CONCENTRATIONS_CO2()), 
                           yrs = 1850:2100)  
  out$model <- model
  out$source <- "hector"
  
  rslts <- bind_rows(rslts, out)
  
}

# Format the results so that they will be easy to compare with the ESM output. 
rslts %>% 
  mutate(scenario = gsub(pattern = "esm-", x = scenario, replacement = "")) -> 
  hector_rslts

hector_rslts %>% 
  filter(year %in% 1850:2100) %>% 
  filter(variable == GLOBAL_TAS()) %>% 
  filter(scenario == "ssp245") %>% 
  filter(source == "hector") -> 
  gmat_data

# Save the output to be used in the L3 example script. 
models <- unique(gmat_data$model)
for(m in models){
  
  gmat_data %>% 
    filter(model == m) %>% 
    select(scenario, year, value, model) -> 
    x
  
  write.csv(x, file.path("output", paste0(m,".csv")), row.names = FALSE)
  
}


# 2. Hector vs ESM comparison  ------------------------------------------------
# Format the hector results so that it will be easy to make the emulated Hector 
# vs. ESM plots. 
hector_rslts %>% 
  mutate(keep = 1) %>% 
  mutate(keep = if_else(scenario == "1pctCO2" & year >= 2000, 0, keep)) %>% 
  mutate(keep = if_else(scenario == "historical" & year >= 2015, 0, keep)) %>% 
  mutate(keep = if_else(scenario %in% c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434",
                                        "ssp460", "ssp585", "ssp534-over") & 
                        year <= 2015, 0, keep)) %>% 
  filter(keep == 1) %>% 
  select(-keep) -> 
  hector_rslts2

# Import and format the ESM data for comparisons. 
list.files("data/comp_data", pattern = "csv", full.names = TRUE) %>% 
  lapply(FUN = read.csv) %>%
  do.call(what = "rbind") %>% 
  mutate(source = "CMIP6") -> 
  cmip6_rslts

comparison_df <- bind_rows(hector_rslts2, cmip6_rslts)

write.csv(comparison_df, file = file.path("output/comparison_data.csv"), row.names = FALSE)



# Quick let's plot them 
#"MIROC6"     "CanESM5"    "MRI-ESM2-0"
m <- "MRI-ESM2-0" 

comparison_df %>%  
  filter(model == m) %>%
  filter(variable == GLOBAL_TAS()) %>% 
  ggplot(aes(year, value, color = source)) + 
  geom_line() + 
  facet_wrap("scenario", scales = "free") + 
  labs(title = m, 
       y = "Global Air Surface Temp (deg C)", 
       x = NULL) + 
  scale_color_manual(values = c("CMIP6" = "grey", "hector" = "red"))





