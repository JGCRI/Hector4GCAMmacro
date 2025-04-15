# Description: Prepare the ESM output data for Hector calibration so that Hector may act as an
# emulator for said ESM. We need the ESM comparison data to be consistent regarding 
# units, scenario names, and variable names as Hector. 
# Just because the data is included here does not mean that the data will
# be used in calibration protocol.
# 
# This script only needs to be run once, the results will be included
# in the repository.
# 
# 0. Set Up --------------------------------------------------------------------
source("scripts/A.constants.R")

# Models/experiments of interest 
model_names    <- c("MRI-ESM2-0", "CanESM5", "MIROC6")
idealized_exps <- c("1pctCO2", "abrupt-4xCO2")
scns_exps      <- c("historical", "ssp119", "ssp126", "ssp245", "ssp370", "ssp585", 
                    "ssp434", "ssp460", "ssp534-over")
exp_names     <- c(idealized_exps, scns_exps)



# Import the CMIP6 data and throw an error if the raw data has not been 
# downloaded yet. 
CMIP6_DIR <- here::here("data", "JGCRI-hector_cmip6data-182ee34", "outputs") 
stopifnot(dir.exists(CMIP6_DIR))


# Global mean air temperature data 
file.path(CMIP6_DIR, "cmip6_annual_tas_global.csv") %>% 
  read.csv %>%
  filter(model %in% model_names) %>% 
  filter(experiment %in% exp_names) -> 
  raw_temp_data

# Ocean heat content 
file.path(CMIP6_DIR, "cmip6_annual_ocean_heat_flux.csv") %>% 
  read.csv %>%
  filter(model %in% model_names) %>% 
  filter(experiment %in% exp_names) %>%  
  select(model, experiment, year, value = equation, units, ensemble) %>% 
  filter(year <= 2100) -> 
  raw_ohf_data

# 1 . Temperature Data ---------------------------------------------------------

# Save the tgav variable for the idealized scenarios 
raw_temp_data %>% 
  filter(experiment %in% idealized_exps) %>% 
  filter(variable == "Tgav") %>% 
  select(model, experiment, ensemble, variable, year, value, units) -> 
  idealized_temp_data 

raw_temp_data %>% 
  filter(experiment %in% scns_exps) %>% 
  filter(year %in% 1850:1860) %>% 
  filter(variable == "tas") %>%
  summarise(value = mean(value), .by = c("model", "ensemble")) %>% 
  select(model, ensemble, ref_value = value) -> 
  scn_ref_values 

raw_temp_data %>% 
  filter(experiment %in% scns_exps) %>%  
  filter(variable == "tas") %>%
  left_join(scn_ref_values, by = join_by(model, ensemble)) %>% 
  mutate(value = value - ref_value) %>% 
  select(model, experiment, ensemble, variable, year, value, units) -> 
  scn_temp_data 

# The temperature data should include both the idealized and scenario results 
rbind(scn_temp_data, idealized_temp_data) %>%
  mutate(variable = GLOBAL_TAS()) %>% 
  rename(scenario = experiment) -> 
  temp_data
  
  
# 1. Normalize Heat Flux -------------------------------------------------------

# We need to normalize the ocean heat flux relative to the 1850-1860 base period. 
raw_ohf_data %>% 
  filter(experiment == "historical") %>% 
  filter(year %in% 1850:1860) %>% 
  summarise(ref_value = mean(value), .by = c("model")) -> 
  heat_flux_ref_values

raw_ohf_data %>%  
  left_join(heat_flux_ref_values, by = join_by(model)) %>% 
  mutate(value = value - ref_value) %>% 
  select(model, scenario = experiment, year, value, units) ->
  scn_heat_flux_data

# The temperature data should include both the idealized and scenario results 
scn_heat_flux_data %>%
  mutate(variable = HEAT_FLUX()) -> 
  heat_flux_data


# 2. Drop incomplete ensembles -------------------------------------------------
# If an variable is missing an ensemble drop it from our data set. 

# Make sure that that there are results for both variables per 
# scenario and ensemble member. 
heat_flux_data %>% 
  bind_rows(temp_data) %>% 
  select(model, scenario, ensemble, variable) %>% 
  distinct %>% 
  mutate(exists = 1) %>% 
  summarise(count = sum(exists), 
            .by = c(model, scenario, ensemble)) %>% 
  filter(count == 2) %>% 
  select(-count) -> 
  model_scenario_ensemble

heat_flux_data %>% 
  bind_rows(temp_data) -> 
  full_heatflux_tas_data

# Make the scenario name consistent. 
full_heatflux_tas_data %>% 
  mutate(scenario = if_else(scenario == "abrupt-4xCO2", "abrupt4xCO2", scenario)) -> 
  full_heatflux_tas_data

# 3. CO2 Concentrations for emission driven runs -------------------------------

# Save a copy of the CO2 concentrations per model/scenario/ensemble that for the 
# emission driven scenario runs. These are to help constrain the carbon cycle 
# parameters which I am not sure if it is necessary or not... but I suspect 
# that they should be. 
here::here("inputs", "tables") %>% 
  list.files(pattern = "ssp", full.names = TRUE) %>% 
  lapply(function(f){
    
    d <- read.csv(f, comment.char = ";")
    scenario <- gsub(x = basename(f), pattern = "_emiss-constraints_rf.csv", replacement = "")
    name <- paste0("esm-", scenario)
    
    data.frame(year = d$Date, 
               value = d$CO2_constrain, 
               scenario = name, 
               variable = CONCENTRATIONS_CO2())
    
  }) %>% 
  bind_rows() -> 
  raw_co2_data


# Limit the future CO2 data to the ssp years and historical 
temp_data %>% 
  filter(scenario == "ssp245") %>% 
  pull(year) %>% 
  unique -> 
  ssp_years

temp_data %>% 
  filter(scenario == "historical") %>% 
  pull(year) %>% 
  unique -> 
  hist_years

raw_co2_data %>% 
  filter(year %in% ssp_years) -> 
  ssp_co2_data

raw_co2_data %>% 
  # Save only one copy of the CO2 concentrations from the 
  # SSP scenarios.
  filter(scenario == "esm-ssp245") %>%  
  filter(year %in% hist_years) %>% 
  mutate(scenario = "esm-historical") -> 
  hist_co2_data

ssp_co2_data %>% 
  bind_rows(hist_co2_data) %>% 
  mutate(join = 1) -> 
  co2_data


# Repeat the CO2 data such that there is co2 data per model/scenario/ensemble
full_heatflux_tas_data %>% 
  select(model, ensemble) %>% 
  distinct %>% 
  mutate(join = 1) -> 
  missing_co2_data

missing_co2_data %>% 
  full_join(co2_data, relationship = "many-to-many", by = join_by(join)) %>% 
  select(-join) -> 
  complete_co2_data


# X. Save Data --------------------------------------------------------------
# Save the comparison data by model type. 
data <- bind_rows(full_heatflux_tas_data, complete_co2_data)


for(m in model_names){
  
  d <- filter(data, model == m) 
  write.csv(x = d, file = here::here("data", "comp_data", paste0(m, ".csv")), row.names = FALSE)
  
}



