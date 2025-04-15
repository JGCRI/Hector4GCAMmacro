# Description: Fit Hector parameter value to emulate selected CMIP6 ESM models. 
# This script can take a while to run, it calibrates each model individually 
# and saves the best fits out to the output/model_fits.csv

# 0. Set Up --------------------------------------------------------------------
# Make sure that the correct version of Hector is installed 
source("scripts/A.constants.R")
source("scripts/A.fxns_calibration.R")


# 1. MIROC6 --------------------------------------------------------------------
# Load the data 
all_data  <- read.csv("data/comp_data/MIROC6.csv")

# Separate out the data for the two calibration steps
ideal_scns <- c("abrupt4xCO2","1pctCO2")
ideal_data <- filter(all_data, scenario %in% ideal_scns)

esm_scns <- c("esm-ssp119", "esm-ssp585",  "esm-historical")
esm_compdata <- filter(all_data, scenario %in% esm_scns) 

scns <- c("historical", "ssp119", "ssp585")
all_data %>% 
  filter(variable == GLOBAL_TAS()) %>% 
  filter(scenario %in% scns) %>% 
  mutate(scenario = paste0("esm-", scenario)) -> 
  comp_data


# Thermodynamics related parameter values to idealized data. 
fit1 <- my_modfit(x = c("S" = 3, "diff" = 2.5), 
                  comp_data = ideal_data, 
                  lower = c(0.5, 0), 
                  upper = c(10, 100))

# Tune the carbon cycle parameters. 
fit2 <- my_modfit(x = c("beta" = 0.53, "q10_rh" = 2.1), 
                  comp_data = esm_compdata, 
                  lower = c(0, 0),
                  upper = c(2, 4), 
                  fixed_params = fit1$par)

# Tune aerosol scalar. 
fit3 <- my_modfit(x = c("alpha" = 0.5), 
                  comp_data = comp_data, 
                  lower = c(0),
                  upper = c(2), 
                  fixed_params = fit2$par)


# Format and save results 
best_fits <- as.data.frame(t(c(model = "MIROC6", fit3$par)))
write.csv(best_fits, 
          file = file.path("output", "model_fits.csv"), 
          row.names = FALSE)


# 2. CanESM5 --------------------------------------------------------------------
# Load the data 
all_data  <- read.csv("data/comp_data/CanESM5.csv")

# Separate out the data for the two calibration steps
ideal_scns <- c("abrupt4xCO2","1pctCO2")
ideal_data <- filter(all_data, scenario %in% ideal_scns)

esm_scns <- c("esm-ssp119", "esm-ssp585",  "esm-historical")
esm_compdata <- filter(all_data, scenario %in% esm_scns) 

scns <- c("historical", "ssp119", "ssp585")
all_data %>% 
  filter(variable == GLOBAL_TAS()) %>% 
  filter(scenario %in% scns) %>% 
  mutate(scenario = paste0("esm-", scenario)) -> 
  comp_data

# Thermodynamics related parameter values to idealized data. 
fit1 <- my_modfit(x = c("S" = 3, "diff" = 2.5), 
                  comp_data = ideal_data, 
                  lower = c(0.5, 0.1), 
                  upper = c(10, 10))

# Tune the carbon cycle parameters. 
fit2 <- my_modfit(x = c("beta" = 0.53, "q10_rh" = 2.1), 
                  comp_data = esm_compdata, 
                  lower = c(0.1, 0.1),
                  upper = c(2, 4), 
                  fixed_params = fit1$par)

# Tune aerosol scalar. 
fit3 <- my_modfit(x = c("alpha" = 0.5), 
          comp_data = comp_data, 
          lower = c(0),
          upper = c(2), 
          fixed_params = fit2$par)

# Format and save results 
best_fits <- as.data.frame(t(c(model = "CanESM5", fit3$par)))
write.table(best_fits, 
            file.path("output", "model_fits.csv"), 
            sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)



# 3. MRI-ESM2-0 --------------------------------------------------------------------
# Load the data 
all_data  <- read.csv("data/comp_data/MRI-ESM2-0.csv")

# Separate out the data for the two calibration steps
ideal_scns <- c("abrupt4xCO2","1pctCO2")
ideal_data <- filter(all_data, scenario %in% ideal_scns)

esm_scns <- c("esm-ssp119", "esm-ssp585",  "esm-historical")
esm_compdata <- filter(all_data, scenario %in% esm_scns) 

scns <- c("historical", "ssp119", "ssp585")
all_data %>% 
  filter(variable == GLOBAL_TAS()) %>% 
  filter(scenario %in% scns) %>% 
  mutate(scenario = paste0("esm-", scenario)) -> 
  comp_data

# Thermodynamics related parameter values to idealized data. 
fit1 <- my_modfit(x = c("S" = 3, "diff" = 2.5), 
                  comp_data = ideal_data, 
                  lower = c(0.5, 0.1), 
                  upper = c(10, 100))

# Tune the carbon cycle parameters. 
fit2 <- my_modfit(x = c("beta" = 0.1, "q10_rh" = 2.1), 
                  comp_data = esm_compdata, 
                  lower = c(0, 0),
                  upper = c(2, 4), 
                  fixed_params = fit1$par)


# Tune aerosol scalar. 
fit3 <- my_modfit(x = c("alpha" = 0.5), 
                  comp_data = comp_data, 
                  lower = c(0),
                  upper = c(2), 
                  fixed_params = fit2$par)


# Format and save results 
best_fits <- as.data.frame(t(c(model = "MRI-ESM2-0", fit3$par)))
write.table(best_fits, 
            file.path("output", "model_fits.csv"), 
            sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

