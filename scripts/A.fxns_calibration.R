# Description: Commonly used functions for this project. 

# Helper function that sets up a hector core for a scenario with some parameter values.
# Args 
#   scenario: str name
#   fixed_params: a vector of the hector parameters requires string names and numeric values
# Returns: active hc with new parameter values
my_newhc <- function(scenario, fixed_params = NULL){
  
  # Check scenarios
  is_esm <- grepl(pattern = "esm", x = scenario)
  is_ssp <- grepl(pattern = paste0(c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434", 
                                "ssp460", "ssp534-over", "ssp585", "historical"), collapse = "|"), x = scenario)
  is_idealized <- any(scenario %in% c("1pctCO2", "abrupt4xCO2"))
  
  stopifnot(any(c(is_ssp, is_idealized)))
  
  if(is_ssp){
    ini <- list.files(here::here("inputs"), pattern = paste0("_", scenario), full.names = TRUE)
  }
  
  
  if(is_idealized){
    ini <- list.files(here::here("inputs"), pattern = scenario, full.names = TRUE)
  }
  

  stopifnot(file.exists(ini))
  
  hc <- newcore(ini, name = scenario)
  hc <- my_setvars(hc = hc, params = fixed_params)
  
  return(hc)
  
}




# Helper function that sets hector parameter values 
# Args 
#   hc: active hector core 
#   params: a vector of the hector parameters requires string names and numeric values
# Returns: active hc with new parameter values
my_setvars <- function(hc, params){
  
  # TODO comment out the checks when not under development 
  stopifnot(isactive(hc))
  stopifnot(names(params) %in% inputstable$parameter[inputstable$time.variant == "n"])
  
  # Set all the parameter values in the Hector core... 
  for(i  in seq_along(params)){
    var <- names(params)[i]
    val <- params[i]
    setvar(core = hc, dates = NA, values = val, var = var, unit = getunits(var)) 
    reset(core = hc)
  }
  
  return(hc)
  
}



# Helper function that sets up the objective function which will return the 
# error between hector results and ESM comparison data
# Args 
#   comp_data: data frame of the ESM output data 
#   fixed_params: named vector of any potential default parameters 
# Returns: function that will compute the error indicating the fix between hector and the comparison data 
make_objective_fxn <- function(comp_data, fixed_params = NULL){
  
  # Extract the information about the comparison data that will be used 
  # to set up the Hector core in the objective function. 
  scns <- unique(comp_data$scenario)
  yrs  <- unique(comp_data$year)
  vars <- unique(comp_data$variable)
  
  hc_list <- sapply(FUN = my_newhc, X = scns, 
                    fixed_params = fixed_params, 
                    USE.NAMES = TRUE)
  
  hc_historical <- my_newhc(scenario = "historical", 
                            fixed_params = fixed_params)
  
  
  
  objective_fxn <- function(p){
    
    # Run hector with the proposal parameters, this custom 
    # run function is useful because it makes sure that the
    # results are normalized properly. 
    hector_rslts <- custom_run_hector(p = p, hc_list = hc_list, 
                                      hc_historical = hc_historical, 
                                      vars = vars, yrs = yrs)
    
    
    # Get the mean squared error, although we might want to adjust this!
    compute_error(hector_rslts, comp_data) %>% 
      pull(value) %>% 
      mean -> 
      error
    
    return(error)
    
  }
  
  return(objective_fxn)
  
}



# Helper function that calculates the difference between two data frames 
# as the normalized mean squared error. 
# Args 
#   data_1/data_2: data frames to be compared against one another, must include the following columns: scenario, variable, year, value
# Returns: data.frame of the error value per variable
compute_error <- function(data_1, data_2){
  
  data_1 %>% 
    rename(value_x = value) %>% 
    inner_join(data_2, by = join_by(scenario, variable, year)) %>% 
    mutate(SE = (value_x - value)^2) -> 
    SE_df
  
  SE_df %>% 
    summarise(ref_value = sd(SE), .by = c("variable", "scenario")) ->
    ref_value
  
  
  SE_df %>% 
    left_join(ref_value, by = join_by(variable, scenario)) %>% 
    mutate(NSE = SE / ref_value) %>% 
    summarise(MSE = mean(SE), value = mean(NSE), .by = c("variable", "scenario")) %>% 
    mutate(value = if_else(variable == GLOBAL_TAS(), value * 2, value)) -> 
    out 
  
  return(out)
  
  
}


# Fit selected Hector parameter values to minimize error between model 
# output and comparison data. 
# Args 
#   x: vector of an inital guess for the free parameter that will be optimized
#   comp_data: data.frame of ESM data that hector is being constrained to 
#   fixed_params: vector of any parameters that need to be adjusted (relative to the default values)
#   method: str name of the method of the optimization see help(optim) for options, default set to "L-BFGS-B"
#   lower/upper: vector Bounds on the variables for the "L-BFGS-B" method, or bounds in which to search for method "Brent".
# Returns: list containing general-purpose optimization results, if the optimization
# routine converged to a set of best fit parameters, it will include Hector results, 
# and the error value associated with each variable 
my_modfit <- function(x, comp_data, fixed_params = NULL, 
                      method = "L-BFGS-B", lower = -Inf, upper = Inf){
  
  
  # Make the function function that will be minimized by the 
  # optimization algorithm. 
  obj_fxn <- make_objective_fxn(comp_data, fixed_params)
  
  # Find the best fits for the parameters
  fit <- optim(par = x, fn = obj_fxn, lower = lower, upper = upper, method = method)
  
  # After optim is complete update the fit params to include any 
  # default parameters that may have been passed in with the fixed_params 
  # argument. 
  fit$par <- c(fit$par, fixed_params)
  
  # Check to see optimization routine converged, if not return the 
  # fit with a warning message otherwise run Hector with the parameter 
  # values and save output and some additional diagnostic information. 
  if(fit$convergence != 0){
    
    message("Failed to Converge")
    return(fit)
    
  } 
  
  
  
  # Run hector with the best parameter fits, this custom 
  # run function is useful because it makes sure that the
  # results are normalized properly. 
  scns <- unique(comp_data$scenario)
  vars <- unique(comp_data$variable)
  yrs <- unique(comp_data$year)
  
  hc_list <- sapply(FUN = my_newhc, 
                    X = scns, 
                    fixed_params = NULL, 
                    USE.NAMES = TRUE)
  
  hc_historical <- my_newhc(scenario = "historical", 
                            fixed_params = NULL)
  
  hector_rslts <- custom_run_hector(p = fit$par,
                                    hc_list = hc_list, 
                                    hc_historical = hc_historical, 
                                    vars = vars,
                                    yrs = yrs)
  
  # Subset the results to only include comparison data 
  comp_data %>% 
    select(scenario, variable, year) %>% 
    distinct -> 
    comp_data_stats
  
  hector_rslts %>% 
    right_join(comp_data_stats, by = join_by(scenario, year, variable)) -> 
    hector_rslts
  
  
  # Save a copy of the error by variable 
  error_table <- compute_error(hector_rslts, comp_data)
  
  # Append this information to fit 
  
  fit$comp_data <- comp_data
  fit$error <- error_table
  fit$hector <- hector_rslts
  
  return(fit)
  
}


# Run hector with a set of parameter values and normalize the results 
# Args 
#   p: vector of parameter values  
#   hc_list: list of active hector core 
#   hc_historical: active hector core set up to run the concentration driven historical 
#   vars: vector of Hector variables to extract 
#   yrs: vector of the years of data to save
# Return: data.frame of hector results that are normalized to the 1850 - 1860 reference period
custom_run_hector <- function(p, hc_list, hc_historical, vars, yrs){
  
  # Update the historical parameter core with the 
  # proposal parameter values. 
  hc_historical <- my_setvars(hc_historical, p)
  run(hc_historical, runtodate = 1865) 
  
  # Get the reference value to use to get the temperature and heat flux anomaly. 
  fetchvars(core = hc_historical, dates = 1850:1860, vars = c(GLOBAL_TAS(), HEAT_FLUX())) %>% 
    summarise(ref_value = mean(value), .by = "variable") %>% 
    # Although CO2 concentrations do not need to be normalized into 
    # an anomaly add a reference value of 0 here so that NAs are not 
    # introduced into the Hector results. 
    rbind(data.frame("variable" = CONCENTRATIONS_CO2(), "ref_value" = 0)) -> 
    refence_hist
  
  data.frame(variable = c(GLOBAL_TAS(), HEAT_FLUX(), CONCENTRATIONS_CO2()), 
             ref_value = 0) -> 
    refence_zero
  
  
  
  # Run through all of the scenario hector cores
  lapply(hc_list, function(hc){
    
    my_setvars(hc, params = p)
    run(hc)
    
    if(hc$name %in% c( "1pctCO2", "abrupt4xCO2")){
      
      refence_df <- refence_zero
      
    }else{
    
      refence_df <- refence_hist
    }
    
    
    
    
    
    fetchvars(hc, yrs, vars) %>% 
      left_join(refence_df, by = join_by(variable)) %>% 
      mutate(value = value - ref_value) %>%  
      select(scenario, year, variable, value, units)
    
  }) %>% 
    bind_rows ->
    normalized_hector_output
  
  return(normalized_hector_output)
  
}
