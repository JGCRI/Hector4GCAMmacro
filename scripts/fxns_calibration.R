# Objective: Work on setting the calibration functions.... 

library(hector)



# Helper function that sets up a hector core for a scenario with some parameter values.
# Args 
#   scenario: str name
#   fixed_params: a vector of the hector parameters requires string names and numeric values
# Returns: active hc with new parameter values
my_newhc <- function(scenario, fixed_params){
  
  # Check scenarios
  is_esm <- grepl(pattern = "esm", x = scenario)
  is_ssp <- any(scenario %in% c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434", 
                                "ssp460", "ssp534-over", "ssp585"))
  is_idealized <- any(scenario %in% c("1pctCO2", "abruptx4CO2"))
  
  stopifnot(any(c(is_ssp, is_idealized)))
  
  if(is_ssp && is_esm){
    ini <- list.files(here::here("inputs"), pattern = paste0("_", scenario), full.names = TRUE)
    hc <- newcore(ini, name = scenario)
  }
  
  
  if(is_ssp && is_esm){
    ini <- list.files(here::here("inputs"), pattern = paste0("esm-", scenario), full.names = TRUE)
    hc <- newcore(ini, name = scenario)
  }
  
  if(is_idealized){
    ini <- list.files(here::here("inputs"), pattern = scenario, full.names = TRUE)
    hc <- newcore(ini, name = scenario)
  }
  
  # If there are no fixed parameters return the active hector core
  if(is.null(fixed_params)){
    
    return(hc)
    
  }
  
  # Otherwise update the hector core with parameter values 
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
  
  objective_fxn <- function(p){
    
    # Set the proposal parameter to the active Hector cores in preparation for running.
    sapply(X = hc_list, FUN = my_setvars, params = p)
    
    # Run Hector and extract the Hector results.
    lapply(hc_list, function(hc){
      
      run(hc)
      fetchvars(hc, yrs, vars)
      
    }) %>% 
      do.call(what = "rbind") %>% 
      select(scenario, year, variable, value) -> 
      hector_rslts
    
    
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
    summarise(ref_value = sd(SE), .by = c("variable")) ->
    ref_value
  
  
  SE_df %>% 
    left_join(ref_value, by = join_by(variable)) %>% 
    mutate(NSE = SE / ref_value) %>% 
    summarise(MSE = mean(SE), MNSE = mean(NSE), .by = c("variable")) %>% 
    select(variable, value = MNSE)
  
  
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
  
  # Hector output when driven with the best paramter fits. 
  lapply(unique(comp_data$scenario), function(scn){
    
    
    hc <- my_newhc(scn, fixed_params = fit$par)
    
    
    comp_data %>% 
      filter(scenario == scn) -> 
      this_data
    
    run(hc)
    
    out <- fetchvars(hc, dates = this_data$year, vars = unique(this_data$variable))
    return(out)
    
    
  }) %>% 
    do.call(what = "rbind") -> 
    hector_rslts
  
  # Save a copy of the error by variable 
  error_table <- compute_error(hector_rslts, comp_data)
  
  # Append this information to fit 
  
  fit$comp_data <- comp_data
  fit$error <- error_table
  fit$hector <- hector_rslts
  
  return(fit)
  
}




