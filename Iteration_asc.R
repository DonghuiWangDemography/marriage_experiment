library(purrr)
library(dplyr)

# Flexible ASC calibration function
calibrate_asc <- function(model, 
                          data, 
                          target_opt_out = 0.1, 
                          tolerance = 0.001, 
                          max_iterations = 50,
                          asc_coef_name = "asc",
                          opt_out_alt_id = 3,
                          weights_col = "weights",
                          id_col = "id",
                          oid_col = "oid") {
  
  # Set up iterative calibration
  current_coef <- coef(model)
  current_asc <- current_coef[asc_coef_name]
  iteration <- 1
  converged <- FALSE
  
  # Store results for monitoring
  convergence_history <- data.frame(
    iteration = integer(),
    opt_out_share = numeric(),
    asc_value = numeric()
  )
  
  cat("Starting ASC calibration...\n")
  cat("Model:", deparse(substitute(model)), "\n")
  cat("Target opt-out share:", target_opt_out, "\n")
  cat("Tolerance:", tolerance, "\n")
  cat("ASC coefficient name:", asc_coef_name, "\n")
  cat("Opt-out alternative ID:", opt_out_alt_id, "\n\n")
  
  while (!converged && iteration <= max_iterations) {
    # Predict with current coefficients
    calibrated_preds <- predict_mlogit_custom(model, current_coef, data)
    
    # Convert predictions to long format
    fitted_probs_cali <- data.frame(
      id = rep(rownames(calibrated_preds), each = ncol(calibrated_preds)),
      oid = rep(1:ncol(calibrated_preds), nrow(calibrated_preds)),
      prob = as.vector(t(calibrated_preds))
    )
    
    # Merge with original data
    calibrated_data <- merge(
      data,
      fitted_probs_cali,
      by = c(id_col, oid_col)
    ) %>% tibble()
    
    # Calculate current shares
    share_cali <- calibrated_data %>%
      group_by(!!sym(oid_col)) %>%
      summarise(share = sum(prob * !!sym(weights_col)) / sum(!!sym(weights_col)))
    
    current_opt_out_share <- share_cali$share[opt_out_alt_id]
    
    # Store iteration results
    convergence_history <- rbind(convergence_history, data.frame(
      iteration = iteration,
      opt_out_share = current_opt_out_share,
      asc_value = current_coef[asc_coef_name]
    ))
    
    cat(sprintf("Iteration %d: Opt-out share = %.4f, ASC = %.4f\n", 
                iteration, current_opt_out_share, current_coef[asc_coef_name]))
    
    # Check convergence
    if (abs(current_opt_out_share - target_opt_out) < tolerance) {
      converged <- TRUE
      cat(sprintf("\n✓ Converged after %d iterations! Final opt-out share: %.4f\n", 
                  iteration, current_opt_out_share))
      break
    }
    
    # Adjust ASC using the formula: ASC_new = ASC_old + log(S_real) - log(S_predicted)
    asc_adjustment <- log(target_opt_out) - log(current_opt_out_share)
    
    # Update coefficients
    current_coef[asc_coef_name] <- current_coef[asc_coef_name] + asc_adjustment
    
    iteration <- iteration + 1
  }
  
  if (!converged && iteration > max_iterations) {
    warning(sprintf("Maximum iterations (%d) reached. Current opt-out share: %.4f", 
                    max_iterations, current_opt_out_share))
  }
  
  # Return results
  results <- list(
    calibrated_coef = current_coef,
    final_opt_out_share = current_opt_out_share,
    convergence_history = convergence_history,
    target_opt_out = target_opt_out,
    converged = converged,
    iterations = iteration - 1
  )
  
  return(results)
}

# Enhanced prediction function for final market shares
get_calibrated_shares <- function(model, calibrated_coef, data, 
                                  weights_col = "weights",
                                  id_col = "id", 
                                  oid_col = "oid") {
  
  # Predict with calibrated coefficients
  calibrated_preds <- predict_mlogit_custom(model, calibrated_coef, data)
  
  # Convert to long format
  fitted_probs_final <- data.frame(
    id = rep(rownames(calibrated_preds), each = ncol(calibrated_preds)),
    oid = rep(1:ncol(calibrated_preds), nrow(calibrated_preds)),
    prob = as.vector(t(calibrated_preds))
  )
  
  # Merge with original data
  final_data <- merge(
    data,
    fitted_probs_final,
    by = c(id_col, oid_col)
  ) %>% tibble()
  
  # Calculate final shares
  final_shares <- final_data %>%
    group_by(!!sym(oid_col)) %>%
    summarise(share = sum(prob * !!sym(weights_col)) / sum(!!sym(weights_col)))
  
  return(final_shares)
}

# NEW FUNCTION: Get final data with calibrated probabilities
get_calibrated_data <- function(model, calibrated_coef, data, 
                                id_col = "id", 
                                oid_col = "oid",
                                prob_col_name = "prob_calibrated") {
  
  # Predict with calibrated coefficients
  calibrated_preds <- predict_mlogit_custom(model, calibrated_coef, data)
  
  # Convert to long format
  fitted_probs_final <- data.frame(
    id = rep(rownames(calibrated_preds), each = ncol(calibrated_preds)),
    oid = rep(1:ncol(calibrated_preds), nrow(calibrated_preds)),
    prob_calibrated = as.vector(t(calibrated_preds))  # Use the specified column name
  )
  
  # Rename the probability column if different name is requested
  if (prob_col_name != "prob_calibrated") {
    names(fitted_probs_final)[names(fitted_probs_final) == "prob_calibrated"] <- prob_col_name
  }
  
  # Merge with original data
  final_data <- merge(
    data,
    fitted_probs_final,
    by = c(id_col, oid_col)
  ) %>% tibble()
  
  return(final_data)
}

# # Plot convergence history
# plot_convergence <- function(calibration_results) {
#   hist <- calibration_results$convergence_history
#   target <- calibration_results$target_opt_out
#   
#   plot(hist$iteration, hist$opt_out_share, 
#        type = "b", col = "blue", lwd = 2,
#        xlab = "Iteration", ylab = "Opt-out Share",
#        main = "ASC Calibration Convergence",
#        ylim = range(c(hist$opt_out_share, target)))
#   abline(h = target, col = "red", lty = 2, lwd = 2)
#   legend("topright", legend = c("Current", "Target"), 
#          col = c("blue", "red"), lty = c(1, 2), lwd = 2)
# }

# Updated wrapper function for prediction
predict_mlogit_custom <- function(model, new_coef, newdata) {
  model_copy <- model
  
  # Replace coefficients 
  model_copy$coefficients <- new_coef
  
  # Update the log-likelihood function with the new coefficients
  model_copy$logLik <- function(par, X, y, weights, id, ...) {
    sum(weights * log(predict_mlogit_custom_internal(model_copy, par, X, y, id)))
  }
  
  # Use the standard predict method
  predict(model_copy, newdata = newdata)
}

# Print calibration summary
print_calibration_summary <- function(calibration_results, model_name = "Model") {
  cat("\n=== CALIBRATION SUMMARY (", model_name, ") ===\n", sep = "")
  cat("Target opt-out share:", calibration_results$target_opt_out, "\n")
  cat("Final opt-out share:", round(calibration_results$final_opt_out_share, 4), "\n")
  cat("Converged:", calibration_results$converged, "\n")
  cat("Iterations:", calibration_results$iterations, "\n")
  cat("Final ASC value:", round(calibration_results$calibrated_coef["asc"], 4), "\n")
}

#=============================
#scenario 1: never married college graduates by 49 (2020 census): female 1.99 , male 1.73 
#scenario 2: firmly remain single in our survey female 5.44   male : 3.04  / or No do not want to marry : 5.74 for men, 8.02 for female
#scenario 3: Japan 12.3 for females and 16.8 for males 


scenarios <- list(
  results_list_female_seanrio1 = 0.019,
  results_list_female_seanrio2 = 0.0802,
  results_list_female_seanrio3 = 0.123
)

# Function to run calibration and analysis for one scenario
run_scenario <- function(target_opt_out) {
  # Calibrate
  calibration_female <- calibrate_asc(
    model = m1_clogit_female,
    data = marriage_mixlogit_female,
    target_opt_out = target_opt_out
  )
  
  # Get final data with calibrated probabilities
  final_data_female <- get_calibrated_data(
    model = m1_clogit_female,
    calibrated_coef = calibration_female$calibrated_coef,
    data = marriage_mixlogit_female
  )
  
  # Calculate total weights
  total_weights <- final_data_female %>%
    summarise(total = sum(weights)) %>%
    pull(total)
  
  # Calculate unconditional share function
  uncon_shares <- function(var){
    final_data_female %>%
      group_by(across(all_of(var))) %>%
      summarise(nchoose = sum(prob_calibrated * weights),
                ntotal = total_weights / 3) %>%
      ungroup() %>%
      mutate(share = nchoose / ntotal)
  }
  
  # Calculate shares for all group variables
  group_vars <- c("edu", "iage", "beautyc", "housing", "danwei", "onlychild", "isalary")
  results <- map(group_vars, uncon_shares)
  names(results) <- group_vars
  
  return(results)
}

# Run all scenarios
scenario_results <- lapply(scenarios, run_scenario)

# Extract individual results
results_list_female_seanrio1 <- scenario_results$results_list_female_seanrio1
results_list_female_seanrio2 <- scenario_results$results_list_female_seanrio2
results_list_female_seanrio3 <- scenario_results$results_list_female_seanrio3

results_list_female_original # from the original data 


#store the results 
combine_scenarios <- function(var_name) {
  # Extract data for each scenario
  original <- results_list_female_original[[var_name]] %>%
    select(all_of(var_name), share) %>%  # Cleaner syntax
    rename(original = share)
  
  scenario1 <- results_list_female_seanrio1[[var_name]] %>%
    select(all_of(var_name), share) %>%
    rename(senario1 = share)
  
  scenario2 <- results_list_female_seanrio2[[var_name]] %>%
    select(all_of(var_name), share) %>%
    rename(senario2 = share)
  
  scenario3 <- results_list_female_seanrio3[[var_name]] %>%
    select(all_of(var_name), share) %>%
    rename(senario3 = share)
  
  # Combine all
  original %>%
    left_join(scenario1, by = var_name) %>%
    left_join(scenario2, by = var_name) %>%
    left_join(scenario3, by = var_name)
}

sim_results_female <- map(group_vars, combine_scenarios) %>%
  set_names(group_vars)

saveRDS(sim_results_female, file = "sim_results_female.rds")

#=========================================================================
#do the same thing for male 

#scenario 1: never married college graduates by 49: female 1.99 , male 1.73 
#scenario 2: Japan 12.3 for females and 16.8 for males 
#scenario 3: firmly remain single in our survey female 5.44  male : 3.04


scenarios <- list(
  results_list_male_seanrio1 = 0.0173,
  results_list_male_seanrio2 = 0.0574,
  results_list_male_seanrio3 = 0.168
)

# Function to run calibration and analysis for one scenario
run_scenario <- function(target_opt_out) {
  # Calibrate
  calibration_male <- calibrate_asc(
    model = m1_clogit_male,
    data = marriage_mixlogit_male,
    target_opt_out = target_opt_out
  )
  
  # Get final data with calibrated probabilities
  final_data_male <- get_calibrated_data(
    model = m1_clogit_male,
    calibrated_coef = calibration_male$calibrated_coef,
    data = marriage_mixlogit_male
  )
  
  # Calculate total weights
  total_weights <- final_data_male %>%
    summarise(total = sum(weights)) %>%
    pull(total)
  
  # Calculate unconditional share function
  uncon_shares <- function(var){
    final_data_male %>%
      group_by(across(all_of(var))) %>%
      summarise(nchoose = sum(prob_calibrated * weights),
                ntotal = total_weights / 3) %>%
      ungroup() %>%
      mutate(share = nchoose / ntotal)
  }
  
  # Calculate shares for all group variables
  group_vars <- c("edu", "iage", "beautyc", "housing", "danwei", "onlychild", "isalary")
  results <- map(group_vars, uncon_shares)
  names(results) <- group_vars
  
  return(results)
}

# Run all scenarios
scenario_results <- lapply(scenarios, run_scenario)

# Extract individual results
results_list_male_seanrio1 <- scenario_results$results_list_male_seanrio1
results_list_male_seanrio2 <- scenario_results$results_list_male_seanrio2
results_list_male_seanrio3 <- scenario_results$results_list_male_seanrio3

results_list_male_original # from the original data 


#store the results 
combine_scenarios <- function(var_name) {
  # Extract data for each scenario
  original <- results_list_male_original[[var_name]] %>%
    select(all_of(var_name), share) %>%  # Cleaner syntax
    rename(original = share)
  
  scenario1 <- results_list_male_seanrio1[[var_name]] %>%
    select(all_of(var_name), share) %>%
    rename(senario1 = share)
  
  scenario2 <- results_list_male_seanrio2[[var_name]] %>%
    select(all_of(var_name), share) %>%
    rename(senario2 = share)
  
  scenario3 <- results_list_male_seanrio3[[var_name]] %>%
    select(all_of(var_name), share) %>%
    rename(senario3 = share)
  
  # Combine all
  original %>%
    left_join(scenario1, by = var_name) %>%
    left_join(scenario2, by = var_name) %>%
    left_join(scenario3, by = var_name)
}

sim_results_male <- map(group_vars, combine_scenarios) %>%
  set_names(group_vars)

saveRDS(sim_results_male, file = "sim_results_male.rds")

#proceed with visualization 

