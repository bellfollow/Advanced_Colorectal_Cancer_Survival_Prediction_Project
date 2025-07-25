# 06. Machine Learning Modeling for Survival Prediction

# 1. Setup
# ==============================================================================
# Load required packages
library(tidyverse)
library(dplyr)
library(caret)
library(pROC)
library(randomForest)
library(gbm)
library(xgboost)
library(themis) # For SMOTE

# 2. Feature Selection
# ==============================================================================
# Function to extract significant variables from Firth regression results
get_significant_vars <- function(file_path) {
    results <- read.csv(file_path)
    significant_vars <- results %>% 
                filter(.data$p_value < 0.05 & (.data$upper_ci < 1 | .data$lower_ci > 1)) %>% 
        pull(.data$variable)
    return(as.character(significant_vars))
}

# Define paths to results
results_path <- "results/multivariate_analysis/"

# Extract variables for each group
features_total <- get_significant_vars(paste0(results_path, "total_multivariate_p_lt_005_firth_results.csv"))
features_eocrc <- get_significant_vars(paste0(results_path, "eocrc_multivariate_p_lt_005_firth_results.csv"))
features_locrc <- get_significant_vars(paste0(results_path, "locrc_multivariate_p_lt_005_firth_results.csv"))

# Define target variable
target <- "사망여부"

print("Selected features for TOTAL group:")
print(features_total)
print("Selected features for EOCRC group:")
print(features_eocrc)
print("Selected features for LOCRC group:")
print(features_locrc)


# 3. ML Modeling Function
# ==============================================================================
train_and_evaluate_models <- function(group_name, features, target) {
    message(paste("\n--- Starting Modeling for", group_name, "Group ---"))

    # Load pre-split data
    train_path <- file.path("data", "modeling_datasets", group_name, paste0(group_name, "_train.csv"))
    valid_path <- file.path("data", "modeling_datasets", group_name, paste0(group_name, "_valid.csv"))
    
    if (!file.exists(train_path) || !file.exists(valid_path)) {
        stop(paste("Training/validation data not found for group:", group_name))
    }
    
    train_data_raw <- read.csv(train_path, na.strings = "")
    valid_data_raw <- read.csv(valid_path, na.strings = "")

    # Clean feature names to get base columns (e.g., 'MSI_STATUSstable' -> 'MSI_STATUS')
        base_features <- unique(stringr::str_replace(features, "(stable|Other|x|[0-9])$", ""))
    all_cols_to_select <- c(base_features, target)

    # Select base feature columns and convert target to factor
    # Prepare final data: select columns and convert target to the correct factor based on actual data ('N'/'Y')
    train_data <- train_data_raw %>%
        select(any_of(all_cols_to_select)) %>%
        mutate(across(all_of(target), ~factor(., levels = c("N", "Y"), labels = c("Alive", "Dead"))))

    valid_data <- valid_data_raw %>%
        select(any_of(all_cols_to_select)) %>%
        mutate(across(all_of(target), ~factor(., levels = c("N", "Y"), labels = c("Alive", "Dead"))))

    # Now, filter out rows where the target variable became NA (i.e., was not 'N' or 'Y' or was originally NA)
    train_data <- train_data %>% filter(!is.na(.data[[target]]))
    valid_data <- valid_data %>% filter(!is.na(.data[[target]]))

    # Separate Imputation Step: Create and apply imputation model BEFORE training
    # This avoids conflicts between preProcess and sampling inside train()
    imputation_model <- preProcess(train_data, method = c("bagImpute"))
    train_data <- predict(imputation_model, train_data)
    # Apply the same imputation model to the validation data
    if (!is.null(valid_data) && nrow(valid_data) > 0) {
        valid_data <- predict(imputation_model, valid_data)
    }

    # Stop if no data left after initial filtering
    if(nrow(train_data) == 0) {
        warning(paste("No data left for group:", group_name, "after filtering for valid target values. Skipping."))
        return(NULL)
    }


    
    # Set up train control with 10-fold CV and SMOTE for class imbalance
    ctrl <- trainControl(
        method = "cv",
        number = 10,
        classProbs = TRUE,
        summaryFunction = twoClassSummary,
        sampling = "smote" # Apply SMOTE during resampling
    )

    # Train models
    models <- list()
    model_names <- c("rf", "gbm", "xgbTree")

    for (model_name in model_names) {
        cat(paste("Training", model_name, "model...\n"))
        model_features <- intersect(features, names(train_data))

        # Defensive check: Ensure there are features to model
        if (length(model_features) == 0) {
            warning(paste("No selected features found in the training data for model:", model_name, "in group:", group_name, ". Skipping."))
            next # Skip to the next model in the loop
        }

        formula_str <- paste(target, "~", paste(model_features, collapse = " + "))
        
        models[[model_name]] <- train(
            as.formula(formula_str), # Use the dynamically created formula
            data = train_data, # Now using pre-imputed data
            method = model_name,
            trControl = ctrl,
            metric = "ROC"
        )
    }

    # Evaluate models, but only if there is validation data to evaluate on
    if (is.null(valid_data) || nrow(valid_data) == 0) {
        warning(paste("No validation data available for group:", group_name, ". Skipping evaluation."))
        return(NULL) # Return NULL to be skipped by map_df
    }

    results_df <- data.frame()
    for (model_name in names(models)) {
        preds <- predict(models[[model_name]], newdata = valid_data, type = "prob")
        roc_curve <- roc(valid_data[[target]], preds[["Dead"]], quiet = TRUE)
        results_df <- rbind(results_df, data.frame(
            Group = group_name,
            Model = model_name,
            AUC = as.numeric(auc(roc_curve))
        ))
    }

    message(paste("--- Finished Modeling for", group_name, "Group ---"))
    return(results_df)
}

# 4. Execute Modeling for Each Group
# ==============================================================================
# Create a list of all features and groups
feature_list <- list(
    total = features_total,
    eocrc = features_eocrc,
    locrc = features_locrc
)

# Run modeling for all groups
all_results <- map_df(names(feature_list), ~train_and_evaluate_models(
    group_name = .x,
    features = feature_list[[.x]],
    target = target
))

# Print and save results
print("\n--- Final Model Performance ---")
print(all_results)

# Create results directory if it doesn't exist
if (!dir.exists("results/ml_models")) {
    dir.create("results/ml_models", recursive = TRUE)
}

write.csv(all_results, "results/ml_models/model_performance_summary.csv", row.names = FALSE)

message("\nML modeling complete. Results saved to results/ml_models/model_performance_summary.csv")

