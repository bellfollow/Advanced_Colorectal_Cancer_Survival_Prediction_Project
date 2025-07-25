# 07. Machine Learning Modeling for Survival Prediction with Custom Features

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
library(doParallel) # For parallel processing

# 2. Feature Selection
# ==============================================================================
# Define the list of custom features to be used for modeling
custom_features <- c(
    "기본환자진단시연령",
    "기본환자성별코드",
    "진단시점_CEA",
    "최고_CEA",
    "기본환자T병기값",
    "기본환자N병기값",
    "기본환자M병기값",
    "수술여부",
    "수술횟수",
    "항암치료여부",
    "항암치료횟수",
    "CAPEOX_여부",
    "FOLOFOX_여부",
    "방사선치료여부",
    "방사선치료횟수",
    "KRAS_MUTATION",
    "MSI_STATUS"
)

# Define target variable
target <- "사망여부"

print("Using custom features for all groups:")
print(custom_features)


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

    all_cols_to_select <- c(features, target)

    # Define numeric and factor columns for explicit type casting
    numeric_cols <- c("기본환자진단시연령", "진단시점_CEA", "최고_CEA", "수술횟수", "항암치료횟수", "방사선치료횟수")
    factor_cols <- setdiff(features, numeric_cols)

    # Robust data preparation: Select columns and enforce correct data types immediately
    prepare_data <- function(df) {
        df %>% 
            select(any_of(all_cols_to_select)) %>% 
            mutate(
                # Force numeric columns to be numeric (non-numeric values become NA)
                across(any_of(intersect(numeric_cols, names(.))), as.numeric),
                # Force factor columns to be factors
                across(any_of(intersect(factor_cols, names(.))), as.factor),
                # Set up the target variable
                across(all_of(target), ~factor(., levels = c("N", "Y"), labels = c("Alive", "Dead")))
            )
    }

    train_data <- prepare_data(train_data_raw)
    valid_data <- prepare_data(valid_data_raw)

    # Filter out rows where the target variable is NA
    train_data <- train_data %>% filter(!is.na(.data[[target]]))
    valid_data <- valid_data %>% filter(!is.na(.data[[target]]))

    # The 'recipes' pipeline will now handle imputation, so the separate preProcess step is no longer needed.
    # All predictors will be converted to their correct types within the recipe.

    if(nrow(train_data) == 0) {
        warning(paste("No data left for group:", group_name, ". Skipping."))
        return(NULL)
    }
    
    # Create a preprocessing recipe to handle data types and imputation robustly
    data_recipe <- recipe(train_data) %>%
        update_role(all_of(target), new_role = "outcome") %>%
        update_role(all_of(features), new_role = "predictor") %>%
        # Use a faster imputation method
        step_impute_knn(all_predictors(), neighbors = 5) %>% 
        step_novel(all_nominal_predictors()) %>% 
        step_dummy(all_nominal_predictors()) %>% 
        step_zv(all_predictors()) %>% 
        themis::step_smote(all_of(target), over_ratio = 1)

    # Set up train control with 10-fold CV
    ctrl <- trainControl(
        method = "cv",
        number = 10,
        classProbs = TRUE,
        summaryFunction = twoClassSummary
        # Sampling is now handled by the recipe
    )

    # Set up fixed tuning grids to avoid lengthy hyperparameter searches
    tune_grids <- list(
        rf = expand.grid(mtry = max(1, floor(length(features)/3))),
        gbm = expand.grid(n.trees = 150, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10),
        xgbTree = expand.grid(nrounds = 150, max_depth = 3, eta = 0.1, gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 1)
    )

    # Train models using the recipe and fixed hyperparameters
    models <- list()
    model_names <- c("rf", "gbm", "xgbTree")

    for (model_name in model_names) {
        cat(paste("Training", model_name, "model...\n"))
        
        models[[model_name]] <- train(
            data_recipe, 
            data = train_data,
            method = model_name,
            trControl = ctrl,
            metric = "ROC",
            tuneGrid = tune_grids[[model_name]]
        )
    }

    # Evaluate models
    if (is.null(valid_data) || nrow(valid_data) == 0) {
        warning(paste("No validation data available for group:", group_name, ". Skipping evaluation."))
        return(NULL)
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
# Setup parallel processing to speed up execution
# Use all available cores, but leave one free for system stability
num_cores <- detectCores() - 1
if (num_cores < 1) num_cores <- 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
# Define groups to model
groups <- c("total", "eocrc", "locrc")

# Run modeling for all groups using the same custom feature set
all_results <- groups %>% 
    purrr::map_df(~train_and_evaluate_models(group_name = .x, features = custom_features, target = target))

# 5. Save Results
# ==============================================================================
print("\n--- Final Model Performance ---")
print(all_results)

# Create results directory if it doesn't exist
if (!dir.exists("results/ml_models")) {
    dir.create("results/ml_models", recursive = TRUE)
}

write.csv(all_results, "results/ml_models/model_performance_summary_custom.csv", row.names = FALSE)

cat("\nML modeling with custom features complete. Results saved to results/ml_models/model_performance_summary_custom.csv\n")

# Stop the parallel cluster
stopCluster(cl)
