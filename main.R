# Load libraries ####
library(rhdf5)
library(ggplot2)
library(caret)        # For train-test split
library(randomForest) # For Random Forest model
library(Metrics)      # For calculating RMSE
library(ranger)
library(dplyr)
library(R6)
library(MASS)
library(datasets)
library(lpSolve)
library(gridExtra)
library(kantorovich)
library(transport)
library(truncSP)
library(stringr)
library(glmnet)
library(tidyr)
library(latex2exp)
library(rqPen)
library(quantreg)
library(scales)
library(CVXR)
#Config ####
#______________________________________________________________________________
source("functions.R")
# Delete all output files and subdirectories in folder "02_output"
unlink(file.path("02_output", "*"), recursive = TRUE)  # Recursively delete
dataset_list <- list()
seed_number <- 40
#Import data ####
#______________________________________________________________________________
#Concrete
dataset_concrete <- readxl::read_excel("01_input/Concrete_Data.xls") %>%
  dplyr::rename_with(.fn = ~ paste0("X_", .), .cols = !starts_with("Concrete compressive strength(MPa, megapascals)")) %>%
  dplyr::rename("Y" = "Concrete compressive strength(MPa, megapascals)") %>%
 mutate(across(.cols = everything(), .fns = ~ (. - min(.)) / (max(.) - min(.))))
names(dataset_concrete) <- c("X_Cement",
                             "X_Blast",
                             "X_Fly",
                             "X_Water",
                             "X_Superplasticizer",
                             "X_Coarse",
                             "X_Fine",
                             "X_Age",
                             "Y")
dataset_list[["Concrete"]] <- dataset_concrete

#PM10
data(PM10)
dataset_pm10 <- PM10 %>%
  dplyr::rename_with(.fn = ~ paste0("X_", .), .cols = !starts_with("PM10")) %>%
  dplyr::rename("Y" = "PM10") %>%
  mutate(across(.cols = everything(), .fns = ~ (. - min(.)) / (max(.) - min(.))))
dataset_list[["PM10"]] <- dataset_pm10

#Housing
# URL for the dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
# Load the dataset
housing_data <- read.table(url, header = FALSE)
# Add column names (from the dataset documentation)
colnames(housing_data) <- c(
  "CRIM",   # Per capita crime rate by town
  "ZN",     # Proportion of residential land zoned for lots over 25,000 sq.ft.
  "INDUS",  # Proportion of non-retail business acres per town
  "CHAS",   # Charles River dummy variable (1 if tract bounds river; 0 otherwise)
  "NOX",    # Nitric oxides concentration (parts per 10 million)
  "RM",     # Average number of rooms per dwelling
  "AGE",    # Proportion of owner-occupied units built prior to 1940
  "DIS",    # Weighted distances to five Boston employment centers
  "RAD",    # Index of accessibility to radial highways
  "TAX",    # Full-value property-tax rate per $10,000
  "PTRATIO",# Pupil-teacher ratio by town
  "B",      # 1000(Bk - 0.63)^2 where Bk is the proportion of Black residents by town
  "LSTAT",  # % lower status of the population
  "MEDV"    # Median value of owner-occupied homes in $1000's
)
dataset_housing <- housing_data %>%
  dplyr::rename_with(.fn = ~ paste0("X_", .), .cols = !starts_with("MEDV")) %>%
  dplyr::rename("Y" = "MEDV") %>%
  mutate(across(.cols = everything(), .fns = ~ (. - min(.)) / (max(.) - min(.))))
dataset_list[["Housing"]] <- dataset_housing

#Forest
# URL for the dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
# Load the dataset
dataset_forest <- read.csv(url, header = T, sep = ",") %>%
  dplyr::select(-month, -day, -rain) %>%
  dplyr::rename_with(.fn = ~ paste0("X_", .), .cols = !starts_with("area")) %>%
  dplyr::rename("Y" = "area") %>%
  mutate(across(.cols = everything(), .fns = ~ (. - min(.)) / (max(.) - min(.))))
dataset_list[["Forest"]] <- dataset_forest

#Red Wine
# Download the dataset manually from the UCI repository
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
# Read the data directly from the URL
dataset_red_wine <- read.csv(url, header = T, sep = ";") %>%
  dplyr::rename_with(.fn = ~ paste0("X_", .), .cols = !starts_with("quality")) %>%
  dplyr::rename("Y" = "quality") %>%
  mutate(across(.cols = everything(), .fns = ~ (. - min(.)) / (max(.) - min(.))))
dataset_list[["Red wine"]] <- dataset_red_wine

#White Wine
# Download the dataset manually from the UCI repository
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
# Read the data directly from the URL
dataset_white_wine <- read.csv(url, header = T, sep = ";") %>%
  dplyr::rename_with(.fn = ~ paste0("X_", .), .cols = !starts_with("quality")) %>%
  dplyr::rename("Y" = "quality") %>%
  mutate(across(.cols = everything(), .fns = ~ (. - min(.)) / (max(.) - min(.))))
dataset_list[["White wine"]] <- dataset_white_wine

# Define the URL of the dataset
# Load the dataset (no header, space-separated)
dataset_yacht <- read.table(file = file.path("01_input", "yacht_hydrodynamics.data"), header = F) %>%
  mutate(across(.cols = everything(), .fns = ~ (. - min(.)) / (max(.) - min(.))))
names(dataset_yacht) <- c("X_1","X_2","X_3","X_4","X_5","X_6","Y")
dataset_list[["Yacht"]] <- dataset_yacht


#Query Strategies####
#______________________________________________________________________________
#RS####
RS <- R6Class("RS",
              public = list(
                batchsize = NULL,
                dataset = NULL,
                
                # Initialize method
                initialize = function(batchsize = 1, dataset = NULL) {
                  if (is.null(dataset)) {
                    stop("__init__() missing required argument: 'dataset'")
                  }
                  self$batchsize <- batchsize
                  self$dataset <- dataset
                },
                
                # Make query method
                make_query = function() {
                  # Get the unlabeled entries
                  X_pool <- self$dataset %>%
                    dplyr::filter(is.na(Y)) %>%
                    dplyr::select(-Y)
                  
                  if (nrow(X_pool) > self$batchsize) {
                    new_labeled_data_tbl <- X_pool[sample(1:nrow(X_pool), size = self$batchsize), ]
                  } else {
                    # The number of remaining unlabeled data is less than batch size
                    new_labeled_data_tbl <- X_pool
                  }
                  
                  return(new_labeled_data_tbl)
                }
              )
)

#SQBCR_REG_SUB_4####
SQBCR_REG_SUB_4 <- R6Class("SQBCR_REG_SUB_4",
                                   public = list(
                                     batchsize = NULL,
                                     dataset = NULL,
                                     
                                     # Initialize method
                                     initialize = function(batchsize = 1, dataset = NULL) {
                                       if (is.null(dataset)) {
                                         stop("__init__() missing required argument: 'dataset'")
                                       }
                                       self$batchsize <- batchsize
                                       self$dataset <- dataset
                                     },
                                     
                                     # Make query method
                                     make_query = function() {
                                       # Get the unlabeled entries
                                       X_pool <- self$dataset %>%
                                         dplyr::filter(is.na(Y)) %>%
                                         dplyr::select(-Y)
                                       # Get the lableled entries
                                       labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                         dplyr::select(-ID, -LABELER)
                                       if (nrow(X_pool) > self$batchsize) {
                                         # Predict the values of the unlabeled data with Committee
                                         models <- list()  # List to store models
                                         bootstrap_indices <- list()  # To keep track of sampled indices
                                         for (i in 1:4) {
                                           # Bootstrap sampling (with replacement)
                                           random_samples <- sample(1:nrow(labeled_data),
                                                                    size = nrow(self$dataset), replace = T)
                                           bootstrap_data <- labeled_data[random_samples,]
                                           
                                           # Fit a linear regression model on the bootstrap sample
                                           formula <- as.formula(paste("Y", "~ ."))
                                           model <- model_gen(data = bootstrap_data,
                                                              target_variable = "Y",
                                                              model_space = "polynomial")
                                           
                                           # Store the model and sampled indices
                                           models[[i]] <- model
                                         }
                                         
                                         predictions_tbl <- sapply(models, function(model) {
                                           model_predict(model = model,
                                                         data = X_pool,
                                                         model_space = "polynomial")
                                         }) %>% as.data.frame() %>%
                                           cbind(X_pool)
                                         
                                         # Compute the standard deviation of predictions for each unlabeled datapoint
                                         sd_tbl <- predictions_tbl %>%
                                           dplyr::mutate(SD = apply(predictions_tbl %>%
                                                                      dplyr::select(starts_with("V")), 1, sd)) %>%
                                           #Compute weighted probabilites of standard deviation
                                           dplyr::mutate(SD_PROB_NORM = exp(SD) / sum(exp(SD))) %>%
                                           dplyr::mutate(SD_L = (SD - min(SD)) / (max(SD) - min(SD))) %>%
                                           dplyr::mutate(SD_L_PROB = SD_L / sum(SD_L))
                                         new_labeled_data_tbl <- sd_tbl[sample(1:nrow(sd_tbl),
                                                                               size = self$batchsize, prob = sd_tbl$SD_L_PROB), ] %>%
                                           dplyr::select(-starts_with("V"), -SD, -SD_PROB_NORM, -SD_L, -SD_L_PROB)
                                       } else {
                                         # The number of remaining unlabeled data is less than batch size
                                         new_labeled_data_tbl <- X_pool
                                       }
                                       
                                       return(new_labeled_data_tbl)
                                     }
                                   )
)
#SQBCR_AREG_SUB_4####
SQBCR_AREG_SUB_4 <- R6Class("SQBCR_AREG_SUB_4",
                           public = list(
                             batchsize = NULL,
                             dataset = NULL,
                             
                             # Initialize method
                             initialize = function(batchsize = 1, dataset = NULL) {
                               if (is.null(dataset)) {
                                 stop("__init__() missing required argument: 'dataset'")
                               }
                               self$batchsize <- batchsize
                               self$dataset <- dataset
                             },
                             
                             # Make query method
                             make_query = function() {
                               # Get the unlabeled entries
                               X_pool <- self$dataset %>%
                                 dplyr::filter(is.na(Y)) %>%
                                 dplyr::select(-Y)
                               # Get the lableled entries
                               labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                 dplyr::select(-ID, -LABELER)
                               if (nrow(X_pool) > self$batchsize) {
                                 # Predict the values of the unlabeled data with Committee
                                 models <- list()  # List to store models
                                 bootstrap_indices <- list()  # To keep track of sampled indices
                                 for (i in 1:4) {
                                   # Bootstrap sampling (with replacement)
                                   random_samples <- sample(1:nrow(labeled_data),
                                                            size = nrow(self$dataset), replace = T)
                                   bootstrap_data <- labeled_data[random_samples,]
                                   
                                   # Fit a linear regression model on the bootstrap sample
                                   formula <- as.formula(paste("Y", "~ ."))
                                   model <- model_gen(data = bootstrap_data,
                                                      target_variable = "Y",
                                                      model_space = "lad")
                                   
                                   # Store the model and sampled indices
                                   models[[i]] <- model
                                 }
                                 
                                 predictions_tbl <- sapply(models, function(model) {
                                   model_predict(model = model,
                                                 data = X_pool,
                                                 model_space = "lad")
                                 }) %>% as.data.frame() %>%
                                   cbind(X_pool)
                                 
                                 # Compute the standard deviation of predictions for each unlabeled datapoint
                                 sd_tbl <- predictions_tbl %>%
                                   dplyr::mutate(SD = apply(predictions_tbl %>%
                                                              dplyr::select(starts_with("V")), 1, sd)) %>%
                                   #Compute weighted probabilites of standard deviation
                                   dplyr::mutate(SD_PROB_NORM = exp(SD) / sum(exp(SD))) %>%
                                   dplyr::mutate(SD_L = (SD - min(SD)) / (max(SD) - min(SD))) %>%
                                   dplyr::mutate(SD_L_PROB = SD_L / sum(SD_L))
                                 new_labeled_data_tbl <- sd_tbl[sample(1:nrow(sd_tbl),
                                                                       size = self$batchsize, prob = sd_tbl$SD_L_PROB), ] %>%
                                   dplyr::select(-starts_with("V"), -SD, -SD_PROB_NORM, -SD_L, -SD_L_PROB)
                               } else {
                                 # The number of remaining unlabeled data is less than batch size
                                 new_labeled_data_tbl <- X_pool
                               }
                               
                               return(new_labeled_data_tbl)
                             }
                           )
)
#SQBCR_AREG_SUB_10####
SQBCR_AREG_SUB_10 <- R6Class("SQBCR_AREG_SUB_10",
                            public = list(
                              batchsize = NULL,
                              dataset = NULL,
                              
                              # Initialize method
                              initialize = function(batchsize = 1, dataset = NULL) {
                                if (is.null(dataset)) {
                                  stop("__init__() missing required argument: 'dataset'")
                                }
                                self$batchsize <- batchsize
                                self$dataset <- dataset
                              },
                              
                              # Make query method
                              make_query = function() {
                                # Get the unlabeled entries
                                X_pool <- self$dataset %>%
                                  dplyr::filter(is.na(Y)) %>%
                                  dplyr::select(-Y)
                                # Get the lableled entries
                                labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                  dplyr::select(-ID, -LABELER)
                                if (nrow(X_pool) > self$batchsize) {
                                  # Predict the values of the unlabeled data with Committee
                                  models <- list()  # List to store models
                                  bootstrap_indices <- list()  # To keep track of sampled indices
                                  for (i in 1:10) {
                                    # Bootstrap sampling (with replacement)
                                    random_samples <- sample(1:nrow(labeled_data),
                                                             size = nrow(self$dataset), replace = T)
                                    bootstrap_data <- labeled_data[random_samples,]
                                    
                                    # Fit a linear regression model on the bootstrap sample
                                    formula <- as.formula(paste("Y", "~ ."))
                                    model <- model_gen(data = bootstrap_data,
                                                       target_variable = "Y",
                                                       model_space = "lad")
                                    
                                    # Store the model and sampled indices
                                    models[[i]] <- model
                                  }
                                  
                                  predictions_tbl <- sapply(models, function(model) {
                                    model_predict(model = model,
                                                  data = X_pool,
                                                  model_space = "lad")
                                  }) %>% as.data.frame() %>%
                                    cbind(X_pool)
                                  
                                  # Compute the standard deviation of predictions for each unlabeled datapoint
                                  sd_tbl <- predictions_tbl %>%
                                    dplyr::mutate(SD = apply(predictions_tbl %>%
                                                               dplyr::select(starts_with("V")), 1, sd)) %>%
                                    #Compute weighted probabilites of standard deviation
                                    dplyr::mutate(SD_PROB_NORM = exp(SD) / sum(exp(SD))) %>%
                                    dplyr::mutate(SD_L = (SD - min(SD)) / (max(SD) - min(SD))) %>%
                                    dplyr::mutate(SD_L_PROB = SD_L / sum(SD_L))
                                  new_labeled_data_tbl <- sd_tbl[sample(1:nrow(sd_tbl),
                                                                        size = self$batchsize, prob = sd_tbl$SD_L_PROB), ] %>%
                                    dplyr::select(-starts_with("V"), -SD, -SD_PROB_NORM, -SD_L, -SD_L_PROB)
                                } else {
                                  # The number of remaining unlabeled data is less than batch size
                                  new_labeled_data_tbl <- X_pool
                                }
                                
                                return(new_labeled_data_tbl)
                              }
                            )
)
#SQBCR_AREG_SUB_25####
SQBCR_AREG_SUB_25 <- R6Class("SQBCR_AREG_SUB_25",
                             public = list(
                               batchsize = NULL,
                               dataset = NULL,
                               
                               # Initialize method
                               initialize = function(batchsize = 1, dataset = NULL) {
                                 if (is.null(dataset)) {
                                   stop("__init__() missing required argument: 'dataset'")
                                 }
                                 self$batchsize <- batchsize
                                 self$dataset <- dataset
                               },
                               
                               # Make query method
                               make_query = function() {
                                 # Get the unlabeled entries
                                 X_pool <- self$dataset %>%
                                   dplyr::filter(is.na(Y)) %>%
                                   dplyr::select(-Y)
                                 # Get the lableled entries
                                 labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                   dplyr::select(-ID, -LABELER)
                                 if (nrow(X_pool) > self$batchsize) {
                                   # Predict the values of the unlabeled data with Committee
                                   models <- list()  # List to store models
                                   bootstrap_indices <- list()  # To keep track of sampled indices
                                   for (i in 1:25) {
                                     # Bootstrap sampling (with replacement)
                                     random_samples <- sample(1:nrow(labeled_data),
                                                              size = nrow(self$dataset), replace = T)
                                     bootstrap_data <- labeled_data[random_samples,]
                                     
                                     # Fit a linear regression model on the bootstrap sample
                                     formula <- as.formula(paste("Y", "~ ."))
                                     model <- model_gen(data = bootstrap_data,
                                                        target_variable = "Y",
                                                        model_space = "lad")
                                     
                                     # Store the model and sampled indices
                                     models[[i]] <- model
                                   }
                                   
                                   predictions_tbl <- sapply(models, function(model) {
                                     model_predict(model = model,
                                                   data = X_pool,
                                                   model_space = "lad")
                                   }) %>% as.data.frame() %>%
                                     cbind(X_pool)
                                   
                                   # Compute the standard deviation of predictions for each unlabeled datapoint
                                   sd_tbl <- predictions_tbl %>%
                                     dplyr::mutate(SD = apply(predictions_tbl %>%
                                                                dplyr::select(starts_with("V")), 1, sd)) %>%
                                     #Compute weighted probabilites of standard deviation
                                     dplyr::mutate(SD_PROB_NORM = exp(SD) / sum(exp(SD))) %>%
                                     dplyr::mutate(SD_L = (SD - min(SD)) / (max(SD) - min(SD))) %>%
                                     dplyr::mutate(SD_L_PROB = SD_L / sum(SD_L))
                                   new_labeled_data_tbl <- sd_tbl[sample(1:nrow(sd_tbl),
                                                                         size = self$batchsize, prob = sd_tbl$SD_L_PROB), ] %>%
                                     dplyr::select(-starts_with("V"), -SD, -SD_PROB_NORM, -SD_L, -SD_L_PROB)
                                 } else {
                                   # The number of remaining unlabeled data is less than batch size
                                   new_labeled_data_tbl <- X_pool
                                 }
                                 
                                 return(new_labeled_data_tbl)
                               }
                             )
)
#SQBCR_REG_SUB_10####
SQBCR_REG_SUB_10 <- R6Class("SQBCR_REG_SUB_10",
                           public = list(
                             batchsize = NULL,
                             dataset = NULL,
                             
                             # Initialize method
                             initialize = function(batchsize = 1, dataset = NULL) {
                               if (is.null(dataset)) {
                                 stop("__init__() missing required argument: 'dataset'")
                               }
                               self$batchsize <- batchsize
                               self$dataset <- dataset
                             },
                             
                             # Make query method
                             make_query = function() {
                               # Get the unlabeled entries
                               X_pool <- self$dataset %>%
                                 dplyr::filter(is.na(Y)) %>%
                                 dplyr::select(-Y)
                               # Get the lableled entries
                               labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                 dplyr::select(-ID, -LABELER)
                               if (nrow(X_pool) > self$batchsize) {
                                 # Predict the values of the unlabeled data with Committee
                                 models <- list()  # List to store models
                                 bootstrap_indices <- list()  # To keep track of sampled indices
                                 for (i in 1:10) {
                                   # Bootstrap sampling (with replacement)
                                   random_samples <- sample(1:nrow(labeled_data),
                                                            size = nrow(self$dataset), replace = T)
                                   bootstrap_data <- labeled_data[random_samples,]
                                   
                                   # Fit a linear regression model on the bootstrap sample
                                   formula <- as.formula(paste("Y", "~ ."))
                                   model <- model_gen(data = bootstrap_data,
                                                      target_variable = "Y",
                                                      model_space = "polynomial")
                                   
                                   # Store the model and sampled indices
                                   models[[i]] <- model
                                 }
                                 
                                 predictions_tbl <- sapply(models, function(model) {
                                   model_predict(model = model,
                                                 data = X_pool,
                                                 model_space = "polynomial")
                                 }) %>% as.data.frame() %>%
                                   cbind(X_pool)
                                 
                                 # Compute the standard deviation of predictions for each unlabeled datapoint
                                 sd_tbl <- predictions_tbl %>%
                                   dplyr::mutate(SD = apply(predictions_tbl %>%
                                                              dplyr::select(starts_with("V")), 1, sd)) %>%
                                   #Compute weighted probabilites of standard deviation
                                   dplyr::mutate(SD_PROB_NORM = exp(SD) / sum(exp(SD))) %>%
                                   dplyr::mutate(SD_L = (SD - min(SD)) / (max(SD) - min(SD))) %>%
                                   dplyr::mutate(SD_L_PROB = SD_L / sum(SD_L))
                                 new_labeled_data_tbl <- sd_tbl[sample(1:nrow(sd_tbl),
                                                                       size = self$batchsize, prob = sd_tbl$SD_L_PROB), ] %>%
                                   dplyr::select(-starts_with("V"), -SD, -SD_PROB_NORM, -SD_L, -SD_L_PROB)
                               } else {
                                 # The number of remaining unlabeled data is less than batch size
                                 new_labeled_data_tbl <- X_pool
                               }
                               
                               return(new_labeled_data_tbl)
                             }
                           )
)
#SQBCR_REG_SUB_25####
SQBCR_REG_SUB_25 <- R6Class("SQBCR_REG_SUB_25",
                           public = list(
                             batchsize = NULL,
                             dataset = NULL,
                             
                             # Initialize method
                             initialize = function(batchsize = 1, dataset = NULL) {
                               if (is.null(dataset)) {
                                 stop("__init__() missing required argument: 'dataset'")
                               }
                               self$batchsize <- batchsize
                               self$dataset <- dataset
                             },
                             
                             # Make query method
                             make_query = function() {
                               # Get the unlabeled entries
                               X_pool <- self$dataset %>%
                                 dplyr::filter(is.na(Y)) %>%
                                 dplyr::select(-Y)
                               # Get the lableled entries
                               labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                 dplyr::select(-ID, -LABELER)
                               if (nrow(X_pool) > self$batchsize) {
                                 # Predict the values of the unlabeled data with Committee
                                 models <- list()  # List to store models
                                 bootstrap_indices <- list()  # To keep track of sampled indices
                                 for (i in 1:25) {
                                   # Bootstrap sampling (with replacement)
                                   random_samples <- sample(1:nrow(labeled_data),
                                                            size = nrow(self$dataset), replace = T)
                                   bootstrap_data <- labeled_data[random_samples,]
                                   
                                   # Fit a linear regression model on the bootstrap sample
                                   formula <- as.formula(paste("Y", "~ ."))
                                   model <- model_gen(data = bootstrap_data,
                                                      target_variable = "Y",
                                                      model_space = "polynomial")
                                   
                                   # Store the model and sampled indices
                                   models[[i]] <- model
                                 }
                                 
                                 predictions_tbl <- sapply(models, function(model) {
                                   model_predict(model = model,
                                                 data = X_pool,
                                                 model_space = "polynomial")
                                 }) %>% as.data.frame() %>%
                                   cbind(X_pool)
                                 
                                 # Compute the standard deviation of predictions for each unlabeled datapoint
                                 sd_tbl <- predictions_tbl %>%
                                   dplyr::mutate(SD = apply(predictions_tbl %>%
                                                              dplyr::select(starts_with("V")), 1, sd)) %>%
                                   #Compute weighted probabilites of standard deviation
                                   dplyr::mutate(SD_PROB_NORM = exp(SD) / sum(exp(SD))) %>%
                                   dplyr::mutate(SD_L = (SD - min(SD)) / (max(SD) - min(SD))) %>%
                                   dplyr::mutate(SD_L_PROB = SD_L / sum(SD_L))
                                 new_labeled_data_tbl <- sd_tbl[sample(1:nrow(sd_tbl),
                                                                       size = self$batchsize, prob = sd_tbl$SD_L_PROB), ] %>%
                                   dplyr::select(-starts_with("V"), -SD, -SD_PROB_NORM, -SD_L, -SD_L_PROB)
                               } else {
                                 # The number of remaining unlabeled data is less than batch size
                                 new_labeled_data_tbl <- X_pool
                               }
                               
                               return(new_labeled_data_tbl)
                             }
                           )
)
#QBCR_AREG_SUB_4####
QBCR_AREG_SUB_4 <- R6Class("QBCR_AREG_SUB_4",
                          public = list(
                            batchsize = NULL,
                            dataset = NULL,
                            
                            # Initialize method
                            initialize = function(batchsize = 1, dataset = NULL) {
                              if (is.null(dataset)) {
                                stop("__init__() missing required argument: 'dataset'")
                              }
                              self$batchsize <- batchsize
                              self$dataset <- dataset
                            },
                            
                            # Make query method
                            make_query = function() {
                              # Get the unlabeled entries
                              X_pool <- self$dataset %>%
                                dplyr::filter(is.na(Y)) %>%
                                dplyr::select(-Y)
                              # Get the lableled entries
                              labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                dplyr::select(-ID, -LABELER)
                              
                              if (nrow(X_pool) > self$batchsize) {
                                # Predict the values of the unlabeled data with Committee
                                models <- list()  # List to store models
                                bootstrap_indices <- list()  # To keep track of sampled indices
                                for (i in 1:4) {
                                  # Bootstrap sampling (with replacement)
                                  random_samples <- sample(1:nrow(labeled_data),
                                                           size = nrow(self$dataset), replace = T)
                                  bootstrap_data <- labeled_data[random_samples,]
                                  
                                  # Fit a lad regression model on the bootstrap sample
                                  formula <- as.formula(paste("Y", "~ ."))
                                  model <- model_gen(data = bootstrap_data,
                                                     target_variable = "Y",
                                                     model_space = "lad")
                                  
                                  # Store the model and sampled indices
                                  models[[i]] <- model
                                }
                                
                                predictions_tbl <- sapply(models, function(model) {
                                  model_predict(model, X_pool, "lad")
                                }) %>% as.data.frame() %>%
                                  cbind(X_pool)
                                # Compute the standard deviation of predictions for each unlabeled datapoint
                                sd_tbl <- predictions_tbl %>%
                                  dplyr::mutate(SD = apply(predictions_tbl %>%
                                                             dplyr::select(starts_with("V")), 1, sd),
                                                MEAN_ERROR = abs(apply(predictions_tbl %>%
                                                                         dplyr::select(starts_with("V")), 1, mean) - LABELER)) %>%
                                  # Get most uncertain predictions
                                  dplyr::arrange(desc(SD))
                                new_labeled_data_tbl <- head(sd_tbl, self$batchsize) %>%
                                  dplyr::select(-starts_with("V"), -SD, -MEAN_ERROR)
                              } else {
                                # The number of remaining unlabeled data is less than batch size
                                new_labeled_data_tbl <- X_pool
                              }
                              
                              return(new_labeled_data_tbl)
                            }
                          )
)
#QBCR_AREG_SUB_10####
QBCR_AREG_SUB_10 <- R6Class("QBCR_AREG_SUB_10",
                           public = list(
                             batchsize = NULL,
                             dataset = NULL,
                             
                             # Initialize method
                             initialize = function(batchsize = 1, dataset = NULL) {
                               if (is.null(dataset)) {
                                 stop("__init__() missing required argument: 'dataset'")
                               }
                               self$batchsize <- batchsize
                               self$dataset <- dataset
                             },
                             
                             # Make query method
                             make_query = function() {
                               # Get the unlabeled entries
                               X_pool <- self$dataset %>%
                                 dplyr::filter(is.na(Y)) %>%
                                 dplyr::select(-Y)
                               # Get the lableled entries
                               labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                 dplyr::select(-ID, -LABELER)
                               
                               if (nrow(X_pool) > self$batchsize) {
                                 # Predict the values of the unlabeled data with Committee
                                 models <- list()  # List to store models
                                 bootstrap_indices <- list()  # To keep track of sampled indices
                                 for (i in 1:10) {
                                   # Bootstrap sampling (with replacement)
                                   random_samples <- sample(1:nrow(labeled_data),
                                                            size = nrow(self$dataset), replace = T)
                                   bootstrap_data <- labeled_data[random_samples,]
                                   
                                   # Fit a lad regression model on the bootstrap sample
                                   formula <- as.formula(paste("Y", "~ ."))
                                   model <- model_gen(data = bootstrap_data,
                                                      target_variable = "Y",
                                                      model_space = "lad")
                                   
                                   # Store the model and sampled indices
                                   models[[i]] <- model
                                 }
                                 
                                 predictions_tbl <- sapply(models, function(model) {
                                   model_predict(model, X_pool, "lad")
                                 }) %>% as.data.frame() %>%
                                   cbind(X_pool)
                                 # Compute the standard deviation of predictions for each unlabeled datapoint
                                 sd_tbl <- predictions_tbl %>%
                                   dplyr::mutate(SD = apply(predictions_tbl %>%
                                                              dplyr::select(starts_with("V")), 1, sd),
                                                 MEAN_ERROR = abs(apply(predictions_tbl %>%
                                                                          dplyr::select(starts_with("V")), 1, mean) - LABELER)) %>%
                                   # Get most uncertain predictions
                                   dplyr::arrange(desc(SD))
                                 new_labeled_data_tbl <- head(sd_tbl, self$batchsize) %>%
                                   dplyr::select(-starts_with("V"), -SD, -MEAN_ERROR)
                               } else {
                                 # The number of remaining unlabeled data is less than batch size
                                 new_labeled_data_tbl <- X_pool
                               }
                               
                               return(new_labeled_data_tbl)
                             }
                           )
)
#QBCR_AREG_SUB_25####
QBCR_AREG_SUB_25 <- R6Class("QBCR_AREG_SUB_25",
                            public = list(
                              batchsize = NULL,
                              dataset = NULL,
                              
                              # Initialize method
                              initialize = function(batchsize = 1, dataset = NULL) {
                                if (is.null(dataset)) {
                                  stop("__init__() missing required argument: 'dataset'")
                                }
                                self$batchsize <- batchsize
                                self$dataset <- dataset
                              },
                              
                              # Make query method
                              make_query = function() {
                                # Get the unlabeled entries
                                X_pool <- self$dataset %>%
                                  dplyr::filter(is.na(Y)) %>%
                                  dplyr::select(-Y)
                                # Get the lableled entries
                                labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                  dplyr::select(-ID, -LABELER)
                                
                                if (nrow(X_pool) > self$batchsize) {
                                  # Predict the values of the unlabeled data with Committee
                                  models <- list()  # List to store models
                                  bootstrap_indices <- list()  # To keep track of sampled indices
                                  for (i in 1:25) {
                                    # Bootstrap sampling (with replacement)
                                    random_samples <- sample(1:nrow(labeled_data),
                                                             size = nrow(self$dataset), replace = T)
                                    bootstrap_data <- labeled_data[random_samples,]
                                    
                                    # Fit a lad regression model on the bootstrap sample
                                    formula <- as.formula(paste("Y", "~ ."))
                                    model <- model_gen(data = bootstrap_data,
                                                       target_variable = "Y",
                                                       model_space = "lad")
                                    
                                    # Store the model and sampled indices
                                    models[[i]] <- model
                                  }
                                  
                                  predictions_tbl <- sapply(models, function(model) {
                                    model_predict(model, X_pool, "lad")
                                  }) %>% as.data.frame() %>%
                                    cbind(X_pool)
                                  # Compute the standard deviation of predictions for each unlabeled datapoint
                                  sd_tbl <- predictions_tbl %>%
                                    dplyr::mutate(SD = apply(predictions_tbl %>%
                                                               dplyr::select(starts_with("V")), 1, sd),
                                                  MEAN_ERROR = abs(apply(predictions_tbl %>%
                                                                           dplyr::select(starts_with("V")), 1, mean) - LABELER)) %>%
                                    # Get most uncertain predictions
                                    dplyr::arrange(desc(SD))
                                  new_labeled_data_tbl <- head(sd_tbl, self$batchsize) %>%
                                    dplyr::select(-starts_with("V"), -SD, -MEAN_ERROR)
                                } else {
                                  # The number of remaining unlabeled data is less than batch size
                                  new_labeled_data_tbl <- X_pool
                                }
                                
                                return(new_labeled_data_tbl)
                              }
                            )
)
#QBCR_REG_SUB_4####
QBCR_REG_SUB_4 <- R6Class("QBCR_REG_SUB_4",
                          public = list(
                            batchsize = NULL,
                            dataset = NULL,
                            
                            # Initialize method
                            initialize = function(batchsize = 1, dataset = NULL) {
                              if (is.null(dataset)) {
                                stop("__init__() missing required argument: 'dataset'")
                              }
                              self$batchsize <- batchsize
                              self$dataset <- dataset
                            },
                            
                            # Make query method
                            make_query = function() {
                              # Get the unlabeled entries
                              X_pool <- self$dataset %>%
                                dplyr::filter(is.na(Y)) %>%
                                dplyr::select(-Y)
                              # Get the lableled entries
                              labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                dplyr::select(-ID, -LABELER)
                              
                              if (nrow(X_pool) > self$batchsize) {
                                # Predict the values of the unlabeled data with Committee
                                models <- list()  # List to store models
                                bootstrap_indices <- list()  # To keep track of sampled indices
                                for (i in 1:4) {
                                  # Bootstrap sampling (with replacement)
                                  random_samples <- sample(1:nrow(labeled_data),
                                                           size = nrow(self$dataset), replace = T)
                                  bootstrap_data <- labeled_data[random_samples,]
                                  
                                  # Fit a linear regression model on the bootstrap sample
                                  formula <- as.formula(paste("Y", "~ ."))
                                  model <- model_gen(data = bootstrap_data,
                                                     target_variable = "Y",
                                                     model_space = "linear")
                                  
                                  # Store the model and sampled indices
                                  models[[i]] <- model
                                }
                                
                                predictions_tbl <- sapply(models, function(model) {
                                  model_predict(model, X_pool, "linear")
                                }) %>% as.data.frame() %>%
                                  cbind(X_pool)
                                # Compute the standard deviation of predictions for each unlabeled datapoint
                                sd_tbl <- predictions_tbl %>%
                                  dplyr::mutate(SD = apply(predictions_tbl %>%
                                                             dplyr::select(starts_with("V")), 1, sd),
                                                MEAN_ERROR = abs(apply(predictions_tbl %>%
                                                                         dplyr::select(starts_with("V")), 1, mean) - LABELER)) %>%
                                  # Get most uncertain predictions
                                  dplyr::arrange(desc(SD))
                                new_labeled_data_tbl <- head(sd_tbl, self$batchsize) %>%
                                  dplyr::select(-starts_with("V"), -SD, -MEAN_ERROR)
                              } else {
                                # The number of remaining unlabeled data is less than batch size
                                new_labeled_data_tbl <- X_pool
                              }
                              
                              return(new_labeled_data_tbl)
                            }
                          )
)
#QBCR_REG_SUB_10####
QBCR_REG_SUB_10 <- R6Class("QBCR_REG_SUB_10",
                          public = list(
                            batchsize = NULL,
                            dataset = NULL,
                            
                            # Initialize method
                            initialize = function(batchsize = 1, dataset = NULL) {
                              if (is.null(dataset)) {
                                stop("__init__() missing required argument: 'dataset'")
                              }
                              self$batchsize <- batchsize
                              self$dataset <- dataset
                            },
                            
                            # Make query method
                            make_query = function() {
                              # Get the unlabeled entries
                              X_pool <- self$dataset %>%
                                dplyr::filter(is.na(Y)) %>%
                                dplyr::select(-Y)
                              # Get the lableled entries
                              labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                dplyr::select(-ID, -LABELER)
                              
                              if (nrow(X_pool) > self$batchsize) {
                                # Predict the values of the unlabeled data with Committee
                                models <- list()  # List to store models
                                bootstrap_indices <- list()  # To keep track of sampled indices
                                for (i in 1:10) {
                                  # Bootstrap sampling (with replacement)
                                  random_samples <- sample(1:nrow(labeled_data),
                                                           size = nrow(self$dataset), replace = T)
                                  bootstrap_data <- labeled_data[random_samples,]
                                  
                                  # Fit a linear regression model on the bootstrap sample
                                  formula <- as.formula(paste("Y", "~ ."))
                                  model <- model_gen(data = bootstrap_data,
                                                     target_variable = "Y",
                                                     model_space = "linear")
                                  
                                  # Store the model and sampled indices
                                  models[[i]] <- model
                                }
                                
                                predictions_tbl <- sapply(models, function(model) {
                                  model_predict(model, X_pool, "linear")
                                }) %>% as.data.frame() %>%
                                  cbind(X_pool)
                                # Compute the standard deviation of predictions for each unlabeled datapoint
                                sd_tbl <- predictions_tbl %>%
                                  dplyr::mutate(SD = apply(predictions_tbl %>%
                                                             dplyr::select(starts_with("V")), 1, sd),
                                                MEAN_ERROR = abs(apply(predictions_tbl %>%
                                                                         dplyr::select(starts_with("V")), 1, mean) - LABELER)) %>%
                                  # Get most uncertain predictions
                                  dplyr::arrange(desc(SD))
                                new_labeled_data_tbl <- head(sd_tbl, self$batchsize) %>%
                                  dplyr::select(-starts_with("V"), -SD, -MEAN_ERROR)
                              } else {
                                # The number of remaining unlabeled data is less than batch size
                                new_labeled_data_tbl <- X_pool
                              }
                              
                              return(new_labeled_data_tbl)
                            }
                          )
)
#QBCR_REG_SUB_25####
QBCR_REG_SUB_25 <- R6Class("QBCR_REG_SUB_25",
                           public = list(
                             batchsize = NULL,
                             dataset = NULL,
                             
                             # Initialize method
                             initialize = function(batchsize = 1, dataset = NULL) {
                               if (is.null(dataset)) {
                                 stop("__init__() missing required argument: 'dataset'")
                               }
                               self$batchsize <- batchsize
                               self$dataset <- dataset
                             },
                             
                             # Make query method
                             make_query = function() {
                               # Get the unlabeled entries
                               X_pool <- self$dataset %>%
                                 dplyr::filter(is.na(Y)) %>%
                                 dplyr::select(-Y)
                               # Get the lableled entries
                               labeled_data <- self$dataset %>% dplyr::filter(!is.na(Y)) %>%
                                 dplyr::select(-ID, -LABELER)
                               
                               if (nrow(X_pool) > self$batchsize) {
                                 # Predict the values of the unlabeled data with Committee
                                 models <- list()  # List to store models
                                 bootstrap_indices <- list()  # To keep track of sampled indices
                                 for (i in 1:25) {
                                   # Bootstrap sampling (with replacement)
                                   random_samples <- sample(1:nrow(labeled_data),
                                                            size = nrow(self$dataset), replace = T)
                                   bootstrap_data <- labeled_data[random_samples,]
                                   
                                   # Fit a linear regression model on the bootstrap sample
                                   formula <- as.formula(paste("Y", "~ ."))
                                   model <- model_gen(data = bootstrap_data,
                                                      target_variable = "Y",
                                                      model_space = "linear")
                                   
                                   # Store the model and sampled indices
                                   models[[i]] <- model
                                 }
                                 
                                 predictions_tbl <- sapply(models, function(model) {
                                   model_predict(model, X_pool, "linear")
                                 }) %>% as.data.frame() %>%
                                   cbind(X_pool)
                                 # Compute the standard deviation of predictions for each unlabeled datapoint
                                 sd_tbl <- predictions_tbl %>%
                                   dplyr::mutate(SD = apply(predictions_tbl %>%
                                                              dplyr::select(starts_with("V")), 1, sd),
                                                 MEAN_ERROR = abs(apply(predictions_tbl %>%
                                                                          dplyr::select(starts_with("V")), 1, mean) - LABELER)) %>%
                                   # Get most uncertain predictions
                                   dplyr::arrange(desc(SD))
                                 new_labeled_data_tbl <- head(sd_tbl, self$batchsize) %>%
                                   dplyr::select(-starts_with("V"), -SD, -MEAN_ERROR)
                               } else {
                                 # The number of remaining unlabeled data is less than batch size
                                 new_labeled_data_tbl <- X_pool
                               }
                               
                               return(new_labeled_data_tbl)
                             }
                           )
)
#run AL algorithm ####
#______________________________________________________________________________
results_list <- list()
plot_list <- list()
for(dataset_index in (6:7)){ #(1:length(dataset_list))
  dataset_prod <- dataset_list[[dataset_index]]
  dataset_name <- names(dataset_list)[dataset_index]
  print(paste0("Dataset: ", dataset_name))
#for (setup_1 in  (1:3)){
  setup_1 <- 2
  batch_proz <- ifelse(setup_1 == 1, 0.03, ifelse(setup_1 == 2, 0.06, 0.12))
  print(paste0("Batch-Size %: ", batch_proz * 100))
#for (setup_2 in (1:3)){
  setup_2 <- 1
  initial_proz <- ifelse(setup_2 == 1, 0.1, ifelse(setup_2 == 2, 0.2, 0.3))
  print(paste0("Initial-Size %: ",initial_proz * 100))
for(i in (1:10)) {
  set.seed(seed_number + i)
  test_proz <- 0.2
  unlabeled_proz <- 1 - test_proz - initial_proz
  results_list[[i]] <-  run_al(dataset = dataset_prod %>% mutate(Y = Y),
                               model_space = "lad", #linear, rf, polynomial, svm, logistic
                               batch_proz = batch_proz,
                               initial_proz = initial_proz,
                               sim_index = i,
                               query_strategies = list("RS" = RS,
                                                       "QBCR_REG_SUB_25" = QBCR_REG_SUB_25,
                                                       "QBCR_REG_SUB_10" = QBCR_REG_SUB_10,
                                                       "QBCR_REG_SUB_4" = QBCR_REG_SUB_4,
                                                       "SQBCR_REG_SUB_25" = SQBCR_REG_SUB_25,
                                                       "SQBCR_REG_SUB_10" = SQBCR_REG_SUB_10,
                                                       "SQBCR_REG_SUB_4" = SQBCR_REG_SUB_4,
                                                       "SQBCR_AREG_SUB_25" = SQBCR_AREG_SUB_25,
                                                       "SQBCR_AREG_SUB_10" = SQBCR_AREG_SUB_10,
                                                       "SQBCR_AREG_SUB_4" = SQBCR_AREG_SUB_4,
                                                       "QBCR_AREG_SUB_25" = QBCR_AREG_SUB_25,
                                                       "QBCR_AREG_SUB_10" = QBCR_AREG_SUB_10,
                                                       "QBCR_AREG_SUB_4" = QBCR_AREG_SUB_4
                               ),
                               unlabeled_proz = unlabeled_proz
  ) 
}
#Analyse results ####
#______________________________________________________________________________
#Unlist results
#Test errors
sum_test_errors_rs <- log(1 + Reduce("+", lapply(results_list, function(x) x$RS$test_errors)) / 10)

sum_test_errors_sqbcr_reg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_4$test_errors)) / 10)
sum_test_errors_sqbcr_reg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_10$test_errors)) / 10)
sum_test_errors_sqbcr_reg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_25$test_errors)) / 10)

sum_test_errors_sqbcr_areg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_4$test_errors)) / 10)
sum_test_errors_sqbcr_areg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_10$test_errors)) / 10)
sum_test_errors_sqbcr_areg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_25$test_errors)) / 10)

sum_test_errors_qbcr_areg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_4$test_errors)) / 10)
sum_test_errors_qbcr_areg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_10$test_errors)) / 10)
sum_test_errors_qbcr_areg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_25$test_errors)) / 10)

sum_test_errors_qbcr_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_4$test_errors)) / 10)
sum_test_errors_qbcr_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_10$test_errors)) / 10)
sum_test_errors_qbcr_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_25$test_errors)) / 10)
#Training errors
sum_training_errors_rs <- log(1 + Reduce("+", lapply(results_list, function(x) x$RS$training_errors)) / 10)

sum_training_errors_sqbcr_reg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_4$training_errors)) / 10)
sum_training_errors_sqbcr_reg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_10$training_errors)) / 10)
sum_training_errors_sqbcr_reg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_25$training_errors)) / 10)

sum_training_errors_sqbcr_areg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_4$training_errors)) / 10)
sum_training_errors_sqbcr_areg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_10$training_errors)) / 10)
sum_training_errors_sqbcr_areg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_25$training_errors)) / 10)

sum_training_errors_qbcr_areg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_4$training_errors)) / 10)
sum_training_errors_qbcr_areg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_10$training_errors)) / 10)
sum_training_errors_qbcr_areg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_25$training_errors)) / 10)

sum_training_errors_qbcr_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_4$training_errors)) / 10)
sum_training_errors_qbcr_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_10$training_errors)) / 10)
sum_training_errors_qbcr_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_25$training_errors)) / 10)
#IPM errors
sum_ipm_errors_rs <- log(1 + Reduce("+", lapply(results_list, function(x) x$RS$ipm_errors)) / 10)

sum_ipm_errors_sqbcr_reg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_4$ipm_errors)) / 10)
sum_ipm_errors_sqbcr_reg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_10$ipm_errors)) / 10)
sum_ipm_errors_sqbcr_reg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_25$ipm_errors)) / 10)

sum_ipm_errors_sqbcr_areg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_4$ipm_errors)) / 10)
sum_ipm_errors_sqbcr_areg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_10$ipm_errors)) / 10)
sum_ipm_errors_sqbcr_areg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_25$ipm_errors)) / 10)

sum_ipm_errors_qbcr_areg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_4$ipm_errors)) / 10)
sum_ipm_errors_qbcr_areg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_10$ipm_errors)) / 10)
sum_ipm_errors_qbcr_areg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_25$ipm_errors)) / 10)

sum_ipm_errors_qbcr_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_4$ipm_errors)) / 10)
sum_ipm_errors_qbcr_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_10$ipm_errors)) / 10)
sum_ipm_errors_qbcr_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_25$ipm_errors)) / 10)
#Rademacher Compl.
rad_compl_rs <- log(1 + Reduce("+", lapply(results_list, function(x) x$RS$rad_vec)) / 10)

rad_compl_sqbcr_reg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_4$rad_vec)) / 10)
rad_compl_sqbcr_reg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_10$rad_vec)) / 10)
rad_compl_sqbcr_reg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_25$rad_vec)) / 10)

rad_compl_sqbcr_areg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_4$rad_vec)) / 10)
rad_compl_sqbcr_areg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_10$rad_vec)) / 10)
rad_compl_sqbcr_areg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_25$rad_vec)) / 10)

rad_compl_qbcr_areg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_4$rad_vec)) / 10)
rad_compl_qbcr_areg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_10$rad_vec)) / 10)
rad_compl_qbcr_areg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_25$rad_vec)) / 10)

rad_compl_qbcr_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_4$rad_vec)) / 10)
rad_compl_qbcr_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_10$rad_vec)) / 10)
rad_compl_qbcr_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_25$rad_vec)) / 10)

#Upper bounds
sum_upper_bounds_rs <- log(1 + Reduce("+", lapply(results_list, function(x) x$RS$sum_vec)) / 10)

sum_upper_bounds_sqbcr_reg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_4$sum_vec)) / 10)
sum_upper_bounds_sqbcr_reg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_10$sum_vec)) / 10)
sum_upper_bounds_sqbcr_reg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_REG_SUB_25$sum_vec)) / 10)

sum_upper_bounds_sqbcr_areg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_4$sum_vec)) / 10)
sum_upper_bounds_sqbcr_areg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_10$sum_vec)) / 10)
sum_upper_bounds_sqbcr_areg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$SQBCR_AREG_SUB_25$sum_vec)) / 10)

sum_upper_bounds_qbcr_areg_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_4$sum_vec)) / 10)
sum_upper_bounds_qbcr_areg_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_10$sum_vec)) / 10)
sum_upper_bounds_qbcr_areg_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_AREG_SUB_25$sum_vec)) / 10)

sum_upper_bounds_qbcr_4 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_4$sum_vec)) / 10)
sum_upper_bounds_qbcr_10 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_10$sum_vec)) / 10)
sum_upper_bounds_qbcr_25 <- log(1 + Reduce("+", lapply(results_list, function(x) x$QBCR_REG_SUB_25$sum_vec)) / 10)
# generate plots
n_training_data <- nrow(dataset_prod) 
# Get the length of errors
len_errors <- length(results_list[[1]]$RS$test_errors)
# Loop through the range from 1 to the length of errors
n_training_data <- (1:(len_errors - 2))
source("PLOT_5.R")
source("PLOT_4.R")
source("PLOT_3.R")
source("PLOT_2.R")
source("PLOT_1.R")
source("PLOT_LEGEND.R")
#source("PLOT_6.R")
plot_list[["PLOTS"]][[dataset_name]][["PLOT_1"]] <- plot_1
plot_list[["PLOTS"]][[dataset_name]][["PLOT_2"]] <- plot_2
plot_list[["PLOTS"]][[dataset_name]][["PLOT_3"]] <- plot_3
plot_list[["PLOTS"]][[dataset_name]][["PLOT_4"]] <- plot_4
plot_list[["PLOTS"]][[dataset_name]][["PLOT_5"]] <- plot_5
plot_list[["PLOTS"]][[dataset_name]][["LEGEND"]] <- plot_legend
#plot_list[["PLOTS"]][[dataset_name]][["PLOT_6"]] <- plot_6
}
test_errors_plot <- arrangeGrob(
  plot_list[["PLOTS"]]$Concrete$LEGEND,
  plot_list[["PLOTS"]]$Concrete$PLOT_3,
  plot_list[["PLOTS"]]$PM10$PLOT_3,
  plot_list[["PLOTS"]]$Housing$PLOT_3,
  plot_list[["PLOTS"]]$Forest$PLOT_3,
  plot_list[["PLOTS"]]$'Red wine'$PLOT_3,
  plot_list[["PLOTS"]]$'White wine'$PLOT_3,
  plot_list[["PLOTS"]]$Yacht$PLOT_3,
  layout_matrix = rbind(
    c(NA, 1, NA),       # First row with first plot in the center
    c(2, 3, 4),      # Second row with 3 plots, first empty to center
    c(5, 6, 7),      # Third row with remaining plot
    c(NA,8,NA)
  )
)
ggsave(file.path("02_output","test_errors_plot.png"), plot = test_errors_plot, width = 11, height = 9, dpi = 300)
  upper_bound_plot <- arrangeGrob(
  plot_list[["PLOTS"]]$Concrete$LEGEND,
  plot_list[["PLOTS"]]$Concrete$PLOT_5,
  plot_list[["PLOTS"]]$PM10$PLOT_5,
  plot_list[["PLOTS"]]$Housing$PLOT_5,
  plot_list[["PLOTS"]]$Forest$PLOT_5,
  plot_list[["PLOTS"]]$'Red wine'$PLOT_5,
  plot_list[["PLOTS"]]$'White wine'$PLOT_5,
  plot_list[["PLOTS"]]$Yacht$PLOT_5,
  layout_matrix = rbind(
    c(NA, 1, NA),       # First row with first plot in the center
    c(2, 3, 4),      # Second row with 3 plots, first empty to center
    c(5, 6, 7),      # Third row with remaining plot
    c(NA,8,NA)
  )
)
  ggsave(file.path("02_output","upper_bound_plot.png"), plot = upper_bound_plot, width = 11, height = 9, dpi = 300)
train_errors_plot <- arrangeGrob(
  plot_list[["PLOTS"]]$Concrete$LEGEND,
  plot_list[["PLOTS"]]$Concrete$PLOT_1,
  plot_list[["PLOTS"]]$PM10$PLOT_1,
  plot_list[["PLOTS"]]$Housing$PLOT_1,
  plot_list[["PLOTS"]]$Forest$PLOT_1,
  plot_list[["PLOTS"]]$'Red wine'$PLOT_1,
  plot_list[["PLOTS"]]$'White wine'$PLOT_1,
  plot_list[["PLOTS"]]$Yacht$PLOT_1,
  layout_matrix = rbind(
    c(NA, 1, NA),       # First row with first plot in the center
    c(2, 3, 4),      # Second row with 3 plots, first empty to center
    c(5, 6, 7),      # Third row with remaining plot
    c(NA,8,NA)
  )
)
ggsave(file.path("02_output","train_errors_plot.png"), plot = train_errors_plot, width = 11, height = 9, dpi = 300)
ipm_plot <- arrangeGrob(
  plot_list[["PLOTS"]]$Concrete$LEGEND,
  plot_list[["PLOTS"]]$Concrete$PLOT_2,
  plot_list[["PLOTS"]]$PM10$PLOT_2,
  plot_list[["PLOTS"]]$Housing$PLOT_2,
  plot_list[["PLOTS"]]$Forest$PLOT_2,
  plot_list[["PLOTS"]]$'Red wine'$PLOT_2,
  plot_list[["PLOTS"]]$'White wine'$PLOT_2,
  plot_list[["PLOTS"]]$Yacht$PLOT_2,
  layout_matrix = rbind(
    c(NA, 1, NA),       # First row with first plot in the center
    c(2, 3, 4),      # Second row with 3 plots, first empty to center
    c(5, 6, 7),      # Third row with remaining plot
    c(NA,8,NA)
  )
)
ggsave(file.path("02_output","ipm_plot.png"), plot = ipm_plot, width = 11, height = 9, dpi = 300)
rad_plot <- arrangeGrob(
  plot_list[["PLOTS"]]$Concrete$LEGEND,
  plot_list[["PLOTS"]]$Concrete$PLOT_4,
  plot_list[["PLOTS"]]$PM10$PLOT_4,
  plot_list[["PLOTS"]]$Housing$PLOT_4,
  plot_list[["PLOTS"]]$Forest$PLOT_4,
  plot_list[["PLOTS"]]$'Red wine'$PLOT_4,
  plot_list[["PLOTS"]]$'White wine'$PLOT_4,
  plot_list[["PLOTS"]]$Yacht$PLOT_4,
  layout_matrix = rbind(
    c(NA, 1, NA),       # First row with first plot in the center
    c(2, 3, 4),      # Second row with 3 plots, first empty to center
    c(5, 6, 7),      # Third row with remaining plot
    c(NA,8,NA)
  )
)
ggsave(file.path("02_output","rad_plot.png"), plot = rad_plot, width = 11, height = 9, dpi = 300)

