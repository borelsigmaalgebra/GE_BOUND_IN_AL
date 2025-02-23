run_al <- function(dataset = NULL,
                   model_space = as.character(),
                   batch_proz = as.numeric(),
                   initial_proz = as.numeric(),
                   sim_index = as.numeric(),
                   query_strategies = as.character(),
                   unlabeled_proz = as.numeric()
                   ){
  # Train-/Initial-Test-Split
  initial_size <- floor(nrow(dataset) * initial_proz)
  train_size <- floor(nrow(dataset) * (unlabeled_proz + initial_proz))
  test_size <- floor(nrow(dataset) * (1 - (unlabeled_proz + initial_proz)))
  batchsize <- floor(nrow(dataset) * batch_proz)
  
  index_train <- sample(nrow(dataset),
                        size = nrow(dataset) * (unlabeled_proz + initial_proz),
                        replace = FALSE)
  dataset_train <- dataset[index_train, ]
  dataset_test <- dataset[-index_train, ]
  
  #Remove labels expect for initial data and add ID
  dataset_start <- dataset_train %>%
    dplyr::mutate(LABELER = Y) %>%
    dplyr::mutate(ID = row_number())
  index_initial <- sample(nrow(dataset_start),
                          size = nrow(dataset) * (initial_proz),
                          replace = FALSE)
  for (i in 1:nrow(dataset_start)) {
    if (!(i %in% index_initial)) {
      dataset_start$Y[i] <- NA  # Set unlabeled entries to NULL
    }
  }
  
  #Get error and fit on whole training data
  model <- lm("Y ~ .", data = dataset_train)
  train_pred <- predict(model, dataset_test)
  error_test <- rmse(dataset_test$Y, train_pred)
  #plot information for all training data
  frequ_input <- ggplot(dataset_train,
                        aes(x = Y)) +
    geom_histogram(binwidth = 1,
                   fill = "blue",
                   color = "white",
                   alpha = 0.7) +
    scale_x_continuous(limits = c(min(dataset_train$Y),
                                  max(dataset_train$Y))) +
    theme_minimal() +
    labs(
      title = "Histogram of Training Data Y",
      x = "Y",
      y = "Frequency"
    ) +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5)
    )
  # Extract the data used in the plot (the counts from the histogram)
  hist_data <- ggplot_build(frequ_input)$data[[1]]
  # Find the maximum frequency (max count) in the histogram
  max_frequency <- max(hist_data$count)
  all_train_plot <- ggplot(dataset_train,
                       aes(x = Y)) +
    geom_histogram(binwidth = 1,
                   fill = "blue",
                   color = "white",
                   alpha = 0.7) +
    scale_x_continuous(limits = c(min(dataset_train$Y),
                                  max(dataset_train$Y))) +
    theme_minimal() +
    labs(
      title = "Histogram of Training Data Y",
      x = "Y",
      y = "Frequency"
    ) +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5)
    ) +
    annotate("text",
             x = quantile(dataset_train$Y, 0.99),
             y = max_frequency, label = paste0("Test Error: ",
                                               error_test),
             size = 3,
             color = "red")
  
  #Setup
  #___________________________________
  #Query-Iteration count
  init_iteration <- 0
  #setup query-strat dependent list
  qs_list <- list()
  for(qs in names(query_strategies)){
    #Add Sim index
    qs_list[[qs]]$sim_index <- sim_index
    #Add name
    qs_list[[qs]]$name <- qs
    #Add iteration
    qs_list[[qs]]$iteration <- init_iteration
    #Add Query Class
    qs_list[[qs]]$class <- query_strategies[[qs]]
    #Add initial dataset (dataset_start for all strategies at start)
    qs_list[[qs]]$dataset <- dataset_start
    #Add test dataset
    qs_list[[qs]]$dataset_test <- dataset_test
    #Add newly labeled data
    qs_list[[qs]]$new_labeled_pool_data <- NA
    #Error of new queried data
    qs_list[[qs]]$new_errors <- NA
    #Test Errors of (so far) queried an labeled data model on Test Sample over iterations
    qs_list[[qs]]$test_errors <- NA
    #Training Errors of (so far) queried an labeled data model on (so far) queried and labeled data over iterations
    qs_list[[qs]]$training_errors <- NA
    #IPMs between X of Training Sample and (so far) queried data over iterations
    qs_list[[qs]]$ipm_errors <- NA
    #Rademacher Complexity of (so far) queried an labeled data over iterations
    qs_list[[qs]]$rad_vec <- NA
    #o(i)-Terms over iterations
    qs_list[[qs]]$term_vec <- NA
    #Calculated uper bound over iterations
    qs_list[[qs]]$sum_vec <- NA
    #Model weight norm
    qs_list[[qs]]$weight_norm_vec <- NA
  }
  
  result_list <- list()
  #Run queries
  for(qs in names(query_strategies)){
    # Define the directory path
    dir_path <- file.path("02_output",
                          sim_index,
                          model_space,
                          qs)
    # Create the directory
    if (!dir.exists(dir_path)) {  # Check if the directory already exists
      dir.create(dir_path, recursive = TRUE)  # Create directory and parent directories if needed
      message("Directory created: ", dir_path)
    } else {
      message("Directory already exists: ", dir_path)
    }
  results_temp  <- run_queries(model_space = model_space,
              batchsize = batchsize,
              sim_index = sim_index,
              init_query_strategy = qs_list[[qs]],
              plot_max_frequency = max_frequency)
  # Open a connection to a text file
  file_conn <- file(file.path(dir_path, "components.txt"), "w")
  # Write Log
  writeLines(paste("Simulation index:",
                   toString(sim_index)),
             file_conn)
  writeLines(paste("Model Space:",
                   toString(model_space)),
             file_conn)
  writeLines(paste("Query Strat:",
                   toString(results_temp$name)),
             file_conn)
  writeLines(paste("Number of Iterations:",
                   toString(results_temp$iteration)),
             file_conn)
  writeLines(paste("Test Errors:",
                   toString(results_temp$test_errors)),
             file_conn)
  writeLines(paste("Training Errors:",
                   toString(results_temp$training_errors)),
             file_conn)
  writeLines(paste("IPM:",
                   toString(results_temp$ipm_errors)),
             file_conn)
  writeLines(paste("Rademacher Complexity:",
                   toString(results_temp$rad_vec)),
             file_conn)
  writeLines(paste("Constant Term:",
                   toString(results_temp$term_vec)),
             file_conn)
  writeLines(paste("Upper Bound:",
                   toString(results_temp$sum_vec)),
             file_conn)
  # Close the connection
  close(file_conn)
  result_list[[qs]] <- results_temp
  }
  #generate results
  result_list$global$plot <- all_train_plot
  result_list$global$dataset_train <- dataset_train
  result_list$global$dataset_test <- dataset_test
  result_list$global$error_test <- error_test
  return(result_list)
}

run_queries <- function(model_space = as.character(),
                        batchsize = as.numeric(),
                        sim_index = as.numeric(),
                        init_query_strategy = NULL,
                        plot_max_frequency = NULL) {
  #Initialize new list
  query_strategy <- init_query_strategy
  #parameters
  delta <- 0.95 #not necessary, because term constant over all query strategies
  #initial values
  
  # Get labeled entries in the pool for each query strategy
  labeled_data <- query_strategy$dataset %>% dplyr::filter(!is.na(Y))
  
  # Fit model on all so far queried and labeled data
  query_strategy$model <- fit_l1_regression(data = labeled_data %>% dplyr::select(-LABELER, -ID),
                                    target_variable = "Y")
  
  #Training and Test error for this model
  #Train
  y_pred_train <- predict_l1_regression(model = query_strategy$model,
                                data = labeled_data %>% dplyr::select(-LABELER, -ID, -Y))
  error_train <- mean(abs(labeled_data$Y - y_pred_train))
  #error_train <- rmse(labeled_data$Y, y_pred_train)
  query_strategy$training_errors <- error_train
  #Test
  y_pred_test <- predict_l1_regression(model = query_strategy$model,
                               data = query_strategy$dataset_test %>% dplyr::select(- Y))
  error_test <- mean(abs(query_strategy$dataset_test$Y - y_pred_test))
  #error_test <- rmse(query_strategy$dataset_test$Y, y_pred_test)
  query_strategy$test_errors <- error_test
  
  #IPM value
  error_ipm <- ipm_kantorovich_estim_func(x_data = query_strategy$dataset %>% dplyr::select(starts_with("X")),
                                          q_data = query_strategy$dataset %>% filter(!is.na(Y)) %>% dplyr::select(starts_with("X")),
                                          p = 1)
  query_strategy$ipm_errors <- error_ipm
  
  #Minimal k
  y_pred_all <- predict_l1_regression(model = query_strategy$model,
                              data = query_strategy$dataset %>% dplyr::select(-LABELER, -ID, -Y))
  y_true_label_all <- query_strategy$dataset %>% dplyr::select(LABELER)
  k_min <- min(abs(y_pred_all - y_true_label_all))
  
  #Term value (not Query-Strat dependant)
  term <- k_min * sqrt((2 * log(4 / delta)) / nrow(labeled_data %>% filter(!is.na(Y)))) #TBD k upper bound of loss function
  query_strategy$term_vec <- term
  
  #Norm of weight vector of model
  model_weight_norm <- norm(query_strategy$model$coefficients, "2")
  query_strategy$weight_norm_vec <- model_weight_norm
  
  #Rademacher Complexity
  error_rad <- 2 * model_weight_norm * max(apply(labeled_data %>%
                                               dplyr::filter(!is.na(Y)) %>%
                                               dplyr::select(starts_with("X")), 1, euclidean_norm)) /
    sqrt((nrow(labeled_data %>% filter(!is.na(Y))))) #TBD aktuell upper bound hier genutzt rad <= |w| * max|x_i|/m
  query_strategy$rad_vec <- error_rad
  
  #Calculate upper bound
  sum <- error_train + error_ipm + error_rad + term
  query_strategy$sum_vec <- sum
  
  while (nrow(query_strategy$dataset %>% filter(is.na(Y))) > 0) {
    #Update batch index
    query_strategy$iteration <- query_strategy$iteration + 1
    # Initialize Query strategie
    query_class_obj <- query_strategy$class$new(batchsize = batchsize, dataset = query_strategy$dataset)
    
    # Query new datapoints
    query_strategy$new_labeled_pool_data <- query_class_obj$make_query()
    
    # Label these datapoints and update the dataset
    query_strategy$dataset <- query_strategy$dataset %>%
      dplyr::left_join(query_strategy$new_labeled_pool_data %>% rename(Y_NEW = LABELER) %>% dplyr::select(ID, Y_NEW),
                       by = "ID") %>%
      dplyr::mutate(Y = dplyr::case_when(!is.na(Y_NEW) ~ Y_NEW,
                                         T ~ Y)) %>%
      dplyr::select(-Y_NEW)
    # Get labeled entries in the pool for each query strategy
    labeled_data <- query_strategy$dataset %>% dplyr::filter(!is.na(Y))
    
    # Fit model on all so far queried and labeled data
    query_strategy$model <- fit_l1_regression(data = labeled_data %>% dplyr::select(-LABELER, -ID),
                       target_variable = "Y")
    
    #Training and Test error for this model
    #Train
    y_pred_train <- predict_l1_regression(model = query_strategy$model,
                                  data = labeled_data %>% dplyr::select(-LABELER, -ID, -Y))
    error_train <- mean(abs(labeled_data$Y - y_pred_train))
    #error_train <- rmse(labeled_data$Y, y_pred_train)
    query_strategy$training_errors <- c(query_strategy$training_errors, error_train)
    #Test
    y_pred_test <- predict_l1_regression(model = query_strategy$model,
                           data = query_strategy$dataset_test %>% dplyr::select(- Y))
    error_test <- mean(abs(query_strategy$dataset_test$Y - y_pred_test))
    #error_test <- rmse(query_strategy$dataset_test$Y, y_pred_test)
    query_strategy$test_errors <- c(query_strategy$test_errors, error_test)
    
    #IPM value
    error_ipm <- ipm_kantorovich_estim_func(x_data = query_strategy$dataset %>% dplyr::select(starts_with("X")),
                                            q_data = query_strategy$dataset %>% filter(!is.na(Y)) %>% dplyr::select(starts_with("X")),
                                            p = 1)
    query_strategy$ipm_errors <- c(query_strategy$ipm_errors, error_ipm)
    
    #Minimal k
    y_pred_all <- predict_l1_regression(model = query_strategy$model,
                                data = query_strategy$dataset %>% dplyr::select(-LABELER, -ID, -Y))
    y_true_label_all <- query_strategy$dataset %>% dplyr::select(LABELER)
    k_min <- min(abs(y_pred_all - y_true_label_all))
    
    #Term value (not Query-Strat dependant)
    term <- k_min * sqrt((2 * log(4 / delta)) / nrow(labeled_data %>% filter(!is.na(Y)))) #TBD k upper bound of loss function
    query_strategy$term_vec <- c(query_strategy$term_vec, term)
    
    #Norm of weight vector of model
    model_weight_norm <- norm(query_strategy$model$coefficients, "2")
    query_strategy$weight_norm_vec <- c(query_strategy$weight_norm_vec, model_weight_norm)
    
    #Rademacher Complexity
    error_rad <- 2 * model_weight_norm * max(apply(labeled_data %>%
                                                 dplyr::filter(!is.na(Y)) %>%
                                                 dplyr::select(starts_with("X")), 1, euclidean_norm)) /
      (nrow(labeled_data %>% filter(!is.na(Y)))) #TBD aktuell upper bound hier genutzt rad <= |w| * max|x_i|/m
    query_strategy$rad_vec <- c(query_strategy$rad_vec, error_rad)
    
    #Calculate upper bound
    sum <- error_train + error_ipm + error_rad + term
    query_strategy$sum_vec <- c(query_strategy$sum_vec, sum)
  
  #Print information about batch i
   cat("SIM: ", sim_index, " ", query_strategy$name, " ",
       "iteration:", query_strategy$iteration,
            "\n",
            "size of labeled data:", nrow(labeled_data),
            "\n",
            "test error:", error_test,
            "\n",
            "training error:", error_train,
            "\n",
            "ipm error:", error_ipm,
            "\n",
            "rademacher complexity:", error_rad,
            "\n",
            "constant term:", term,
            "\n",
            "Weight norm:", model_weight_norm,
            "\n",
            "============================",
            "\n"
    )
   
   #plot queried Data
   plot <- ggplot(labeled_data, aes(x = LABELER)) +
     geom_histogram(binwidth = 1,
                    fill = "blue",
                    alpha = 0.7) +
     scale_x_continuous(limits = c(min(query_strategy$dataset$LABELER),
                                   max(query_strategy$dataset$LABELER))) +
     scale_y_continuous(limits = c(0, plot_max_frequency)) +
     theme_light() +  # Use theme_light to keep grid lines with a white background
     labs(
       title = paste0("Histogram of so far queried Y, Iteration nr. ",
                      query_strategy$iteration,
                      " Sim nr. ", sim_index),
       x = "Y",
       y = "Frequency"
     ) +
     theme(
       text = element_text(size = 12),
       plot.title = element_text(hjust = 0.5)
     ) +
     annotate("text",
              x = median(query_strategy$dataset$LABELER),
              y = plot_max_frequency,
              label = paste0("Test Error: ",
                             error_test,
                             "\n",
                             "Training Error: ",
                             error_train),
              size = 3,
              color = "red")
   
   
   # Specify the directory and file name
   file_name <- file.path("02_output",
                          sim_index,
                          model_space,
                          query_strategy$name,
                          paste0(query_strategy$name,
                                 "_",
                                 "iteration",
                                 "_",
                                 query_strategy$iteration,
                                 ".png"))
   # Save the plot as a PNG file
   ggsave(filename = file_name, plot = plot, width = 8, height = 6)
  }
  
  return(query_strategy)
}

#Overwerite model function for different models
model_gen <- function(data = NULL,
                      target_variable = as.character(),
                      model_space = as.character()) {
  if(model_space == "linear")  {
    model <- lm(paste0(target_variable, "~."),
                data = data)
  } else if(model_space == "lasso") {
    model <- cv.glmnet(x = data %>% dplyr::select(starts_with("X")) %>% as.matrix(),
                    y = data %>% dplyr::select(starts_with(target_variable)) %>% pull(),
                    family = "gaussian",
                    alpha = 1,
                    standardize = F,
                    quantile = 0.5
                    )
  } else if(model_space == "rf") {
    model <- ranger(
      formula = paste0(target_variable, "~."), 
      data = data,
      num.trees = 100,
      mtry = 12,
      min.node.size = 1,
      sample.fraction = 1,
      replace = FALSE
    )
  } else if(model_space == "lad"){
    model <- rq(formula = paste0(target_variable, "~."),
                    data = data,
                    tau = 0.5
                )
    
  } else if(model_space == "polynomial"){
    # Generate terms dynamically
    degree <- 3 #10
    terms <- c()
    for (name in names(data %>% dplyr::select(-all_of(target_variable)))) {
      # Add polynomial terms (x_i, x_i^2, ...)
      terms <- c(terms, paste0("poly(",
                               name, ", ", degree, ", raw = TRUE)"))
    }
    model <- lm(as.formula(paste("Y ~", paste(terms, collapse = " + "))),
                data = data)
  }
  return(model)
}

#Overwrite prediction function for different models
model_predict <- function(model = NULL,
                    data = NULL,
                    model_space = as.character()){
  if(model_space == "linear"){
    predictions <- predict(object = model, newdata = data)
  } else if(model_space == "rf"){
    predictions <- predict(model, data)$predictions
  } else if(model_space == "lasso"){
    optimal_lambda <- model$lambda.min
    predictions <- predict(model, newx = data %>% as.matrix(), s = optimal_lambda)
  } else if(model_space == "logistic"){
    
  } else if(model_space == "polynomial"){
    predictions <- predict(model, newdata = data)
  } else if(model_space == "lad") {
    predictions <- predict(model, newdata = data)
  }
  return(predictions)
}

#Kantorovic IPM
ipm_kantorovich_estim_func <- function(x_data = NA,
                                       q_data = NA,
                                       p = NA) {
  a <- pp(as.matrix(x_data[sample(nrow(x_data), nrow(q_data)),]))
  b <- pp(as.matrix(q_data))
  
  wasserstein_distance <- transport::wasserstein(a = a,
                                                 b = b,
                                                 prob = T,
                                                   p = 1,
                                                 distance = "euclidean",
                                                 method = "networkflow")
  return(wasserstein_distance)
}
# Function to compute the row-wise Euclidean norm
euclidean_norm <- function(row) {
  sqrt(sum(row^2))
}

get_legend <- function(plot) {
  g <- ggplotGrob(plot)
  legend <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
  return(legend)
}

# Function to fit constrained L1-loss linear regression
fit_l1_regression <- function(data, target_variable) {
  X <- as.matrix(data %>% dplyr::select(starts_with("X")))
  n <- nrow(X)
  p <- ncol(X)
  y <- data %>% pull("Y")
  
  # Define the intercept term b_0 and the weight vector beta
  b_0 <- Variable(1)
  beta <- Variable(p)
  
  # Define the prediction model: X %*% beta + b_0
  prediction <- X %*% beta + b_0
  
  # Define the objective function (absolute loss)
  objective <- Minimize(sum(abs(prediction - y)))
  
  # Define the constraint ||beta||_2 <= 1 (no constraint on b_0)
  constraints <- list(norm(beta, "2") <= 1)
  
  # Solve the problem
  problem <- Problem(objective, constraints)
  result <- solve(problem)
  
  # Return both b_0 and beta
  return(list(intercept = result$getValue(b_0), coefficients = result$getValue(beta)))
}

# Prediction function
predict_l1_regression <- function(model, data) {
  beta <- model$coefficients
  b_0 <- model$intercept
  X <- as.matrix(data)
  return(X %*% beta + b_0)
}
