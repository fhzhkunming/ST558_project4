# ST588 Final Project
# Hui Fang

library(DT)
library(nnet)
library(shiny)
library(dplyr)
library(caret)
library(ggplot2)
library(lattice)
library(shinyjs)
library(Metrics)
library(tidyverse)
library(randomForest)
library(shinydashboard)

# Read in data 
  obesity <- read.csv("../ObesityDataSet.csv")

# Pre-compute some variables to be used by app
  not_numeric <- sapply(names(obesity), function(x) !is.numeric(obesity[[x]]))
  df <- obesity
  raw_df <- obesity

# Define UI ----
ui <- dashboardPage(
  dashboardHeader(title = div("A Comprehensive Investigation of Obesity Data", style = "font-size: 34px"), 
                              titleWidth = 1000),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("archive")),
      menuItem("Data Exploration", tabName = "data_explore", icon = icon("chart-line")),
      menuItem("Modeling", tabName = "modeling", icon = icon("chart-area"),
               menuSubItem("Modeling Info", tabName = "modeling_info"),
               menuSubItem("Model Fitting", tabName = "model_fitting"),
               menuSubItem("Prediction", tabName = "prediction")  # Close menu Item here
      )
    )
  ),
  
#####################################
# About tab content

  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              fluidRow(
                withMathJax(),
                # add a photo
                mainPanel(style = "text-align: right;",  # Align content to the center
                  imageOutput("logo")
                ),
                
                column(6,
                       h1("What does this app do?"),
                       box(background = "light-blue", width = 12,
                           h4("This is a straightforward Shiny application designed for exploratory data analysis and model fitting using an obesity dataset. The dataset, obtained from", a(href = "https://www.kaggle.com/datasets/aravindpcoder/obesity-or-cvd-risk-classifyregressorcluster",  HTML("<span style='color: blue;'>kaggle</span>")), "provides insights into estimating obesity levels in 14 to 61-year-old individuals from Mexico, Peru, and Colombia, based on their eating habits and physical conditions. The data contains 17 attributes and 2111 records, each labeled with the class variable 'NObesitydad' (Obesity Level)."),
                           h4("The application offers a comprehensive exploration of the obesity data, presenting visualizations through plots and tables based on user selections. Additionally, users have the option to manipulate model training, fitting, and prediction for a more interactive experience."),
                           h4("Attributes information:"),
                           h4(HTML(paste(
                               "<b>family_history_with_overweight:</b> 'yes', 'no',<br>",
                               "<b>FAVC:</b> Frequent consumption of high caloric food, 'yes', 'no',<br>",
                               "<b>FCVC:</b> Frequency of consumption of vegetables, 1, 2, 3,<br>",
                               "<b>NCP:</b> Number of main meals, 1, 2, 3, 4,<br>",
                               "<b>CAEC:</b> Consumption of food between meals, 'Always', 'Sometimes', 'Frequently', 'no',<br>",
                               "<b>CH2O:</b> Consumption of water daily, 1, 2, 3,<br>",
                               "<b>CALC:</b> Consumption of alcohol, 'Always', 'Sometimes', 'Frequently', 'no',<br>",
                               "<b>SCC:</b> The attributes related to the physical condition are: Calories consumption monitoring, 'yes', 'no',<br>",
                               "<b>SMOKE:</b> 'yes', 'no',<br>",
                               "<b>FAF:</b> Physical activity frequency, 0, 1, 2, 3,<br>",
                               "<b>TUE:</b> Time using technology devices, 0, 1, 2,<br>",
                               "<b>MTRANS:</b> Transportation used, 'Public Transportation', 'Automobile', 'Motorbike', 'Bike', 'Walking',<br>",
                               "<b>Nobeyesdad:</b> Obesity level, 'Underweight', 'Normal', 'Overweight', 'Obesity I', 'Obesity II', 'Obesity III' "
                               )
                             )
                           )
                           
                       )
                ),
                column(6,
                       h1("How to use the app?"),
                       box(background = "light-blue", width = 12,
                           h4(
                             HTML(paste(
                               "<b>About Page:</b> The 'About' page provides general information about the application and the underlying data. Navigate using the tabs on the left side for a seamless experience.<br>",
                               "<b>Data Exploration:</b> Explore the Obesity data comprehensively in the 'Data Exploration' tab. Begin by filtering data based on the 'Consumption of food between meals' with four levels: 'Always,' 'Sometimes,' 'Frequently,' and 'No.' Different tabs within 'EDA' offer various analyses based on the filtered data.<br>",
                               "<b>Plot Tab:</b> Explore diverse visualizations in the 'Plot' tab, including scatter plots, box plots, bar plots, and histograms for variables of interest. Customize your visualizations using dropdown boxes labeled 'X,' 'Y,' and 'Color.' Additionally, enable the 'Linear Regression' option to visualize regression lines in scatter plots.<br>",
                               "<b>Numeric Summary Tab:</b> Head to the 'Numeric Summary' tab for statistical summaries of numeric variables. Check the boxes next to statistics of interest to display them in a table. Choose multiple statistics for a comprehensive overview.<br>",
                               "<b>Character Summary Tab:</b> In this tab, explore a contingency table summarizing counts of selected character variables. Use the left panel to customize your analysis.<br>",
                               "<b>Correlation Tab:</b> In this section, you can access a correlation matrix of numeric variables based on filtered data from the 'Plot' tab.<br>",
                               "<b>Modeling Tab:</b> The 'Modeling' tab includes the following three sub-tabs:<br>",
                               "&nbsp;&nbsp;- <b>Modeling Info:</b> Get insights into the models used in the app along with their pros and cons.<br>",
                               "&nbsp;&nbsp;- <b>Model Fitting:</b> Customize your model fitting process by choosing a test/train split percentage. Select predictor variables for each model. For the random forest model, specify tuning parameter grids and cross-validation settings. Click 'Fit Model' to fit both models on the training data. View fit statistics and summaries, including a variable importance plot for the random forest model.<br>",
                               "&nbsp;&nbsp;- <b>Prediction:</b> Utilize both models for predictions in the 'Prediction' tab.<br>",
                               "Navigate through the tabs for a comprehensive analysis and model exploration."
                             )
                          )
                        )
                     )
                  )
              )
      ),
      
 #####################################
      # EDA tab content
 
      tabItem(tabName = "data_explore",
              navbarPage("EDA",
                    tabPanel("Plot", 
                        fluidRow(
                          withMathJax(),
                           sidebarLayout(
                             sidebarPanel(
                             # filter the dataset based on CAEC
                             h3(style = "color: blue; font-size: 20px;", "Select consumption of food between meals (CAEC):"),
                             selectizeInput("caec", "CAEC", selected = "yes", choices = levels(factor(df$FAVC, levels = c("Always", "Sometimes", "Frequently", "no")))),
                             br(),
                             selectInput("plotType", "Plot Type",
                                         choices = c("Scatter Plot", "Boxplot", "Bar Plot", "Histogram"),
                                         selected = "Scatter Plot"),
                             
                             selectInput("x", "X", names(df)),
                             selectInput("y", "Y", c("None", names(df)), names(df)[[2]]),
                             
                             # only allow non-numeric variables for color
                             selectInput("color", "Color", c("None", names(df)[not_numeric])),
                             
                             p("Linear regression is only available for scatter plot with two numeric variables are selected."),
                             checkboxInput("smooth", "Linear regression")
                           ),
                        
                      mainPanel(
                      # Output: Tab set
                      plotOutput("plot")
                           )
                         )
                      )
                ),
               
       tabPanel("Numeric Summary", 
               h4(style = "color: blue; font-size: 20px;", "Numeric summary is for numeric variables after filtering by CAEC."), 
               hr(),
               sidebarPanel(
                 h4("Summary of Interest"),
                 checkboxInput("mean", "Mean", value = TRUE),
                 checkboxInput("median", "Median", value = TRUE),
                 checkboxInput("q1", "Q1", value = TRUE),
                 checkboxInput("q3", "Q3", value = TRUE),
                 checkboxInput("min", "Minimum", value = TRUE),
                 checkboxInput("max", "Maximum", value = TRUE)
                 ),
                 
               mainPanel(
                 dataTableOutput("numeric_summary")
                 )
               ),
      
      tabPanel("Character Summary",
               h4(style = "color: blue; font-size: 20px;","The contingency table is for character variables after filtering by CAEC."),
               hr(),
               sidebarPanel(
                 h4("Character Summary"),
                 selectInput("cha1", "X",  choices = names(df)[not_numeric]),
                 selectInput("cha2", "Y",  choices = names(df)[not_numeric])
               ),
               
               mainPanel(
                 dataTableOutput("table")
            )
      ),
      
      tabPanel("Correlation", 
               h4(style = "color: blue; font-size: 20px;", "Correlation matrix is for numeric variables aftering filtering by CAEC"),
               hr(),
               dataTableOutput("correlation")
               ), 
        )
  ),
 
 #######################################
 # Modeling info sub-tab content
  
      tabItem("hiddenmodeling", " "),
      tabItem(tabName='modeling_info',
              h1("Modeling Info"),
              tabPanel("Modeling Info", 
                       fluidRow(
                         # add in latex functionality if needed
                         withMathJax(),
                         # two columns for each of the two items
                         column(6,
                                #description of modeling
                                h1("Generalized Linear Regression Model"),
                                #box to contain description
                                box(background = "light-blue", width = 12,
                                    h4("What is a generalized linear model and what are its advantages and limitations?"),
                                    h4("Generalized Linear models (GLMs) are a class of models that are an extension of linear regression models. They are used to model the relationship between a response variable and one or more predictor variables. GLMs are useful when the response variable is not normally distributed or when the variance of the response variable is not constant."),
                                    h4(
                                      HTML(paste(
                                        "<b>Advantages of GLMs:</b> handle a wide range of response distributions, model non-linear relationships, accommodate missing data, support variable selection and regularization.<br>",
                                        "<b>Limitations of GLMs:</b> sensitivity to outliers, difficulty in interpretation, and computational intensity for large datasets.<br>",
                                        "Mathematically, a GLM is defined as:<br>", "Y_i ~ Distribution(ui, ø) <br>",
                                        "Where Yi is the response variable for the i-th observation, ui is the mean of the distribution, and ø donates the dispersion parameter.")
                                         )
                                      ),
                                      
                                    h4("")
                                )
                         ),
                         
                         column(6,
                                # How to use the app
                                h1("Random Forest"),
                                # box to contain description
                                box(background = "light-blue", width = 12,
                                    h4("What is a random forest and what are its pros and cons?"),
                                    h4("A random forest regression model is a versatile machine learning algorithm used for both regression and classification tasks. It is an ensemble technique that uses multiple decision trees to make predictions. In this model, each decision tree is trained on a different subset of the data, and the final output is the average of all the outputs of the individual decision trees. This technique helps to reduce overfitting and improve the accuracy of the model. Random forest can predict continuous values, such as stock prices, temperature, or sales figures, as well as performing classification variables."),
                                    h4(
                                      HTML(paste(
                                        "<b>Advantages of Random Forests:</b> ensemble learning, improved accuracy, robustness to outliers, identifying the most important predictors, and enhancing model interpreting ability.<br>",
                                        "<b>Limitations of Random Forest:</b> interpretation challenges, performance slowdown on large datasets, and challenges against advanced boosting algorithms.<br>",
                                        "The prediction of the random forest model is given by:<br>", "y^ = 1/B∑T_i(X) <br>", "Here, B is the number of trees in the forest, Ti(X) represents the prediction of frome the i-th tree."
                                                )
                                           )
                                      ),
                                    h4("")
                                )
                         ) 
                    )       
              )
      ),

 ################################## 
      #Model fitting sub-tab
 
 tabItem(tabName = "model_fitting",
         fluidRow(
           sidebarLayout(
             sidebarPanel(
               sliderInput("split_percentage", "Choose Train/Test Split Percentage", value = 0.7, min = 0.1, max = 0.9, step = 0.1),
               #Select Model type,
               selectInput("model_type", "Select Model Type", choices = c("Multinomial Regression", "Random Forest")),
               conditionalPanel(condition = "input.model_type == 'Multinomial Regression'",
                                selectInput("mr_predictors", "Select Multinomial Regression Predictors",
                                            choices = setdiff(colnames(obesity), "NObeyesdad"), multiple = TRUE)
               ),
               conditionalPanel(
                 condition = "input.model_type == 'Random Forest'",
                 selectInput("rf_predictors", "Select Random Forest Predictors",
                             choices = setdiff(colnames(obesity), "NObeyesdad"), multiple = TRUE),
                 sliderInput("rf_cv", "Select Random Forest Cross Validation Number (default = 5)", min = 2, max = 10, value = 5)
               ),
               # Add action button
               actionButton("fit_models", "Fit Models")
             ),
             
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Multinomial Regression", 
                          verbatimTextOutput("model_summary_mr"),
                          textOutput("comparison_stats_mr")),
                 tabPanel("Random Forest", 
                          plotOutput("var_importance"),
                          textOutput("comparison_stats_rf"),
                          dataTableOutput("rf_fit_results")
                 )
               )
             )
           )
         )
 ),
###############################################
 #Prediction start here
 
 tabItem(tabName = "prediction",
         tabPanel("Obesity Prediction", 
                  sidebarLayout(
                    sidebarPanel(
                      h3("Input Values of Predictors"),
                      selectInput("model_type", "Select Model", choices = c("Multinomial Regression", "Random Forest")),
                      sliderInput("Age", "Select Age:", min = 14, max = 80, value = 30),
                      sliderInput("Height", "Select Height:", min = 1.4, max = 2.0, value = 1.70),
                      sliderInput("Weight", "Select Weight:", min = 39, max = 180, value = 65),
                      selectInput("Gender", "Select Gender:", choices = list("Female","Male"), multiple = FALSE),
                      selectInput("FCVC", "Select FCVC:", choices = list("1", "2", "3"), multiple = FALSE),
                      selectInput("NCP", "Select NCP:", choices = list("1", "2", "3", "4"), multiple = FALSE),
                      selectInput("FAVC", "Select FAVC:", choices = list("no","yes"), multiple = FALSE),
                      selectInput("CH2O", "Select CH2O:", choices = list("1", "2", "3"), multiple = FALSE),
                      selectInput("SMOKE", "Select SMOKE:", choices = list("no","yes"), multiple = FALSE),
                      selectInput("CAEC", "Select CAEC:", choices = list("no", "Sometimes", "Frequently", "Always"), multiple = FALSE),
                      selectInput("SCC", "Select SCC:", choices = list("no","yes"), multiple = FALSE),
                      selectInput("FAF", "Select FAF:", choices = list("0", "1", "2", "3"), multiple = FALSE),
                      selectInput("TUE","Select TUE:", choices = list("0", "1", "2"), multiple = FALSE),
                      selectInput("CALC","Select CALC:",choices = list("no", "Sometimes", "Frequently", "Always"), multiple = FALSE),
                      selectInput("MTRANS","Select MTRANS:",choices = list("Walking", "Public_Transportation", 
                                                                           "Automobile", "Bike", "Motobike"), multiple = FALSE
                      ),
                      selectInput("family_history_with_overweight", "Select family_history_with_overweight:", choices = list("no", "yes"), multiple = FALSE),
                      selectInput("mr_pred_pred", "Selected Predictors for Multinomial Regression", choices = NULL, multiple = TRUE),
                      selectInput("rf_pred_pred", "Selected Predictors for Random Forest", choices = NULL, multiple = TRUE),
                      actionButton("predict_button", "Get Predictions")
                    ),
                    
                    # Show the generated prediction value
                    mainPanel(
                      h3("The predicted probability for different levels of obesity from Multinomial Regression"),
                      br(),
                      dataTableOutput("prediction_mr"),
                      br(),
                      h3("The predicted probability for different levels of obesity from Random Forest Regression"),
                      dataTableOutput("probabilities_df")
                    )
                  )
         )
      )
    )
  )
)


###########################################
# Define server logic ----

server <- function(input, output, session) {
  # add a photo
  output$logo <- renderImage({
    list(src = "../Obesity.jpg",
         contentType = "image/jpeg", width = 600, height = 400,
         alt = "Logo")
  }, deleteFile = FALSE)
  
  # filter the data based on CAEC (Consumption of food between meals) 
  getData <- reactive({
    caec <- input$CAEC
    newData <- obesity %>% filter(CAEC == input$caec) 
    newData
  })
  
  # get filtered data
  df <- reactive({
    getData()
    })
  
  # Output: Correlation Matrix
  output$correlation <- renderDataTable({
    
    # get filtered data
    df <- getData()
    
    if (!is.null(df)) {
      correlation_matrix <- cor(df[, sapply(df, is.numeric)], use = "complete.obs")
      rounded_correlation_matrix <- round(correlation_matrix, 2)
      data.frame(rounded_correlation_matrix)
    }
  })
  
# Define a function to summarize numeric variables
  summarise_numeric_stats <- function(data) {
    numeric_columns <- data[, sapply(data, is.numeric)]
    selected_stats <- c(
      Mean = input$mean,
      Median = input$median,
      Q1 = input$q1,
      Q3 = input$q3,
      Min = input$min,
      Max = input$max
    )
    
    summary_data <- apply(numeric_columns, 2, function(x) {
      stats <- numeric()
      if (selected_stats["Min"]) stats <- c(stats, Min = round(min(x), 2))
      if (selected_stats["Q1"]) stats <- c(stats, Q1 = round(quantile(x, 0.25), 2))
      if (selected_stats["Median"]) stats <- c(stats, Median = round(median(x), 2))
      if (selected_stats["Mean"]) stats <- c(stats, Mean = round(mean(x), 2))
      if (selected_stats["Q3"]) stats <- c(stats, Q3 = round(quantile(x, 0.75), 2))
      if (selected_stats["Max"]) stats <- c(stats, Max = round(max(x), 2))
      stats
    })
    
    result_df <- as.data.frame(t(summary_data))
    # Remove decimal part and "%" sign from column names
    colnames(result_df) <- gsub("\\.\\d+", "", colnames(result_df))
    colnames(result_df) <- gsub("%", "", colnames(result_df))
    result_df
  }
  
  # Output: Numeric Summary Table
  output$numeric_summary <- renderDataTable({
    df <- getData()

    if (!is.null(df)) {
      summarise_numeric_stats(df[, sapply(df, is.numeric)])
    } else {
      data.frame()  # Return an empty data frame if conditions are not met
    }
  }, options = list(dom = 't'))  # Set options for DT

  # Output of Two-way contingency table
  output$table <- renderDataTable({
    # get filtered data
    df_filtered <- df()
    
        # Two-way contingency table
        contingency_table <- table(df_filtered[, input$cha1], df_filtered[, input$cha2])
        
        # Convert the contingency table to a data frame for rendering
        contingency_df <- as.data.frame.matrix(contingency_table)
        contingency_df
  })
  

  observe({
    
    # get filtered data
    df <- getData()
    
    # Update choices for x and y based on plot type
    if (input$plotType == "Scatter Plot") {
      updateSelectInput(session, "x", choices = names(df)[sapply(df, is.numeric)], selected = input$x)
      updateSelectInput(session, "y", choices = names(df)[sapply(df, is.numeric)], selected = input$y)
    } else if (input$plotType == "Boxplot") {
      updateSelectInput(session, "x", choices = names(df)[!sapply(df, is.numeric)])
      updateSelectInput(session, "y", choices = names(df)[sapply(df, is.numeric)])
    } else if (input$plotType == "Bar Plot") {
      updateSelectInput(session, "x", choices = names(df)[!sapply(df, is.numeric)])
      updateSelectInput(session, "y", choices = "None")
    } else if (input$plotType == "Histogram") {
      updateSelectInput(session, "x", choices = names(df)[sapply(df, is.numeric)])
      updateSelectInput(session, "y", choices = "None")
    }
  })
  
  # Create plot based on plot type
  output$plot <- renderPlot({
    # get filtered data
    df <- getData()
    
    if (input$plotType == "Scatter Plot") {
      # Scatter plot code
      p <- ggplot(df, aes_string(x = input$x, y = input$y))
      p <- p + geom_point(alpha = 0.5, size = 5)  # Adjust the size parameter as needed
      if (input$smooth)
        p <- p + geom_smooth(method = "lm", se = TRUE)
      if (input$color != "None")
        p <- p + aes_string(color = input$color) 
    } else if (input$plotType == "Boxplot") {
      # Boxplot code
      p <- ggplot(df, aes_string(x = input$x, y = input$y)) + geom_boxplot()
      if (input$color != "None")
        p <- p + aes_string(fill = input$color) 
    } else if (input$plotType == "Bar Plot") {
      # Bar plot code
      if (input$y == "None") {
        p <- ggplot(df, aes_string(x = input$x)) + geom_bar()
        if (input$color != "None")
          p <- p + aes_string(fill = input$color)
      } else {
        p <- ggplot(df, aes_string(x = input$x, fill = input$y)) + 
          geom_bar(position = "dodge") 
      }
    } else if (input$plotType == "Histogram" && input$color != "None") {
      
      # Histogram code with dynamic color (fill)
      p <- ggplot(df, aes_string(x = input$x, fill = input$color)) + 
        geom_histogram(position = "identity", alpha = 0.5)
    } else {
      p <- ggplot(df, aes_string(x = input$x)) + 
        geom_histogram(position = "identity", alpha = 0.5)
    }
    
    
    # Add title
    if (input$y != "None") {
      p <- p + labs(title = paste(input$y, "vs.", input$x))
    } else {
      p <- p + labs(title = paste("Distribution of", input$x))
    }
    
    # Add styling
    p <- p +
      theme_bw() +
      theme(plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
            axis.title = element_text(size = rel(1.5)),
            legend.text = element_text(size = 15))  # Adjust font size of legend as needed)
    
    print(p)
    
  }, height = 500)

############################################
  # Modeling start here
  data <- reactive({
    raw_df
  })
  
  # Define reactive Values to store selected predictors for prediction
  selected_mr_pred <- reactiveVal(NULL)
  selected_rf_pred <- reactiveVal(NULL)
  
  observeEvent(input$fit_models, {
    # Perform test/train split
    set.seed(110)
    split_index <- createDataPartition(y = data()$NObeyesdad, p = input$split_percentage, list = FALSE)
    train_data <- data()[split_index, ]
    test_data <- data()[-split_index, ]
    
    # Prepare control parameters for train()
    ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
    
    if (input$model_type == "Multinomial Regression") {#%in% input$model_type
      
      # Fit multinomial regression model
      mr_fit <- train(NObeyesdad ~ ., 
                      data = train_data[, c("NObeyesdad", input$mr_predictors)], 
                      method = "multinom",
                      preProcess = c("center", "scale"), 
                      family = "multinomial",
                      summaryFunction = multiClassSummary, # For multinomial 
                      metric = "Accuracy",
                      trace = FALSE,
                      trControl = ctrl
      )
      
      saveRDS(mr_fit, "mr_model.RDS")
      
      # Access the best-tuned parameters
      best_tune <- mr_fit$bestTune
      
      output$model_summary_mr <- renderPrint(
        {
          # Display results of print(mr_fit) and best-tuned parameters
          cat("Results of print(mr_fit):\n")
          print(mr_fit)
          
          cat("\nBest-Tuned Parameters:\n")
          print(best_tune)
        }
      )
      
      # Model comparison on the test set
      predictions <- predict(mr_fit, newdata = test_data, type = "prob")
      predictions_numeric <- as.numeric(predictions[, "Obesity_Type_III"])
      
      # Store selected predictors for multinomial regression model
      selected_mr_pred(input$mr_predictors)
      
      # Reset selected predictors for random forest
      selected_rf_pred(NULL)
      
    } else if (input$model_type == "Random Forest") {
      # Fit random forest model
      if (!is.null(input$rf_predictors) && length(input$rf_predictors) > 0) {
        set.seed(123)  # Set seed for reproducibility
        
        # Ensure valid column names
        valid_columns <- intersect(input$rf_predictors, colnames(train_data))
        
        if (length(valid_columns) == 0) {
          # Handle the case where no valid predictors are selected for the random forest
          output$rf_var_importance <- renderPlot({
            ggplot() + ggtitle("No valid predictors selected for Random Forest")
          })
          return()
        }
        
        rf_fit <- train(NObeyesdad ~ .,
                        data = train_data[, c("NObeyesdad", valid_columns)],
                        method = "rf",
                        trControl = trainControl(method = "repeatedcv",
                                                 number = input$rf_cv,
                                                 repeats = 3,
                                                 savePredictions = TRUE,
                                                 classProbs = TRUE,
                                                 summaryFunction = multiClassSummary,
                                                 # Add any other parameters you need for tuning
                        ),
                        preProcess = c("center", "scale"),
                        tuneGrid = data.frame(mtry = 1:(ncol(train_data[, valid_columns]) - 1)),
                        importance = TRUE,
                        allowParallel = FALSE  # Add this line to turn off parallel processing
        )
        
        saveRDS(rf_fit, "Random_Forest.RDS") # Save random forest fit to a RDS file
        
        # Extract mtry, Accuracy values
        rf_results <- as.data.frame(rf_fit$results[, c("mtry", "Accuracy", "Kappa")])
        
        # Put the results in a table and round to 2 decimals
        output$rf_fit_results <- renderDataTable({
          round(rf_results, 2)
        })
        # Plot the variable importance
        output$rf_var_importance <- renderPlot({
          varImp(rf_fit)
        })
      } else {
        # Handle no predictors are selected for the random forest
        output$rf_var_importance <- renderPlot({
          ggplot() + ggtitle("No predictors selected for Random Forest")
        })
      }
      
      # Variable Importance Plot
      output$var_importance <- renderPlot({
        varImpPlot(rf_fit$finalModel)
      })
      
      # Model comparison on test set
      predictions <- predict(rf_fit, newdata = test_data)
      predictions_numeric <- as.numeric(predictions)
      
      # Check for NAs or non-numeric values in predictions_numeric
      if (any(is.na(predictions_numeric)) || any(!is.finite(predictions_numeric))) {
        
        # Handle the case where predictions contain NAs or non-numeric values
        output$comparison_stats_rf <- renderText({
          "Error: Predictions contain NAs or non-numeric values"
        })
      } else {
        # Output Accuracy value as text
        output$comparison_stats_rf <- renderText({
          paste("Accuracy:", round(rf_fit$results$Accuracy[which.max(rf_fit$results$Accuracy)], 4)
          )
        })
      }
      # Store selected predictors for random forest model
      selected_rf_pred(input$rf_predictors)
      # Reset selected predictors for multinomial regression
      selected_mr_pred(NULL) 
    }
  }
  )
  
  ######################################
  # Prediction 
  # UI: Update selectInput choices based on selected model type for prediction
  observe({
    if (input$model_type == "Multinomial Regression") {
      updateSelectInput(
        session,
        "mr_pred_pred",
        choices = setdiff(colnames(raw_df), "NObeyesdad"), 
        selected = selected_mr_pred()
      )
    } else if (input$model_type == "Random Forest") {
      updateSelectInput(
        session,
        "rf_pred_pred",
        choices = setdiff(colnames(raw_df), "NObeyesdad"), 
        selected = selected_rf_pred()
      )
    }
  })
  
  
  # Read in the input values, and store them as a dataframe. 
  input_df <- reactive({
    data.frame(Gender = input$Gender,
               Age = input$Age,
               Height = input$Height,
               FAVC = input$FAVC,
               FCVC = input$FCVC,
               NCP = input$NCP,
               CAEC = input$CAEC,
               SMOKE = input$SMOKE,
               CH2O = input$CH2O,
               SCC = input$SCC,
               FAF = input$FAF,
               TUE = input$TUE,
               Weight = input$Weight,
               CALC = input$CALC,
               MTRANS = input$MTRANS,
               family_history_with_overweight = input$family_history_with_overweight
    )
  })
  
  # Use thisJava Script function to highlight selected predictors
  shinyjs::runjs(
    "shinyjs.highlightPredictors = function(selector) {
    $(selector).css({'border': '2px solid red'});
  };"
  )
  
  # Use this JavaScript function to remove highlighting
  shinyjs::runjs(
    "shinyjs.removeHighlight = function() {
    $('input, select, textarea').css({'border': ''});
  };"
  )
  
  observeEvent(input$predict_button, {
    
    if (input$model_type == "Multinomial Regression") {
      # read in model
      model_mr <- readRDS("mr_model.RDS")
      
      pred_prob <- reactiveVal(NULL)
      level_names <- reactiveVal(NULL)
      
      # Make predictions
      observe({
        pred_prob_val <- as.numeric(predict(model_mr, input_df(), type = "prob"))
        # Get the levels of the response variable
        level_names_val <- levels(factor(data()$NObeyesdad))  
        pred_prob(pred_prob_val)
        level_names(level_names_val)
      })
      
      
      # Create a data frame with probabilities
      output$prediction_mr <- renderDataTable({
        # Check if pred_prob and level_names are not NULL before creating the data frame
        if (!is.null(pred_prob()) && !is.null(level_names())) {
          # Combine level names and predicted probabilities into a data frame
          probabilities_df <- data.frame(Level = factor(level_names(), 
                                                        levels = level_names()
          ), Probability = as.numeric(pred_prob())
          )
          probabilities_df}
      })
      
    } else if (input$model_type == "Random Forest") {
      
      # read in model
      model_rf <- readRDS("Random_Forest.RDS")
      
      pred_prob <- reactiveVal(NULL)
      level_names <- reactiveVal(NULL)
      
      # Make predictions
      observe({
        pred_prob_val <- as.numeric(predict(model_rf, input_df(), type = "prob"))
        # Get the levels of the response variable
        level_names_val <- levels(factor(data()$NObeyesdad))  
        pred_prob(pred_prob_val)
        level_names(level_names_val)
      })
      
      
      # Create a data frame with probabilities
      output$probabilities_df <- renderDataTable({
        # Check if pred_prob and level_names are not NULL before creating the data frame
        if (!is.null(pred_prob()) && !is.null(level_names())) {
          # Combine level names and predicted probabilities into a data frame
          probabilities_df <- data.frame(Level = factor(level_names(), 
                                                        levels = level_names()
          ), Probability = as.numeric(pred_prob()
          )
          )
          probabilities_df
          
        }
      })
    } 
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

