
# ST558_Final_Project

## About this app
This is repo containing work from the final project for ST558 class.  
This project provides a straightforward Shiny application designed for exploratory data analysis and model fitting using an obesity dataset. The dataset, sourced from [kaggle](https://www.kaggle.com/datasets/aravindpcoder/obesity-or-cvd-risk-classifyregressorcluster), provides insights into estimating obesity levels in 14 to 61-year-old individuals from Mexico, Peru, and Colombia, based on their eating habits and physical conditions. The data contains 17 attributes and 2111 records, each labeled with the class variable 'NObesitydad' (Obesity Level).   

The application provides a comprehensive exploration of the obesity data, presenting visualizations through plots and tables based on user selections. Additionally, users have the option to manipulate model training, fitting, and prediction for a more interactive experience.  

## R packages used in this app  

+ [**`tidyverse`**](https://www.tidyverse.org/) An opinionated collection of R packages designed for data science.  
+ [**`caret`**](https://cran.r-project.org/web/packages/caret/) A set of functions that attempt to streamline the process for creating predictive models. 
+ [**`ggplot2`**](https://cran.r-project.org/web/packages/ggplot2/index.html) A system for 'declaratively' creating graphics.
+ [**`shiny`**](https://cran.r-project.org/web/packages/shiny/index.html) Provides an elegant and powerful web framework for building web applications using R.  
+ [**`DT`**](https://rstudio.github.io/DT/) Provides an R interface to the JavaScript library Data Tables.  
+ [**`metrics`**](https://cran.r-project.org/web/packages/Metrics/index.html)  Provides metrics for regression, time series, binary classification, classification, and information retrieval problems.  
+ [**`nnet`**](https://cran.r-project.org/web/packages/nnet/index.html) Software for feed-forward neural networks with a single hidden layer, and for multinomial log-linear models.  
+ [**`shinydashboard`**](https://cran.r-project.org/web/packages/shinydashboard/index.html) This package provides a theme on top of 'Shiny', making it easy to create attractive dashboards.  
+ [**`dplyr`**](https://cran.r-project.org/web/packages/dplyr/index.html) A fast, consistent tool for working with data frame like objects, both in memory and out of memory.
+ [**`lattice`**](https://cran.r-project.org/web/packages/lattice/index.html) A powerful and elegant high-level data visualization system.  
+ [**`shinyjs`**](https://cran.r-project.org/web/packages/shinyjs/index.html) Perform common useful JavaScript operations in Shiny apps that will greatly improve your apps without having to know any JavaScript.  
+ [**`randomForest`**](https://cran.r-project.org/web/packages/randomForest/) A package for lassification and regression based on a forest of trees using random inputs. 

## Install packages to run the app  
To run this Shiny app, you need to install the required R packages. Open your R console or RStudio and run the following command to install the necessary packages:  
```R
install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'ggplot2', 'caret', 'DT', 'metrics', 'nnet', 'dplyr', 'randomForest', 'shinyjs', 'lattice'))
```
## Run the app from GitHub
You can run this Shiny app from GitHub using this code.
```R
shiny::runGitHub("ST558_project4", "fhzhkunming", subdir = "project4-v2")

runUrl("https://github.com/fhzhkunming/ST558_project4/archive/HEAD.tar.gz",
       subdir = "project4-v")

# Run the app ----
shinyApp(ui = ui, server = server)
```
