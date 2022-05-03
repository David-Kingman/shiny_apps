## Todos:
# - Check code for presentation, redundancy and comments
# - Work out how to size objects to look their best on larger screens
# - Look at making app public on shinyapps.io

## Set-up
library(nycflights13)
library(gapminder)
library(ISLR2)
library(tidyverse)
library(shiny)
library(bslib)
library(thematic)
library(scales)
library(DT)
library(skimr)
library(FNN)
library(splines)
library(rpart)
library(rpart.plot)
library(showtext)
options(scipen = 1000)

## 1. Create datasets

# nycflights13::weather - Dew point vs Temperature
temp_and_dewpoint <-
  nycflights13::weather %>% 
  filter(!is.na(temp)) %>% 
  select(dew_point = dewp,
         temperature = temp) %>% 
  sample_n(size = 3000, replace = TRUE)

# Simulated non-linear data (sin function)
sin_data <- 
  tibble(X = runif(n = 3000, min = 0, max = 13),
         rand = rnorm(n = 3000, sd = 0.25),
         Y = (sin(X) + rand) - (X * 0.25))  %>% 
  mutate(across(everything(), ~ .x * 10)) %>% 
  select(Y, X)

# gapminder::gapminder - Life expectancy vs GDP per capita
gdp_and_life_expectancy <-
  gapminder::gapminder %>% 
  filter(gdpPercap < 50000) %>%
  select(life_expectancy = lifeExp,
         gross_domestic_product_per_capita = gdpPercap)

# ggplot2::diamonds - Price vs Carat
price_and_carat <- 
  diamonds %>% 
  select(price, carat) %>% 
  sample_n(size = 3000, replace = TRUE)

# ISLR2::Wage - Log wage vs Age
age_and_log_wage <- 
  Wage %>% 
  as_tibble() %>% 
  select(log_wage = logwage,
         age)

datasets <- c('Dew point vs temperature' = 'temp_and_dewpoint',
              'Sin function' = 'sin_data',
              'Life expectancy vs GDP' = 'gdp_and_life_expectancy',
              'Price vs carats' = 'price_and_carat',
              'Log wage vs age' = 'age_and_log_wage')

## 2. Create UI
thematic_shiny()
ui <- navbarPage(id = 'navbar', theme = bslib::bs_theme(bootswatch = 'superhero'),
                 
                 # App title
                 title = 'Visual machine learning',
                 
                 # 'Choose dataset' panel
                 tabPanel(title = '1. Choose dataset',
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = 'select_data',
                                          label = strong('Select a dataset:'),
                                          choices = datasets,
                                          selected = datasets[[1]]),
                              sliderInput(inputId = 'train_split',
                                          label = strong('Choose the % of this dataset you will use to train your model:'),
                                          min = 0,
                                          max = 100,
                                          value = 60,
                                          step = 1,
                                          ticks = FALSE,
                                          post = '%'),
                              textOutput('traintext'),
                              textOutput('testtext')
                              ),
                            mainPanel(
                              plotOutput(outputId = 'select_data_plot'),
                              HTML('</p>'),
                              tableOutput(outputId = 'data_info')
                              )
                            )
                          ),
                 
                 # 'Train model' panel
                 tabPanel(title = '2. Train model',
                          # Fixed controls
                          fluidRow(
                            column(4,
                                   wellPanel(
                                     uiOutput('select_model'),
                                     uiOutput('model_params'),
                                     HTML('</p>'),
                                     actionButton(inputId = 'test_button', label = 'Test model'),
                                     HTML('</p>'),
                                     actionButton(inputId = 'resample_button', label = 'Resample data')
                                     )
                                   ),
                            column(8,
                                   strong('Performance on training data:'),
                                   plotOutput('training_model_plot', click = 'plot_click'),
                                   textOutput('train_rmse_naive'),
                                   textOutput('train_rmse_model'),
                                   HTML('</p>'),
                                   htmlOutput('model_info'),
                                   plotOutput('model_plot')
                                   )
                            )
                          ),
                 
                 # 'Leaderboard' panel
                 tabPanel(title = '3. Model leaderboard',
                          fluidRow(
                            column(12, DT::dataTableOutput('leaderboard'))
                            )
                          )
)

## 3. Server function
server <- function(input, output) {
  
  
  # Create functions
      graph <- function(x) {
        
        y_var <- colnames(x)[1]
        x_var <- colnames(x)[2]
        y_mean <- round(mean(x[[1]]), 2)
        
        x %>%
          ggplot() +
          geom_point(aes_string(x = x_var, y = y_var), alpha = 0.5, shape = 21, fill = 'red') +
          (if (input$navbar == '2. Train model') geom_hline(yintercept = y_mean, linetype = 'dotted', color = 'white', alpha = 0.5, size = 1)) +
          (if ('pred_y' %in% names(x)) geom_line(aes_string(x = x_var, y = 'pred_y', group = 1), color = 'black', size = 1.5)) +
          labs(x = paste0('\n', str_to_title(str_replace_all(x_var, '_', ' '))),
               y = paste0(str_to_title(str_replace_all(y_var, '_', ' '), '\n')))
        
      }
      
      rmse <- function(data, model = NULL, naive = TRUE) {
        
        y_var <- colnames(data)[1]
        
        if (naive == TRUE) {
          
          data <- mutate(data, pred_y = mean(.data[[y_var]]))
          
        } else {
          
          data$pred_y <- predict(model, data)
          
        }
        
        data %>%
          mutate(residual = .data[[y_var]] - pred_y) %>% 
          summarise(sum_of_squares = sum(residual^2),
                    rmse = sqrt(sum_of_squares / nrow(data))) %>%
          mutate(rmse = round(rmse, 2)) %>%
          pull(rmse)
        
      }
      
      # Partition dataset (and allow the user to resample the data by clicking the 'Resample' button or changing the slider)
      select_data <- reactive({ get(input$select_data) })
      react_list <- reactiveValues()
      models <- reactiveValues()
      current_model <- reactiveVal()
      
      observeEvent(c(input$train_split, input$resample_button), {
        
        react_list$train_rows <- 
          select_data() %>%
          mutate(row = row_number()) %>% 
          sample_frac(size = (input$train_split / 100), replace = FALSE) %>% 
          pull(row)
        
      })
      
      training_data <- reactive({
        select_data() %>% 
          filter(row_number() %in% react_list$train_rows)
      })
      
      test_data <- reactive({
        select_data() %>% 
          filter(!row_number() %in% react_list$train_rows)
      })
      
      output$traintext <- renderText({str_glue('Training observations: {scales::comma(nrow(training_data()))}')})
      output$testtext <- renderText({str_glue('Test observations: {scales::comma(nrow(test_data()))}')})
      
      # '1. Choose dataset' panel outputs  
      output$select_data_plot <- renderPlot({ graph(select_data()) })
      output$data_info <- renderTable({ 
        
        select_data() %>% 
          skim() %>% 
          as_tibble() %>% 
          select(name = skim_variable,
                 numeric.mean:numeric.p100) %>% 
          rename_with(~str_remove(., 'numeric.'), contains('numeric')) 
        
        })
      
      observeEvent(c(input$navbar), {
        
        current_model(NULL)
        
        output$select_model <- renderUI({
          
          radioButtons(inputId = 'model',
                       label = strong('Choose a model:'),
                       choices = c('None',
                                   'Linear',
                                   'GAM',
                                   'KNN',
                                   'Decision Tree'),
                       selected = 'None')
          })
        })
      
      
      # Hyperparameter controls
      observe({

        req(input$model)

        if (input$model == 'None') {

          output$model_params <- NULL

        } else if (input$model == 'Linear') {

          output$model_params <- renderUI({

            radioButtons(inputId = 'poly',
                         label = strong('Highest degree term for polynomial regression:'),
                         choices = c('Linear' = '1',
                                     'Quadratic' = '2',
                                     'Cubic' = '3'),
                        selected = 1)

            })

          } else if (input$model == 'GAM') {

            output$model_params <- renderUI({

           tagList(
             sliderInput(inputId = 'knots',
                       label = strong('Number of knots:'),
                       value = 1,
                       min = 1,
                       max = 50,
                       step = 1),

             numericInput(inputId = 'degree',
                          label = strong('Highest degree polynomial:'),
                          value = 1,
                          min = 1,
                          max = 3,
                          step = 1)
           )

         })

       } else if (input$model == 'KNN') {

         output$model_params <- renderUI({

           sliderInput(inputId = 'k',
                       label = strong('Value of k:'),
                       min = 1,
                       max = nrow(training_data()) - 1,
                       value = nrow(training_data()) - 1)

         })

       } else if (input$model == 'Decision Tree') {

         output$model_params <- renderUI({

           tagList(
             sliderInput(inputId = 'min_leaf_size',
                         label = strong('Minimum number of observations in each leaf:'),
                         min = 1,
                         max = 500,
                         value = 1),

             sliderInput(inputId = 'max_splits',
                         label = strong('Maximum number of tree branches:'),
                         min = 0,
                         max = 25,
                         value = 0)
             )

         })
         }
        })
      
      
      
      # '2. Fit models' panel outputs
      observe({
        
        req(input$model)
        output$train_rmse_naive <- renderText({ str_glue('Naive model RMSE: {rmse(training_data(), model = NULL, naive = TRUE)}') })
        
        ## No model
        if (input$model == 'None') {
          
          # Plot training data with naive model only
          output$training_model_plot <- renderPlot({ graph(training_data()) })
          
          # No hyperparameters, model info or model plot
          output$model_params <- NULL
          output$model_info <- NULL
          output$model_plot <- NULL
          
        ## Linear models
        } else if (input$model == 'Linear') {
          
          # Fit model
          req(input$poly)
          linear_formula <-
            reactive({ as.formula(paste(colnames(training_data())[1], '~ poly(', colnames(training_data())[2], ',', as.numeric(input$poly), ')')) })
          linear_model <- reactive({ lm(linear_formula(), data = training_data()) })
          
          # Training set
          output$training_model_plot <- renderPlot({
            
            training_data() %>%
              bind_cols(pred_y = linear_model()[['fitted.values']]) %>%
              graph() 
            
          })
          
          models$train_rmse <- rmse(training_data(), model = linear_model(), naive = FALSE)
          output$train_rmse_model <- renderText({ str_glue('Training RMSE: {models$train_rmse}') })
          current_model(linear_model())
          
          # Training model info
          output$model_info <- NULL
          output$model_plot <- NULL
        
        ## Generalized Additive Models
        } else if (input$model == 'GAM') {
          
          # Fit model
          req(input$knots)
          req(input$degree)
          
          gam_formula <-
            reactive({
              as.formula(
                paste(colnames(training_data())[1], '~ splines::bs(', colnames(training_data())[2], ',', input$knots, ', degree =', input$degree, ')')
              )
            })
          gam_model <- reactive({ lm(gam_formula(), data = training_data()) })
          
          # Training set
          output$training_model_plot <- renderPlot({
            
            training_data() %>%
              bind_cols(pred_y = gam_model()[['fitted.values']]) %>%
              graph() 
            
          })
          
          models$train_rmse <- rmse(training_data(), model = gam_model(), naive = FALSE)
          output$train_rmse_model <- renderText({ str_glue('Training RMSE: {models$train_rmse}') })
          current_model(gam_model())
          
          # Training model info
          output$model_info <- renderText({
          '<strong>Explanation:</strong>
          <br>
          
          <p>Generalized Additive Models (GAMs) extend linear models by fitting a series of piecewise continuous polynomials to the data which are smoothly connected 
          at a series of fixed points called <em>knots</em>. By adjusting the number of knots (which are usually located at specified quantiles within the data) and the
          degree of these polynomials you can enable the GAM to model increasingly non-linear relationships.<p>
            
          <strong>The plot below visually displays the locations of the knots within the training data:</strong>'
          })
          
          output$model_plot <- renderPlot({
            
            training_data() %>%
              bind_cols(pred_y = gam_model()[['fitted.values']]) %>%
              graph() +
              geom_vline(xintercept = quantile(training_data()[[2]], probs = seq(0, 1, by = (1 / input$knots))), linetype = 'dashed', color = 'yellow', alpha = 0.5)
            
            })
          
        ## K-nearest neighbors
        } else if (input$model == 'KNN') {
          
          # Training set
          req(input$k)
          knn_model_train <- reactive({ FNN::knn.reg(train = training_data()[[2]], y = training_data()[[1]], k = input$k) })
          
          nearest_rows <- reactive({ 
            
            req(input$plot_click)
            
            nearest_row_index <- FNN::knnx.index(data = training_data()[[2]], query = input$plot_click[['x']], k = input$k)[1, ]
            
            training_data() %>% 
              filter(row_number() %in% nearest_row_index)
            
            })
          
          output$training_model_plot <- renderPlot({
            
            training_data() %>%
              bind_cols(pred_y = knn_model_train()[['pred']]) %>%
              graph() +
              (if (!is.null(input$plot_click)) geom_point(data = nearest_rows(),
                                                          aes(x = .data[[names(nearest_rows())[2]]],
                                                              y = .data[[names(nearest_rows())[1]]]),
                                                          color = 'yellow'))
            
          })
          
          knn_train_rmse <- reactive({
            
            training_data() %>%
              bind_cols(pred_y = knn_model_train()[['pred']]) %>%
              mutate(residual = .[[1]] - pred_y) %>%
              summarise(sum_of_squares = sum(residual^2),
                        rmse = sqrt(sum_of_squares / nrow(.))) %>%
              mutate(rmse = round(rmse, 2)) %>%
              pull(rmse)
            
          })
          
          output$train_rmse_model <- renderText({ str_glue('Fitted model RMSE: {knn_train_rmse()}') })
          models$knn_training_rmse <- knn_train_rmse()
          
          # Fit KNN test model
          knn_test_model <- reactive({ FNN::knn.reg(train = training_data()[, 1:2], test = test_data(), y = training_data()[[1]], k = input$k) })
          
          knn_test_rmse <- reactive({
            
            test_data() %>%
              bind_cols(pred_y = knn_test_model()[['pred']]) %>%
              mutate(residual = .[[1]] - pred_y) %>%
              summarise(sum_of_squares = sum(residual^2),
                        rmse = sqrt(sum_of_squares / nrow(.))) %>%
              mutate(rmse = round(rmse, 2)) %>%
              pull(rmse)
            
          })
          
        models$knn_test_model <- knn_test_model()
        models$knn_test_rmse <- knn_test_rmse()
        current_model(knn_model_train())
        
        output$model_info <- renderText({
        '<strong>Explanation:</strong>
        <br>
        <p>K-Nearest Neighbours (KNN) regression works by predicting that new observations will have the mean value of Y among the <em>k</em> nearest observations 
        within the training data (based on Euclidean distance).<p>
        
        <strong>Click on the plot to highlight the <em>k</em> nearest neighbours to each value of X.</strong>'
        })
        
        output$model_plot <- NULL
        
        ## Decision tree
        } else if (input$model == 'Decision Tree') {
          
          # Fit model
          req(input$min_leaf_size)
          req(input$max_splits)
          
          tree_formula <- reactive({ as.formula(paste(colnames(training_data())[1], '~', colnames(training_data())[2])) })
          tree_model <-
            reactive({ rpart::rpart(formula = tree_formula(),
                                    data = training_data(),
                                    method = 'anova',
                                    control = list(minsplit = 2,
                                                   minbucket = input$min_leaf_size,
                                                   maxdepth = input$max_splits,
                                                   cp = 0,
                                                   xval = 0)) })
          
          # Training set
          output$training_model_plot <- renderPlot({
            
            training_data() %>%
              bind_cols(pred_y = predict(tree_model(), .)) %>%
              graph()
            
          })
          
          models$train_rmse <- rmse(training_data(), model = tree_model(), naive = FALSE)
          output$train_rmse_model <- renderText({ str_glue('Training RMSE: {models$train_rmse}') })
          current_model(tree_model())
          
          # Training model info
          output$model_info <- renderText({
          "<strong>Explanation:</strong>
          <br>
          <p>Decision trees use a <em>recursive partitioning algorithm</em> to find the split points within the data which result in the greatest improvement in the 
          accuracy of the model's predictions (measured using a cost function such as the RMSE). These split points are used to define <em>branches</em> which terminate
          in nodes called <em>leaves</em>. In regression problems, each new observation is predicted the mean value of the observations within the training data
          which fell into the same leaf as the new observation after applying the decision tree's rules, resulting in a step function. The number of splits 
          (<em>tree depth</em>) and the minimum number of observations which is allowed to fall into each leaf are hyperparameters which are used to control the 
          complexity of the tree.<p>
          
          <p>Individual decision trees tend to exhibit poor real-world performance because they have low bias but high variance, but this is often greatly improved by 
          fitting <em>ensembles</em> consisting of many trees and averaging their results. This approach underpins widely-used machine learning algorithms like 
          bagged decision trees, random forests and gradient-boosted decision trees.<p>
          
          <strong>The decision which has been fitted on the training data is visualised below:</strong>"
          })
          
          output$model_plot <- renderPlot({ rpart.plot::rpart.plot(tree_model(),
                                                                   col = 'black',
                                                                   split.col = 'white',
                                                                   branch.col = 'white',
                                                                   nn.col = 'white') })
          
          }
        })

      # 'Test model' pop-up window
      observeEvent(input$test_button, {
        
        if (is.null(current_model())) {
          
          showModal(
            modalDialog(
              size = 'l',
              title = strong('NO MODELS HAVE BEEN TRAINED YET!')
            )
          )
          
        } else {
          
          showModal(
            modalDialog(
              size = 'l',
              title = strong('Performance on test set:'),
            
            # Plot model predictions on test data
            renderPlot({
              if (input$model == 'KNN') {
                
                test_data() %>%
                  bind_cols(pred_y = models$knn_test_model[['pred']]) %>%
                  graph()
                
                } else {
                  
                  test_data() %>%
                    bind_cols(pred_y = predict(current_model(), .)) %>%
                    graph()
                  
                  }
              }),
            
            # Print naive model RMSE on test set
            renderText({ str_glue('Naive model RMSE: {rmse(test_data(), model = NULL, naive = TRUE)}') }),
            
            # Print training set RMSE
            renderText({ if (input$model == 'KNN') {
              
              str_glue('Training set RMSE: {models$knn_training_rmse}')
              
              } else {
                
                str_glue('Training set RMSE: {rmse(training_data(), model = current_model(), naive = FALSE)}')
                
                }}),
            
            # Print test set RMSE
            renderText({ if (input$model == 'KNN') {
              
              str_glue('Test RMSE: {models$knn_test_rmse}')
              
              } else {
                
                str_glue('Test RMSE: {rmse(test_data(), model = current_model(), naive = FALSE)}')
                
                }}),
            
            # Add action buttons to footer
            footer = tagList(
              modalButton('Discard model'),
              actionButton(inputId = 'leaderboard_button',
                           label = 'Add to leaderboard')
            )
            )
          )
        }
        })
        
        
      
      # '3. Model leaderboard' panel outputs
      observeEvent(input$leaderboard_button, {
        
        params <- reactive({
          
          if (input$model == 'Linear') {
            
            str_glue('(Degree: {input$poly})')
            
            } else if (input$model == 'GAM') {
              
              str_glue('(Knots: {input$knots}) + (Degree: {input$degree})')
              
              } else if (input$model == 'KNN') {
                
                str_glue('(K: {input$k})')
                
              } else if (input$model == 'Decision Tree') {
                  
                str_glue('(Splits: {input$max_splits}) + (Min leaf: {input$min_leaf_size})')
                
              }
          
          })
        
        leaderboard_entry <- reactive({
          
          tibble(Data = names(datasets[datasets == input$select_data]),
                 Model = input$model,
                 Hyperparameters = params(),
                 'Training Data' = str_glue('{input$train_split}% ({scales::comma(nrow(training_data()))} rows)'),
                 'Test Data' = str_glue('{(100 - input$train_split)}% ({scales::comma(nrow(test_data()))} rows)'),
                 'Naive RMSE' = rmse(training_data(), model = NULL, naive = TRUE),
                 'Training RMSE' = ifelse(input$model == 'KNN', models$knn_training_rmse, models$train_rmse),
                 'Test RMSE' = ifelse(input$model == 'KNN', models$knn_test_rmse, rmse(test_data(), model = current_model(), naive = FALSE)))
          
          })
        
        if (is.null(models$leaderboard_data)) {
          
          models$leaderboard_data <- leaderboard_entry()
          
        } else {
            
          models$leaderboard_data <-
            models$leaderboard_data %>% 
            bind_rows(., leaderboard_entry()) %>% 
            arrange(`Test RMSE`)
          
          }
        
        removeModal()
        
        })
      
      output$leaderboard <- DT::renderDataTable({ models$leaderboard_data })
      
      
}

## 4. Run app
shinyApp(ui, server)
