setwd("C:/Users/Admin/Desktop/project")

#' @description these following packages allow me to use multiple features. The @usage of them are for:
#' Plotting
#' Predictive Methods
#' Aggregation
#' Builiding a frontend
#' Using & conneting to a database
#' Documentation
library(class)
library(ggplot2)
library(randomForest)
library(rpart)
library(zoo)
library(shiny)
library(DBI)
library(roxygen2)
# library(RMySQL)
library(dbConnect)
library(RMariaDB)

#' read a csv file and attach it to the dataset 
#' 
#'  @description this passes the csv file into the name of a dataset 
#'  as iowtrain. 
#'  the headers are set to true and have a sepreter placed.
#'  
#'  @usage the seperator creates a white space between columns.
iowaTrain <- read.csv("train.csv", header = TRUE, sep = ',')

#' After setting the dataset and importing the csv file, cleansing of 
#' the data must take place.
#' 
#' @description these following coklumns from the dataset are
#' being assigned the value of NULL. This essentially 
#' removes them from the columns from it.
iowaTrain$Id <- NULL
iowaTrain$Street <- NULL
iowaTrain$Alley <- NULL
iowaTrain$LotFrontage <- NULL
iowaTrain$LotConfig <- NULL
iowaTrain$MasVnrArea <- NULL
iowaTrain$BsmtExposure <- NULL
iowaTrain$BsmtFinSF1 <- NULL
iowaTrain$BsmtFinSF2 <- NULL
iowaTrain$PoolArea <- NULL
iowaTrain$GarageYrBlt <- NULL
iowaTrain$PoolArea <- NULL
iowaTrain$PoolQC <- NULL
iowaTrain$MiscFeature <- NULL
iowaTrain$X3SsnPorch <- NULL
iowaTrain$MiscVal <- NULL
iowaTrain$WoodDeckSF <- NULL
iowaTrain$OpenPorchSF <- NULL
iowaTrain$EnclosedPorch <- NULL
iowaTrain$ScreenPorch <- NULL
iowaTrain$FireplaceQu <- NULL
iowaTrain$GarageFinish <- NULL
iowaTrain$GarageCond <- NULL
iowaTrain$SaleType <- NULL
iowaTrain$SaleCondition <- NULL
iowaTrain$Fence <- NULL
iowaTrain$Utilities <-NULL
iowaTrain$LandSlope <- NULL

#' setting/changing values of the columns.
#' 
#' @description thesee columns that are part of the data set
#' are being changed into integers.
#' 
#' @usage as.integer will allow me to take factors or character strings
#' and convert them into numeric values to get effrcient predicitions.
#' 
#' the basement quality is also having all the na values aggreagated.
iowaTrain$BsmtQual <- as.integer(iowaTrain$BsmtQual)
iowaTrain$ExterQual <- as.integer(iowaTrain$ExterQual)
iowaTrain$OverallQual <- as.integer(iowaTrain$OverallQual)
iowaTrain$KitchenQual <- as.integer(iowaTrain$KitchenQual)
iowaTrain$BsmtQual <- na.aggregate(iowaTrain$BsmtQual)

write.csv(iowaTrain, file = "train1.csv", row.names = FALSE)

con <- dbConnect(RMariaDB::MariaDB(),
                 user ='root',
                 password = 'new_password',
                 dbname = 'iowa_homes',
                 host = 'localhost')
dbWriteTable(con, name = 'iowa1', value = iowaTrain)
iowa1 <- dbGetQuery(con, "SELECT * FROM iowa1 LIMIT 1500")
dbListTables(con)
#' @description attaching the caTools library for my random forest method
#' also setting the seed for a psuedo random number generator for the method
library(caTools)
set.seed(123)

#' @description these are my training and test data sets that are split 2/3 & 1/3
#' @usage they come off from the main dataset and are split to create the predtiction against one another
train_training <- iowa1[1:1095,]
train_test <- iowa1[1096:1460,]

#' @description this is a random forest formula taking into consideraation 12 variables against the saleprice
#' it derives of the data set and the number of trees it is searching through is 300
fit <- randomForest(SalePrice ~ ExterQual  + GarageArea + TotalBsmtSF + OverallQual +  BsmtQual + X1stFlrSF + KitchenQual + YearBuilt +  OverallCond + YearRemodAdd + FullBath + LotArea, data = iowa1, ntree = 300)

#' @description is the final method which is my formula and my test data taken into consideration
#' @param predict allows the fit forumla to apply to the test data and outputs the data to the function 'predicted'
predicted <- predict(fit, train_test)

#' setting the length of my predicted function and assigning the length of a new variable to 0
numberpredictions <- length(predicted)
correct <- 0L

#' @description this is a for loop to look through the length of my predicition test data through each length.
#' the @if statement mesures the predicted method against the sale price and vice versa whilst incremmenting the length of 
#' the numcorrect value.
for(i in 1:npredictions){
  if(predicted[i] > train_test$SalePrice[i] & predicted[i] < train_test$SalePrice[i]){
    correct <- correct + 1
  }
}
#' @description setting the accuracy amount through the method of dividing my new variable length 
#' along with the exisiting predicition length and multiplying it by a 100
#' @usage this will print out the percentage of accuracy the prediction model has
Accuracy <- (correct/ numberpredictions) * 100
print(Accuracy) 


#' library for the version of shiny used for the frontend
library(shinydashboard)

#' @description this is setting the scipen to 9999 and also attaching the @dashboardPage to the ui
#' this is opened up with a @dashboardHeader which has title for it, the skin color, the @dashboardSidebar
#' with two menu items. @usage these items have two different dashboard pages and are accesible through sidebar.
options(scipen = 9999)
  ui <- dashboardPage(
    dashboardHeader(
      title = "Iowa Homes Prediciton Model",
      titleWidth = 300
    ),
  skin = ("yellow"),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
    menuItem("Predict", tabName = "predict", icon = icon("home")),
    menuItem("Plot", tabName = "plot", icon = icon("chart-line"))
    )
  ),
    
  #' the @dashboardBody holds tabitems for each of the menuitems to allow different views on each, 
  #' in the @predict tabitem there are variable inputs for the formula, allowing the user to input them in accordingly.
  #' These are set in a @box within two columns which is within a fluid row this allows the formation neat and layout to be in an good fashion. 
  #' The @plot tabitem plots the graph outputs for the predictive model.
    dashboardBody(
      tabItems(
        tabItem( tabName = "predict",
          fluidRow(
            column(width = 6,
              box(
                background = "black", width = 75,
                sliderInput(inputId = "ExterQual", "External Quality:", 0, 4, 50)),
              box(
                background = "black", width = 75,
                sliderInput("BsmtQual", "Basement Quality:", 0, 4, 50)),
              box(
                background = "black", width = 75,
                selectInput(inputId = "OverallQual", label = "Overall Quality of Property:", choices = c(0:10))),
              box(
                background = "black", width = 75,
                selectInput(inputId = "YearBuilt", label = "Year Built:", choices = c(1850:2010))),
              box(
                background = "black", width = 75,
                numericInput(inputId = "X1stFlrSF", label = "1st Floor in SF:", value = 0, min = 0, step = 1, max = 6500)),
              box(
                background = "black", width = 75,
                numericInput(inputId = "GarageArea", label = "Garage Area:", value = 0, min = 0, step = 1, max = 1500)),
              submitButton(text = "Predict", icon = NULL, width = 70)
            ),
        
            column(width = 6,
              box(
                background = "black", width = 75,
                sliderInput("KitchenQual", "Kitchen Quality:", 0, 4, 50)),
              box(
                background = "black", width = 75,
                sliderInput("FullBath", "Full Bath:", 0, 3, 50)),
              box(
                background = "black", width = 75,
                selectInput(inputId = "OverallCond", label = "Overall Condition of Property:", choices = c(0:10))),
              box(
                background = "black", width = 75,
                selectInput(inputId = "YearRemodAdd", label = "Year It Was Remodelled:", choices = c(1850:2010))),
              box(
                background = "black", width = 75,
                numericInput(inputId = "TotalBsmtSF", label = "Total Basement in SF:", value = 0, min = 0, step = 1, max = 6500)),
              box(
                background = "black", width = 75,
                numericInput(inputId = "LotArea", label = "Lot Area:", value = 0, min = 0, step = 1, max = 6500)),
              textOutput(outputId = "Predicted")
            )
          )
        ),
        tabItem( tabName = "plot",
            plotOutput(outputId = "Ploty"),
            plotOutput(outputId = "Ploty1")
      )
    )
  )
)

#' @description this is a function which i had created for user input, this contains a @list of the variables available as part of the formula attached to
#' the value of its name in the training data. @test1 is a data frame which containes the list. This essentially acts as the test data in the predicted method, which takes 
#' in also the @fit formula with the @param return outputting the value of the input.
logic1 <- function(input) {
  inp <- list(  "ExterQual" = input$ExterQual,
                "GarageArea" = input$GarageArea,
                "TotalBsmtSF" = input$TotalBsmtSF,
                "OverallQual" = as.numeric(input$OverallQual),
                "BsmtQual" = input$BsmtQual,
                "X1stFlrSF" = input$X1stFlrSF,
                "KitchenQual" = input$KitchenQual,
                "YearBuilt" = as.numeric(input$YearBuilt),
                "OverallCond" = as.numeric(input$OverallCond),
                "YearRemodAdd" = as.numeric(input$YearRemodAdd),
                "FullBath" = input$FullBath,
                "LotArea" = input$LotArea)
  test1 = as.data.frame(inp)
  print(str(test1))
  fit <- randomForest(SalePrice ~ ExterQual  + GarageArea + TotalBsmtSF + OverallQual +  BsmtQual + X1stFlrSF + KitchenQual + YearBuilt +  OverallCond + YearRemodAdd + FullBath + LotArea, data = iowa1, ntree = 300)
  predicted <- predict(fit, test1)
  return(predicted)
  }

#' @usage for the server function allows both input and output to be taken in for the app.
#' The output is referenced in the both tabitems for @predict & @plot. The output for @predict also
#' pastes the input from the logic1 function above within it.
server <- function(input, output) {

  output$Predicted <- renderText({
        paste("Predicted Value: ", logic1(input))
      })
  
  output$Ploty <- renderPlot({
   plot(iowa1$SalePrice~iowa1$OverallQual)
  })
  output$Ploty1 <- renderPlot({
    hist(iowa1$SalePrice)
  })

}

#' the @ui at the top of the frontend set with the dashboard page and the @server above are equal to both the ui and server for the shinyApp ui. 
shinyApp(ui = ui, server = server)


























# train_test <- rbind(train_training[54, ] , train_test)
# train_test <- train_test[-54,]
# 
# 
# #see if there are any n/a's in the data set
# any(is.na(train_test))
# 
# #see how many n/a's in the data set
# sum(is.na(train_test))
# 
# na.cols = which(colSums(is.na(train_test)) > 0)
# sort(colSums(sapply(train_test[na.cols], is.na)), decreasing = TRUE)
# 
# train_test <- na.omit(train_test)
# 
# sum(is.na(train_test))
# 
# set.seed(1234)
# regressor = randomForest(x = iowaTrain[1],
#                          y = iowaTrain$SalePrice,
#                          ntree = 10)
# 
# x_grid = seq(min(train_training$SalePrice), max(train_training$SalePrice), 1)
# ggplot() +
#   geom_point(aes(x = iowaTrain$OverallCond, y =iowaTrain$SalePrice),
#              colour = "red") +
#   geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(train_test))),
#             colour = "blue") +
#   ggtitle("Random Forest Regression") +
#   xlab('OverallCond') +
#   ylab('SalePrice')
# 
# y_pred = predict(fit, train_test)
