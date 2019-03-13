library("shiny")
library("dplyr")
library("tidyr")
library("ggplot2")
source("our_server.R")
source("our_ui.R")
library("jsonlite")


# Creating data sets
fast_food_restaurants_df <- read.csv("Datafiniti_Fast_Food_Restaurants.csv",
                                     stringsAsFactors = FALSE)
obesity_df <- read.csv("original_weight_data.csv", stringsAsFactors = FALSE)

obesity_df <- obesity_df %>%
  filter(
    !is.na(Data_Value)
  ) %>%
  filter(
    LocationAbbr != "US"
  ) %>%
  filter(
    QuestionID == "Q036"
  )


# Running the app

shinyApp(ui = our_ui, server = our_server)
