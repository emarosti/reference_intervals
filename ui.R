#
# Eliana Marostica
# Friday June 23, 2017
# Last Updated: June 23, 2017
# ui

library(shiny)
library(data.table)
library(shinythemes)
library(tidyverse)


#SETUP
#
ref_ranges <- read_csv(file="nhanes_Lab_Reference_Ranges.csv")

keys <- ref_ranges[["var_desc"]] %>%
  unique()
keys <- setNames(keys, keys)

# Encoding(ref_ranges[["Test Name"]]) <- "UTF-8"
# iconv(ref_ranges[["Test Name"]], "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''


#UI
ui <- fluidPage(
  theme = shinytheme("slate"),

  #Title
  titlePanel(h1("Published Reference Intervals")),
  h4(em("Sources: Stedman's and Merck Manual")),

  #Layout
  sidebarLayout(
    #Sidebar
    sidebarPanel(
      radioButtons(inputId = "units",
                   label = "Units",
                   choices = c("SI" = "SI Unit", "Conventional" = "Conventional Unit")
                   ),
      selectInput(inputId = "testname",
                  label = "Test",
                  choices = keys #c("Albumin" = "Albumin", "Calcitonin" = "Calcitonin")
                  )
    ),
    #Main Panel
    mainPanel(
      h4("Plots"),
      fluidRow(plotOutput(outputId = "age_group")),
      br(),
      fluidRow(plotOutput(outputId = "gender")),
      br(),
      h4("Table"),
      fluidRow(dataTableOutput(outputId = "table"))
    )
  ),

  #Signature
  h6(em("Created by Eliana Marostica. Last Updated: June 23, 2017"))
  )






