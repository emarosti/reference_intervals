# Author: Eliana Marostica
# Contributors: Arjun (Raj) Manrai, PhD, Chirag Patel, PhD
# Created: Friday June 23, 2017
# Last Updated: August 4, 2017
# ui

library(shiny)
library(data.table)
library(shinythemes)
library(tidyverse)
library(survey)

#SETUP
ref_ranges <- read_csv(file = "nhanes_Lab_Reference_Ranges.csv")
test_names <- ref_ranges[["Test Name"]]
test_names <- unique(test_names)

#UI
ui <- fluidPage(
  theme = shinytheme("yeti"),

  #Title
  titlePanel(h1("Biomarker Reference Intervals and Ranges")),
  HTML('<p>Created by Eliana Marostica, as part of the <a href="http://www.chiragjpgroup.org/">Chirag Patel Group</a></p>'),
  HTML('<p>Data from <a href="http://stedmansonline.com/webFiles/Dict-Stedmans28/APP17.pdf">Stedmans Medical Dictionary</a>, 
       <a href="http://www.merckmanuals.com/professional/appendixes/normal-laboratory-values/normal-laboratory-values">Merck Manual</a>, 
       and <a href="https://www.cdc.gov/nchs/nhanes/index.htm">National Health and Nutrition Examination Survey</a> </p>'),

  
  navbarPage("",

              tabPanel("Summary",
                       value = "B", 
                       h4("Summary Statistics - Counts"),
                       dataTableOutput(outputId = "flow")),
    
              tabPanel("Reference Ranges", value = "A", 
                       #Layout
                       sidebarLayout(
                         #Sidebar
                         sidebarPanel(
                           radioButtons(inputId = "units",
                                        label = "Units",
                                        choices = c("SI" = "SI Unit", "Conventional" = "Conventional Unit", "Not Specified" = "-")
                           ),
                           
                           selectInput(inputId = "testname",
                                       label = "Test",
                                       choices = test_names
                                       ),
                           downloadButton(outputId = "download",
                                          label = "Download Table (.csv)"
                                          )
                         ),
                         #Main Panel
                         mainPanel(
                           #h4("Plot"),
                           fluidRow(plotOutput(outputId = "plot"))
                         )
                       ),
                       h4("Table"),
                       fluidRow(column(dataTableOutput(outputId = "table"), width = 12))
                
              )
  ),

  #Signature
  h6(HTML('<p><i>Last Updated: August 4, 2017</i> | <a href="https://github.com/emarosti/reference_intervals">GitHub Repository</a>'))
  )

#end
