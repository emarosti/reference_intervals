#
# Eliana Marostica
# Friday June 23, 2017
# Last Updated: June 23, 2017
# server

#SETUP
library(shiny)
library(data.table)
#extract relevant data
ref_ranges <- read_csv(file="nhanes_Lab_Reference_Ranges.csv") %>%
  select(Test_Name, var_desc, `Unit Type`, var, Gender, `Age group`, `Low CU`, `High CU`)
#convert to data.table format
refrang_table <- data.table(ref_ranges)
#set the keys to enable fast look ups
setkey(refrang_table, var_desc)



#SERVER
server <- function(input, output) {
  #Plots - Age Group
  output$age_group <- renderPlot({
    test_ranges <- refrang_table[.(input$testname)]
    
    ggplot(test_ranges) +
      geom_errorbar(mapping = aes(x = `Age group`, ymin = `Low CU`, ymax = `High CU`), width = 0.8) +
      labs(title = "Age Group")
  })
  #Plots - Gender
  output$gender <- renderPlot({
    test_ranges <- refrang_table[.(input$testname)]
    
    ggplot(test_ranges) +
      geom_errorbar(mapping = aes(x = `Gender`, ymin = `Low CU`, ymax = `High CU`), width = 0.8) +
      labs(title = "Gender")
  })
  #Table
  output$table <- renderDataTable({
    test_ranges <- refrang_table[.(input$testname)]
    filter(test_ranges, `Unit Type` == input$units)
    test_ranges
  })
}







