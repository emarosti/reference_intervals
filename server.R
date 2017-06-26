#
# Eliana Marostica
# Friday June 23, 2017
# Last Updated: June 23, 2017
# server
#TO DO: write function to filter out non numeric values from the df (as.numeric)
#make a slide to show isseus with data
#look at stedman's and merck manuals
#  take 10 things from the manual and check to make sure they were accurately written in excel
#  lack of interaction between gender and age - is it ambiguous in the manuals?
#METHODS
# extending reference ranges
# r and shiny
# statistical test used to analyze it


#SETUP
library(shiny)
library(data.table)
#extract relevant data
ref_ranges <- read_csv(file="nhanes_Lab_Reference_Ranges.csv") %>%
  select(`Test Name`, var_desc, `Unit Type`, var, Gender, `Age group`, `Low CU`, `High CU`)
ref_ranges$`Low CU` <- as.numeric(ref_ranges$`Low CU`)
ref_ranges$`High CU` <- as.numeric(ref_ranges$`High CU`)
ref_ranges$`High CU`
ref_ranges <- ref_ranges %>%
  filter(!is.na(`Low CU`), !is.na(`High CU`), `Unit Type` != "-") %>% #filtering by unit type for now because
                                                                  #when there isn't a unit type listed it
                                                                  #makes visuals tough for other units, 
                                                                  #but you do lose some data on tests like
                                                                  #carbon dioxide levels that have data on
                                                                  #male vs female but not the unit type listed
  unique()


#convert to data.table format
refrang_table <- data.table(ref_ranges)
#set the keys to enable fast look ups
setkey(refrang_table, `Test Name`)


#SERVER
server <- function(input, output) {
  
  #Plots - Age Group
  output$age_group <- renderPlot({
    refrang_table <- filter(refrang_table, `Unit Type` == input$units)
    #convert to data.table format
    refrang_table <- data.table(ref_ranges)
    #set the keys to enable fast look ups
    setkey(refrang_table, `Test Name`)
    test_ranges <- refrang_table[.(input$testname)]
    
    ggplot(test_ranges, aes(x = `Age group`, ymin = `Low CU`, ymax = `High CU`, group = var, color = var_desc)) +
      geom_errorbar(width = 0.2,
                    position = position_dodge(.5)) +
      labs(title = "Age Group")
  })
  #Plots - Gender
  output$gender <- renderPlot({
    refrang_table <- filter(refrang_table, `Unit Type` == input$units)
    #convert to data.table format
    refrang_table <- data.table(ref_ranges)
    #set the keys to enable fast look ups
    setkey(refrang_table, `Test Name`)
    test_ranges <- refrang_table[.(input$testname)]
    
    ggplot(test_ranges, aes(x = `Gender`, ymin = `Low CU`, ymax = `High CU`, group = var, color = var_desc)) +
      geom_errorbar(width = 0.2,
                    position = position_dodge(.5)) +
      labs(title = "Gender")
  })
  #Table
  output$table <- renderDataTable({
    test_ranges <- refrang_table[.(input$testname)]
    test_ranges
  })
}







