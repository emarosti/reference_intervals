# Author: Eliana Marostica
# Contributors: Arjun (Raj) Manrai, PhD, Chirag Patel, PhD
# Created: Friday June 23, 2017
# Last Updated: August 4, 2017
# server

#SETUP
library(tidyverse)
library(shiny)
library(data.table)
library(survey)
library(stringr)
library(ggthemes)

#SURVEY SETUP

load(str_c(getwd(), "quantiles.Rdata", sep = "/"))
quantiles <- data.table(quantiles)
setkey(quantiles, variable)


#SERVER
server <- function(input, output) {
  
  #DATA TABLE - STATS
  output$flow <- renderDataTable({
    ref_ranges <- read_csv(file="nhanes_Lab_Reference_Ranges.csv") %>%
      filter(Source != "-")
    
    steds <- filter(ref_ranges, Source == "Stedman's")
    steds <- distinct(steds, `Test Name`, .keep_all = TRUE)
    
    mercks <- filter(ref_ranges, Source == "Merck's Manual")
    mercks <- distinct(mercks, `Test Name`, .keep_all = TRUE)
    
    by_ID <- distinct(ref_ranges, ID, .keep_all = TRUE) #filter by ID
    by_TN <- distinct(ref_ranges, `Test Name`, .keep_all = TRUE) #only find distinct tests --> filter by test name
    
    stratifications <- c("Total", "Age", "Gender", "Ethnicity")
    total <- c(nrow(by_TN),
               nrow(filter(by_TN, `Age_group` != '-')),
               nrow(filter(by_TN, Gender != '-')),
               0)
    stedmans <- c(nrow(steds),
               nrow(filter(steds, `Age_group` != '-')),
               nrow(filter(steds, Gender != '-')),
               0)
    mercksmanual <- c(nrow(mercks),
               nrow(filter(mercks, `Age_group` != '-')),
               nrow(filter(mercks, Gender != '-')), 
               0)
    overlap <- c((nrow(steds) + nrow(mercks) - nrow(by_TN)),
                  (nrow(filter(steds, `Age_group` != '-')) + nrow(filter(mercks, `Age_group` != '-')) - nrow(filter(by_TN, `Age_group` != '-'))),
                  (nrow(filter(steds, Gender != '-')) + nrow(filter(mercks, Gender != '-')) - nrow(filter(by_TN, Gender != '-'))), 0)
    rangs <- c(135, 0, 135, 135)
    
    df <- data.frame("Stratifications" = stratifications, 
                     "Total" = total, 
                     "Stedman" = stedmans,
                     "Merck Manual" = mercksmanual,
                     "Overlap" = overlap,
                     "Computed Ranges" = rangs)
    
    df
    
  }) 
  
  ranges_df <- reactive({
    #filter by unit type specified by the radio button
    refrangs <- filter(refrang_table, `Unit Type` %in% c(input$units, "Conventional Unit/SI Unit"), `Test Name` == input$testname)
    #convert to data.table format
    test_ranges <- data.table(refrangs)
    #set the keys to enable fast look ups
    setkey(test_ranges, `Test Name`)
    #test_ranges <- refrangs[.(input$testname)]
    
    
    #data frame to be used for plot -- will collect every RI
    ranges_df <- data.frame("Label" = factor(),
                            "Ethnicity" = numeric(),
                            "Gender" = numeric(),
                            "Low"= numeric(),
                            "High" = numeric()
                            )
    
    #for loop setup
    Label <- ""
    
    #for loop to go through each row in test_ranges (each data point/reference interval)
    for (i in 1:nrow(test_ranges)){
      Label <- test_ranges[["Source"]][[i]]
      
      #Specify the serum in the label, if available
      if (test_ranges[["Specimen"]][[i]] != "-"){
        Label <- str_c(Label, test_ranges[["Specimen"]][[i]], sep = ", ")
      }
      #specify the cell type in the label, if available
      if (test_ranges[["Cell Type"]][[i]] != "-"){
        Label <- str_c(Label, test_ranges[["Cell Type"]][[i]], sep = ", ")
      }
      
      if (input$units == "SI Unit"){
        Low <- test_ranges[["Low SI"]][[i]]
        High <- test_ranges[["High SI"]][[i]]
      } else{
        Low <- test_ranges[["Low CU"]][[i]]
        High <- test_ranges[["High CU"]][[i]]
      }
      Count <- NA
      Mid <- (Low + High)/2
      Ethnicity <- 0
      
      gndr <- test_ranges[["Gender"]][[i]]
      if(gndr == "-") Gender <- 0
      else if (gndr == "Male") Gender <- 1
      else Gender <- 2
      
      row <- data.frame(Label, Ethnicity, Gender, Low, High, Mid, Count)
      ranges_df <- rbind(ranges_df, row)
      
      #Add the computed NHANES percentile interval's Label, low value, and high value to the data frame
      nhanes_var <- test_ranges[["var"]][i]
      for (k in 1:24){
        if (nhanes_var %in% quantiles[["variable"]]  &&  !(nhanes_var %in% ranges_df[["Label"]])){
          vec <- filter(quantiles[.(nhanes_var)][k])
          Label <- paste("NHANES,", nhanes_var, vec[["ethnicity"]])
          Low <- as.numeric(vec[["low"]])
          High <- as.numeric(vec[["high"]])
          Count <- vec[["count"]]
          Mid <- as.numeric(vec[["med"]])
          Gender <- vec[["gender"]]
          Ethnicity <- vec[["ethnicity"]]
          row <- data.frame(Label, Ethnicity, Gender, Low, High, Mid, Count)
          ranges_df <- rbind(ranges_df, row)
        }
      }
      #reset Label to be empty
      Label <- ""
    }
    
    #remove duplicates
    ranges_df <- unique(ranges_df)
    #wrap x labels so that they don't overlap
    ranges_df$Label <- str_wrap(ranges_df$Label, width = 30)
    ranges_df
    
  })
  
  #PLOT
  output$plot <- renderPlot({
    tryCatch(
      {
        dat <- ranges_df()
        
        dat[["Gender"]][dat[["Gender"]]=="0" && startsWith(dat[["Label"]], "NHANES")] <- NULL
        
        dat[["Gender"]][dat[["Gender"]]=="0"] <- "Not Specified"
        dat[["Gender"]][dat[["Gender"]]=="1"] <- "Male"
        dat[["Gender"]][dat[["Gender"]]=="2"] <- "Female"
        dat[["Ethnicity"]][dat[["Ethnicity"]] == "0"] <- "Not Specified"
        dat[["Ethnicity"]][dat[["Ethnicity"]] == "1"] <- "Mexican American"
        dat[["Ethnicity"]][dat[["Ethnicity"]]=="2"] <- "Other Hispanic"
        dat[["Ethnicity"]][dat[["Ethnicity"]]=="3"] <- "White"
        dat[["Ethnicity"]][dat[["Ethnicity"]]=="4"] <- "Black"
        dat[["Ethnicity"]][dat[["Ethnicity"]]=="5"] <- "Other/Multi-Racial"
        dat[["Ethnicity"]][dat[["Ethnicity"]]=="6"] <- "Asian"
        dat[["Ethnicity"]][dat[["Ethnicity"]]=="7"] <- "Other/Multi-Racial"
        dat$Ethnicity <- factor(dat$Ethnicity)
        dat$Gender <- factor(dat$Gender)
        
      }, 
      error = function(e) dat <- data.frame()
    )
    
    #filter by unit type specified by the radio button
    refrangs <- filter(refrang_table, `Unit Type` %in% c(input$units, "Conventional Unit/SI Unit"), `Test Name` == input$testname)
    #convert to data.table format
    test_ranges <- data.table(refrangs)
    
    if(nrow(test_ranges) == 0){
      ggplot() +
        ggtitle(label = "Data do not exist in this form.", subtitle = "Try a different unit type.")
    } else{
      #Plot Creation
      
      #setup for units to be displayed on graph
      if (input$units == "SI Unit"){
        units <- test_ranges[["SI Units"]][1]
      } else{
        units <- test_ranges[["Conventional Units"]][1]
      }
      #plot
      ggplot(dat, aes(x = Label, y = Mid, fill = Ethnicity, group = Ethnicity)) + 
        geom_crossbar(aes(ymin = Low, ymax = High), width=0.75, alpha=0.75, position = position_dodge(0.5)) +
        xlab('') + 
        facet_grid(. ~ Gender) +
        theme_fivethirtyeight(base_size=14) +
        theme(axis.title = element_text()) + 
        ylab(paste(input$testname, "(", units, ")")) +
        theme(axis.text.x = element_text(angle=45,size=10,hjust=1)) +
        theme(axis.ticks = element_blank()) +
        theme(plot.margin = unit(c(0.5,0.5,0,1), "cm")) +
        ggtitle(input$testname) 
    }
    
    
    
  })
 
  #DATA TABLE - RIs
  output$table <- renderDataTable({
    tryCatch({
      dat <- ranges_df()
      dat[["Gender"]][dat[["Gender"]]=="0"] <- "Not Specified"
      dat[["Gender"]][dat[["Gender"]]=="1"] <- "Male"
      dat[["Gender"]][dat[["Gender"]]=="2"] <- "Female"
      dat[["Ethnicity"]][dat[["Ethnicity"]] == "0"] <- "Not Specified"
      dat[["Ethnicity"]][dat[["Ethnicity"]] == "1"] <- "Mexican American"
      dat[["Ethnicity"]][dat[["Ethnicity"]]=="2"] <- "Other Hispanic"
      dat[["Ethnicity"]][dat[["Ethnicity"]]=="3"] <- "White"
      dat[["Ethnicity"]][dat[["Ethnicity"]]=="4"] <- "Black"
      dat[["Ethnicity"]][dat[["Ethnicity"]]=="5"] <- "Other/Multi-Racial"
      dat[["Ethnicity"]][dat[["Ethnicity"]]=="6"] <- "Asian"
      dat[["Ethnicity"]][dat[["Ethnicity"]]=="7"] <- "Other/Multi-Racial"
      dat
    },
    error = function(e) NA)
  }, options = list(scrollX = TRUE))
  
  
  output$download <- downloadHandler(
    filename = function(){ paste('data-', input$testname, '.csv', sep='') },
    content = function(file) { write.csv(ranges_df(), file)}
  )
  
}







