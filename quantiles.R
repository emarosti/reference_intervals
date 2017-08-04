# Author: Eliana Marostica
# Contributors: Arjun (Raj) Manrai, PhD, Chirag Patel, PhD
# Created: Friday June 23, 2017
# Last Updated: August 4, 2017
# quantile computations


#SETUP
library(tidyverse)
library(shiny)
library(data.table)
library(survey)
library(stringr)

#load NHANES data and Steds/Mercks data
load(str_c(getwd(), "nhanes_schema_merged_all_031816.Rdata", sep = "/"))
load(str_c(getwd(), "steds_mercks.Rdata", sep = "/"))

#create a fourth ethnicity column to combine RIDRETH1 and RIDRETH3 variables
bigData$RIDRETH4 <- bigData$RIDRETH1
bigData$RIDRETH4[which(bigData$SDDSRVYR >= 7)] = bigData$RIDRETH3[which(bigData$SDDSRVYR >= 7)]

#filter bigData for phenotypes that make the data points representative of a "healthy" population
#Age (18-65), blood pressure, smoking history, weight
bigData <- filter(bigData, RIDAGEYR >= 18, RIDAGEYR <= 65, BPQ020 == 2, SMQ020 == 2, MCQ080 == 2)

#generate list of all nhanes variables to compute quantiles on
nhanes_vars <- unique(refrang_table[["var"]])
nhanes_vars <- nhanes_vars[! nhanes_vars == "-"]

#prepare the dataframe
quantiles <- data.frame("variable" = character(),
                        "ethnicity" = numeric(),
                        "gender" = numeric(),
                        "low" = numeric(),
                        "high" = numeric(),
                        "count" = numeric(),
                        "svyvariance" = numeric(),
                        "variance" = numeric())

#indicator variable values
#0 corresponds to not stratifying by that variable
genders <- c(0, 1, 2)
ethnicities <- c(0, 1, 2, 3, 4, 5, 6, 7) 

#for loop to go through each laboratory test
for (i in 1:length(nhanes_vars)){
  variable <- nhanes_vars[i]
  
  #ignore 2nd tests
  if(!startsWith(variable, "LB2")){
    nhanes_test <- select_(bigData, variable, "RIAGENDR", "RIDRETH4", "SDMVPSU", "SDMVSTRA", "WTMEC2YR")
    
    #cycle through every ethnicity stratum
    for(e in ethnicities){
      ethnicity <- e
      
      #cycle through every gender stratum
      for(g in genders){
        gender <- g
        
        #series of if else statements: determine if either or both ethnicity and gender will not be accounted for
        if(gender == 0 && ethnicity == 0){
          counter <- filter(nhanes_test, !is.na(variable))
          stratum <- nhanes_test
        } 
        else if (gender == 0) {
          counter <- filter(nhanes_test, RIDRETH4 == ethnicity, !is.na(variable))
          stratum <- filter(nhanes_test, RIDRETH4 == ethnicity)
        }
        else if (ethnicity == 0){
          counter <- filter(nhanes_test, RIAGENDR == gender, !is.na(variable))
          stratum <- filter(nhanes_test, RIAGENDR == gender)
        } 
        else{
          counter <- filter(nhanes_test, RIAGENDR == gender, RIDRETH4 == ethnicity, !is.na(variable))
          stratum <- filter(nhanes_test, RIAGENDR == gender, RIDRETH4 == ethnicity)
        } 
        
        count <- nrow(counter)
        dsn <- svydesign(ids=~SDMVPSU, strata=~SDMVSTRA, weights=~!is.na(WTMEC2YR), nest=T, data=stratum)
        quant <- tryCatch(svyquantile(~(eval(parse(text = variable))), dsn, c(0.025, 0.50, 0.975), na.rm = TRUE), error = function(e) c(NA, NA))
        svyvariance <- tryCatch(svyvar(~(eval(parse(text = variable))), dsn, na.rm = TRUE)[[1]], error = function(e) NA)
        variance <- var(stratum[[variable]], na.rm = TRUE)
        low <- quant[1]
        med <- quant[2]
        high <- quant[3]
        row <- data.frame(variable, ethnicity, gender, low, med, high, count, svyvariance, variance)
        quantiles <- rbind(quantiles, row)
        
        #keep track of quantile computations by printing when they have successfully completed
        print(paste("success", i, e, g))
      }#end gender loop
    }#end ethnicity loop
  }#end LB2 if 
}#end lab test loop

#save quantiles in a file titled quantiles.Rdata
save(quantiles, file = "quantiles.Rdata")

