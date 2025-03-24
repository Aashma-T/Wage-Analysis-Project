# Required Libraries
library(readxl)
library(tidyverse)
library(data.table)
library(vtable)
library(lmtest)
library(sandwich)
library(stargazer)

# Read-in data set

WA_DATA <- read.csv("WA_DATA.csv", header = TRUE)

# Subset the data to include only the relevant variables
WA_DATA_subset <- WA_DATA[, c("WAGP", "MIL", "DIS", 
                              "AGEP", "SEX", "SCHL",
                              "RAC1P", "MAR")]


# Remove rows with missing values
WA_DATA_clean <- na.omit(WA_DATA_subset)


#Function to categorize MIL levels
categorize_MIL <- function(MIL) {
  if (MIL %in% c("b", "4")) {
    return("Not a Veteran")
  } else if (MIL %in% c("1")) {
    return("On Active Duty")
  } else if (MIL %in% c("3")) {
    return("On Active Duty in the past")
  } else {
    return(NA)
  }
}

# Apply the function to the SHCL column
WA_DATA_clean <- WA_DATA_clean%>%
  mutate(MIL_Level = sapply(MIL, categorize_MIL))

# Create dummy variables for MIL status
WA_DATA_clean <- WA_DATA_clean %>%
  mutate(Not_a_Veteran = ifelse(MIL_Level == "Not a Veteran", 1, 0),
         On_Active_Duty = ifelse(MIL_Level == "On Active Duty", 1, 0),
         On_Active_Duty_in_the_past = ifelse(MIL_Level == "On Active Duty in the past", 1, 0))                  




#Converting Disablility into a dummy variable with 0 or 1 value
WA_DATA_clean <- WA_DATA_clean %>%
  mutate(DIS = case_when(
    DIS == 1 ~ 1,
    DIS == 2 ~ 0,
    TRUE ~ NA_real_  # Assign NA to any other values if they exist
  ))



#Create Quadratic term for Age
WA_DATA_clean$AGE2 <- WA_DATA_clean$AGEP^2

#Subtract 1 from all values in the Sex column to ensure 1 = Female, 0 = Male, rename the column to female
WA_DATA_clean <- WA_DATA_clean %>%
  mutate(FEMALE = SEX - 1)


#Function to categorize education levels
categorize_education <- function(SCHL) {
  if (SCHL %in% c("bb", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14")) {
    return("Less than High School")
  } else if (SCHL %in% c("15", "16", "17")) {
    return("High School")
  } else if (SCHL %in% c("18", "19", "20", "21", "22", "23", "24")) {
    return("More than High School")
  } else {
    return(NA)
  }
}

# Apply the function to the SHCL column
WA_DATA_clean <- WA_DATA_clean %>%
  mutate(Education_Level = sapply(SCHL, categorize_education))

# Create dummy variables
WA_DATA_clean <- WA_DATA_clean %>%
  mutate(Less_than_High_School = ifelse(Education_Level == "Less than High School", 1, 0),
         High_School = ifelse(Education_Level == "High School", 1, 0),
         More_than_High_School = ifelse(Education_Level == "More than High School", 1, 0))


#Converting Race into a binary variable

WA_DATA_clean <- WA_DATA_clean %>%
  mutate(RAC1P = case_when(
    RAC1P  %in%  c(2, 3,4 , 5, 6, 7, 8, 9) ~ 1,
    RAC1P == 1 ~ 0,
    TRUE ~ NA_real_  # Assign NA to any other values if they exist
  ))

#Creating Binary Variable for married
WA_DATA_clean <- WA_DATA_clean %>%
  mutate(MAR = case_when(
    MAR == 1 ~ 1,
    MAR %in% c( 2, 3 , 4, 5) ~ 0,
    TRUE ~ NA_real_  # Assign NA to any other values if they exist
  ))


colnames(WA_DATA_clean)

WA_DATA_for_model <- WA_DATA_clean[, c("WAGP", "DIS", "AGEP", "FEMALE",
                                       "Not_a_Veteran", "On_Active_Duty" , "On_Active_Duty_in_the_past" ,
                                       "Less_than_High_School", "High_School" , "More_than_High_School", "RAC1P", "MAR")]

WA_DATA_for_model <-  na.omit(WA_DATA_for_model)

##generate descriptive statistics
compute_summary_stats <- function(data, column_name) {
  # Check if the specified column exists in the data
  if (!column_name %in% colnames(data)) {
    WAGP <-  compute_summary_stats(WA_DATA_for_model, "WAGP")
    stop("Column not found in the data frame")
  }
  
  # Extract the specified column
  column <- data[[column_name]]
  
  # Calculate summary statistics
  summary_stats <- c(
    Mean = round(mean(column, na.rm = TRUE), 3),
    Median = round(median(column, na.rm = TRUE), 3),
    SD = round(sd(column, na.rm = TRUE), 3),
    Min = round(min(column, na.rm = TRUE), 3),
    Max = round(max(column, na.rm = TRUE), 3)
  )
  
  return(summary_stats)
}



#Check for zero values in WAGP:
sum(WA_DATA_for_model$WAGP == 0, na.rm = TRUE)
#Remove rows with zero values in WAGP:
WA_DATA_for_model <- WA_DATA_for_model %>%
  filter(WAGP > 0)
         
         
#Calculating summary stats for each variable
WAGP <-  compute_summary_stats(WA_DATA_for_model, "WAGP")

DIS <-  compute_summary_stats(WA_DATA_for_model, "DIS")

AGEP <-  compute_summary_stats(WA_DATA_for_model, "AGEP")

FEMALE <-  compute_summary_stats(WA_DATA_for_model, "FEMALE")

Not_a_Veteran <-  compute_summary_stats(WA_DATA_for_model, "Not_a_Veteran")

On_Active_Duty <-  compute_summary_stats(WA_DATA_for_model, "On_Active_Duty")

On_Active_Duty_in_the_past <-  compute_summary_stats(WA_DATA_for_model, "On_Active_Duty_in_the_past")

Less_than_High_School <-  compute_summary_stats(WA_DATA_for_model, "Less_than_High_School")

High_School <-  compute_summary_stats(WA_DATA_for_model, "High_School")

More_than_High_School <-  compute_summary_stats(WA_DATA_for_model, "More_than_High_School")

RAC1P <-  compute_summary_stats(WA_DATA_for_model, "RAC1P")

MAR <-  compute_summary_stats(WA_DATA_for_model, "MAR")


paste('SUMMARY STATISTICS')
summary_all <- data.frame(WAGP, DIS , AGEP , FEMALE , Not_a_Veteran, On_Active_Duty,
                          On_Active_Duty_in_the_past, Less_than_High_School , High_School , 
                          More_than_High_School , RAC1P , MAR )
print(summary_all)

write.csv(summary_all, file = "output.csv", row.names = TRUE)


# Descriptive statistics for the subset of properties without missing values
vtable(WA_DATA_for_model, lush = TRUE) 



Age_squared <- WA_DATA_for_model$AGEP^2

#Linear regression Model
Model_A <- lm(WAGP ~ On_Active_Duty + On_Active_Duty_in_the_past +  DIS + 
                Less_than_High_School + More_than_High_School + AGEP + Age_squared
              + FEMALE + RAC1P + MAR +
                On_Active_Duty*DIS + On_Active_Duty_in_the_past*DIS, data = WA_DATA_for_model, x = TRUE)
summary(Model_A)

#Conduct BP Test
bptest(Model_A)

#Generate the Variance Covariance Matrix of the Parameter Estimates
vcovHC(Model_A, type = "HC") #the diagonal elements are the variances of the parameter estimates

#Generate the Robust standard errors and print them on screen 
sandwich_se_Model_A <- diag(vcovHC(Model_A, type = "HC"))^0.5
sandwich_se_Model_A


#Compute t statistics based on robust standard errors
BPG_Model_A <-  coeftest(Model_A, vcov = vcovHC(Model_A, type = "HC"))
BPG_Model_A




Age_squared <- WA_DATA_for_model$AGEP^2

#log model
Model_B <- lm( log(WAGP) ~ On_Active_Duty + On_Active_Duty_in_the_past +  DIS + 
                Less_than_High_School + More_than_High_School + AGEP + Age_squared
              + FEMALE + RAC1P + MAR +  On_Active_Duty*DIS + On_Active_Duty_in_the_past*DIS, data = WA_DATA_for_model , x = TRUE)
summary(Model_B)

#Conduct BPG Test for Model B
bptest(Model_B)

#Generate the Variance Covariance Matrix of the Parameter Estimates
vcovHC(Model_B, type = "HC") #the diagonal elements are the variances of the parameter estimates

#Generate the Robust standard errors and print them on screen 
sandwich_se_Model_B <- diag(vcovHC(Model_B, type = "HC"))^0.5
sandwich_se_Model_B


#Compute t statistics based on robust standard errors
BPG_Model_B <-  coeftest(Model_B, vcov = vcovHC(Model_B, type = "HC"))
BPG_Model_B


#Log model with interaction term
Model_C <- lm(log(WAGP) ~ On_Active_Duty + On_Active_Duty_in_the_past +  DIS + 
                 Less_than_High_School + More_than_High_School + AGEP + Age_squared
               + FEMALE + RAC1P + MAR +
                On_Active_Duty*DIS + On_Active_Duty_in_the_past*DIS, data = WA_DATA_for_model, x = TRUE)
summary(Model_C)


#Conduct BPG Test for Model c
bptest(Model_C)

#Generate the Variance Covariance Matrix of the Parameter Estimates
vcovHC(Model_C, type = "HC") #the diagonal elements are the variances of the parameter estimates

#Generate the Robust standard errors and print them on screen 
sandwich_se_Model_C <- diag(vcovHC(Model_C, type = "HC"))^0.5
sandwich_se_Model_C


#Compute t statistics based on robust standard errors
BPG_Model_C <-  coeftest(Model_C, vcov = vcovHC(Model_C, type = "HC"))
BPG_Model_C

stargazer(Model_A, Model_B,Model_C, 
          type = "text", 
          title="<center>Regression Models</center>",
          model.names = FALSE, 
          column.separate =  c(2, 2),
          dep.var.labels = c("Wages", "Log of Wages", "Log of Wages" ),
          align=TRUE,
          omit.stat=c("LL","ser","f"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          se = list((summary(Model_A)$coefficients[,"Std. Error"]),
                    (summary(Model_B)$coefficients[,"Std. Error"]),
                    (BPG_Model_C[,"Std. Error"])),
          notes = "Standard errors for Model 3 have corrected for heteroskedasticity",
          out = "regression_table.text"
)

