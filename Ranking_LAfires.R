################################################################################

###Ranking methodology: LA Wildfires###

# This dataset creates a ranking score for the first batch of participants 
# Registered for cash assistance

#Data dowloaded on xxxxx from RedRose
#Created on: xx January 2025 by Virginia Chu 
#Last updated: 20 January 2025 by Gersan Vasquez

################################################################################


######Step 1: Load packages
library(readxl)
library(dplyr)
library(psych)
library(tidyverse)
library(openxlsx)
library(purrr)


######Step 2: Import data
setwd("C:/Users/GersanVasquez/Documents/CORE/California")
batch1<- read_excel("RR_Collect_core_2025_lafires_registration_v1_Batch1_mod.xlsx")

######Step 3: Prepare data

#3.1 Convert column names to lowercase
colnames(batch1) <- tolower(colnames(batch1))
#Check variable names
names(batch1)

#3.2 Check for duplicates 

#Based on names
duplicated(batch1$full_name)

#Based on age, sex, and dob
duplicates <- batch1[duplicated(batch1[c("age", "sex", "dob")]) | 
                   duplicated(batch1[c("age", "sex", "dob")], fromLast = TRUE), ]

#No duplicates found based on names or demographic characteristics



######Step 4: Cleaning and deprivation cut-off point

######Dimension 1: HOUSEHOLD COMPOSITION#####

###Indicator 1: Household size

#Describe data
describe(batch1$hh_size)

#Create variable
batch1$rank_hhsize <- ifelse(batch1$hh_size >= 3, 1, 0)

#Check proportions
list(batch1$rank_hhsize)
prop.table(table(batch1$rank_hhsize)) * 100

###Indicator 2: Household head

#Describe data
describe(batch1$age)


#Create variable for those who are not the household head but are female and aged 
#60 and above
batch1 <- batch1 %>%
  mutate(hh_head_notrespondent = ifelse(
    hh_head_sex == "Female" | hh_head_age >= 60, 
    1, 
    0
  ))



#Create ranking variable for hh heads aged 60 and female

batch1 <- batch1 %>%
  mutate(rank_hhhead = ifelse(
    (!is.na(hh_head_notrespondent) & hh_head_notrespondent == 1) | 
      (!is.na(sex) & sex == "Female") | 
      (!is.na(age) & age >= 60), 
    1, 
    0
  ))

###Indicator 3: Single-headed household

#Describe data
batch1$rank_singlehead

#Create variable
batch1$rank_singlehead<-ifelse(batch1$num_adults<2 & batch1$hh_head_rela =="Respondent is the household head"  ,1,0)

#Check proportions
prop.table(table(batch1$rank_singlehead)) * 100
list(batch1$rank_singlehead)


###Indicator 4: School-aged-children 
#Describe data
batch1$num_children
batch1$rank_children<-ifelse(batch1$num_children>0,1,0)


#Create variable
batch1$rank_schoolchildren<-ifelse(batch1$child_1_age>=6 & !is.na(batch1$child_1_age)
                                  |batch1$child_2_age>=6 & !is.na(batch1$child_2_age) 
                                  |batch1$child_3_age>=6 & !is.na(batch1$child_3_age)
                                  |batch1$child_4_age>=6 & !is.na(batch1$child_4_age)
                                  |batch1$child_5_age>=6 & !is.na(batch1$child_5_age), 1,0)

#Check proportions
prop.table(table(batch1$rank_schoolchildren)) * 100



###Indicator 5: Children 5 


batch1$rank_childrenyoung<-ifelse(batch1$child_1_age<6 & !is.na(batch1$child_1_age)
                                  |batch1$child_2_age<6 & !is.na(batch1$child_2_age) 
                                  |batch1$child_3_age<6 & !is.na(batch1$child_3_age)
                                  |batch1$child_4_age<6 & !is.na(batch1$child_4_age)
                                  |batch1$child_5_age<6 & !is.na(batch1$child_5_age), 1,0)

prop.table(table(batch1$rank_childrenyoung)) * 100


###Indicator 6: elderly
#Check data
batch1$num_eld

#Create variable
batch1$rank_elderly<- ifelse(batch1$num_eld>0 & !is.na(batch1$num_eld),1,0) 

#Check proportions
prop.table(table(batch1$rank_elderly)) * 100


###Indicator 7: Pregnancy or lactation
#Check data
batch1$num_preg

#Create variable
batch1$rank_preg<-ifelse(batch1$num_preg=="Yes"& !is.na(batch1$num_preg),1,0)

#Check proportions
prop.table(table(batch1$rank_preg)) * 100

###Indicator 8: Disability and Chronic diseases
batch1$rank_disable<- ifelse(batch1$chronic=="Yes",1,
                             ifelse(batch1$disable=="Yes",1,
                                    ifelse(batch1$hh_disable_chronic=="Yes",1,0))) 

#Check proportions
prop.table(table(batch1$rank_disable)) * 100

###Indicator 9: Race

batch1$rank_raceethn<-ifelse(batch1$ethnicity=="Hispanic",1,
                             ifelse(grepl('Black', batch1$race),1,
                                    ifelse(grepl( 'Asian', batch1$race),1,
                                           ifelse(grepl('Native', batch1$race), 1,0)))) 

#Check proportions
prop.table(table(batch1$rank_raceethn)) * 100

###### Dimension II: Socioeconomic and living conditions#####

###Indicator 10: Income
#Check data
prop.table(table(batch1$hh_income)) * 100

#37.5% of participants reported an income below $35,000

#Create variable
batch1$rank_income<-ifelse(batch1$hh_income=="Less than $35,000"|
                             batch1$hh_income=="Between $35,00 and $49,999"|
                             batch1$hh_income=="Between $50,000–$74,999",1,0)

#Check proportions
prop.table(table(batch1$rank_income)) * 100

###Indicator 11: Employment
batch1$rank_employment<-ifelse(batch1$fire_affect_work=="My place of employment was damaged by the wildfires and I am unable to return to work there" & !is.na(batch1$fire_affect_work)|
                                !is.na(batch1$fire_affect_work_other),1,0 )# the 2 other are valid reasons

#Check proportions
prop.table(table(batch1$rank_income)) * 100

###Indicator 12: Assistance

batch1$rank_govt<- ifelse(batch1$hh_govt_help=="Yes" & !is.na(batch1$hh_govt_help),1,0)

#Check proportions
prop.table(table(batch1$rank_govt)) * 100

###Indicator 13: Shelter

batch1$rank_shelter1<- ifelse(batch1$currently_staying=="In an emegerncy shelter (for example, a church or community center)",1,0)
batch1$rank_shelter2<- ifelse(grepl('car',batch1$currently_staying_other), 1, 
                              ifelse(grepl('unhoused', batch1$currently_staying_other),1,0))
batch1$rank_shelter<-batch1$rank_shelter1+ batch1$rank_shelter2

#Check proportions
prop.table(table(batch1$rank_shelter)) * 100

#Indicator 14: Drinking water
batch1$rank_water<- ifelse(grepl("No",batch1$hh_water_access),1,0)

#Check proportions
prop.table(table(batch1$rank_water)) * 100

#Indicator 15: electricity
batch1$rank_power<-ifelse(grepl("No", batch1$hh_power_access),1,0)

#Check proportions
prop.table(table(batch1$rank_power)) * 100

#Indicator 16: LCD
batch1$rank_lcd<-ifelse(grepl("Yes", batch1$hh_document_issue),1,0)

#Check proportions
prop.table(table(batch1$rank_lcd)) * 100



###### Dimension III: Household damage and food security#####

#Indicator 17: household destroyed

batch1$rank_shelterdamage<- ifelse(grepl("Totally", batch1$fire_impact_house),1,0)

#Check proportions
prop.table(table(batch1$rank_shelterdamage)) * 100

#Indicator 18: Reduced coping Strategies Index (rCSI)

# Calculate rCSI

#The rCSI has the following cut-offs: 0-3, 4-18, and 19 and above 
#which correspond to IPC Phases 1, 2 and 3

#Got NAs. Change NAs to 0
rcsi_vars <- c("food_less_pref_days", "food_borrow_days", "food_reducedmeal_days", 
               "food_portionmeal_days", "food_restrictedadult_days")

batch1[rcsi_vars] <- batch1[rcsi_vars] %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) 


batch1$rcsi <- batch1$food_less_pref_days+
                          (batch1$food_borrow_days * 2)+
                          batch1$food_reducedmeal_days+
                          batch1$food_portionmeal_days+
                          (batch1$food_restrictedadult_days * 3)


#Create cut-off point for rcsi >= 19 which is considered "crisis"
batch1$rank_rcsi<- ifelse(batch1$rcsi>=19 & !is.na(batch1$rcsi),1,0) 

#Check proportions
prop.table(table(batch1$rank_rcsi)) * 100

View(batch1[, c("food_less_pref_days", "food_borrow_days", "food_reducedmeal_days", "food_portionmeal_days","food_restrictedadult_days", "rcsi", "rank_rcsi")])



###### Step 5: Scoring

coefficients <- c(
  rank_hhsize = 0.037, rank_hhhead = 0.037, rank_singlehead = 0.037, 
  rank_schoolchildren = 0.037, rank_childrenyoung = 0.037, rank_elderly = 0.037, 
  rank_preg = 0.037, rank_disable = 0.037, rank_raceethn = 0.037, 
  rank_income = 0.0476, rank_employment = 0.0476, rank_govt = 0.0476, 
  rank_shelter = 0.0476, rank_water = 0.0476, rank_power = 0.0476, 
  rank_lcd = 0.0476, rank_shelterdamage = 0.167, rank_rcsi = 0.167
)

# Apply coefficients to the variables and sum them
batch1 <- batch1 %>%
  mutate(score = pmap_dbl(select(batch1, starts_with("rank_")), 
                         ~ sum(c(...) * coefficients[match(names(c(...)), names(coefficients))], na.rm = TRUE)))


# Create a rank variable based on the score
batch1$rank <- rank(-batch1$score, ties.method = "min")

#Sort it from lowest to highest
batch1 <- batch1 %>% arrange(rank)
 
print( batch1$rank)

final_ranking <-(batch1[, c("id", "score", "rank", "full_name")])

#Import results
write.xlsx(final_ranking, "final_ranking.xlsx")


###Step 6:deindentfied
selection<-c("internal_ID", "age", "id", "barcode", 
             "rank_singlehead", "rank_children", "rank_childrenyoung",
             "rank_elderly", "rank_preg", "rank_disable", "rank_income", "rank_raceethn",
             "rank_employment", "rank_govt", "rank_shelter", "rank_water",
             "rank_power","rank_lcd", "rank_shelterdamage", "rank_rcsi")
batch1_deid<-batch1[selection]


#code examples
# ifelse or  #batch1$rank_singlehead<-ifelse(batch1$num_adults<2 & !is.na(batch1$num_adults)| batch1$hh_head_rela =="Respondent is the household head" |!is.na(batch1$hh_head_rela) ,1,0)


###End of script






