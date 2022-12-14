library(scales)
library("tibble")
library("stringr")

#data <- read.csv("F:\\[4 - HKI]Chuyên đề 5\\middle_term\\minorityclone.csv")
data <- read.csv("D:\\Năm 4\\Kỳ 1\\Chuyên đề 5\\Code R\\GiuaKy\\r_dataset_minority\\minorityclone.csv")
colnames(data)
#2 data cleaning
# change column name
data <- setNames(data, c("Id","Order","Minority_Name","Self_title", "Other_names","International_names","Local_group","Distribution","Population","Male","Female","Language","Language_group","Language_branch","Language_family","Region","General_information_about_traditional_costumes","Note"))

# change available value to empty
data<-replace(data,data == "Not avaible","")

# empty value to NA value
data[data == ""] <- NA   

#delete note and language_group column 
data <- data[,!names(data) %in% c("Note")]
data <- data[,!names(data) %in% c("Language_group")]
data <- data[,!names(data) %in% c("Other_names")]
data <- data[,!names(data) %in% c("Local_group")]

#lang_family = table(data$Language_family)
#lang_family

# data cleaning in Language_branch column
# group the categories “Thai”, “Thai, Sino”in a category “Tay-Thai”.
data$Language_branch[data$Language_branch %in% c("Thai", "Thai, Sino")] <- "Tay-Thai" 
data$Language_branch[data$Language_branch %in% c("Kra")] <- "Kadai" 
data$Language_branch[data$Language_branch %in% c("Mon Khmer", "Mon - Khmer")] <- "Mon-Khmer"
data$Language_branch[data$Language_branch %in% c("Vietnamese")] <- "Viet-Muong"
data$Language_branch[(is.na(data$Language_branch)) & (data$Language_family == "H'mong -Mien")] <- "H'Mong"
data$Language_branch[is.na(data$Language_branch)] <- "Kadai"

# data cleaning in Language_family column
# group the categories “Autro-Asiatic”in a category “Austro-Asiatic”.
data$Language_family[data$Language_family %in% c("Autro-Asiatic")] <- "Austro-Asiatic"
data$Language_family[data$Language_family %in% c("Tai-Kadai, Sino-Tibetan")] <- "Tai-Kadai"


data$Self_title[str_detect(data$Self_title, " or ")] <- gsub(" or ",", ", data$Self_title[str_detect(data$Self_title, " or ")])
# replace 'or' with ',' in International_names column
data$International_names[str_detect(data$International_names, " or ")] <- gsub(" or ",", ", data$International_names[str_detect(data$International_names, " or ")])


#4 - Create a new variable(column) based on an existing variable(column)
#SOLVE: Calculate the percentage of the population of each minority name

# calculate the total population of ethnic minorities
totalPopulation = sum(data$Population)
totalMalePopulation = sum(data$Male)
totalFemalePopulation = sum(data$Female)
# create a new column 'Population Percent' that calculates the percentage of the population for each ethnic minority
data$`Population_Percent` = (data$Population/totalPopulation)

# create a new column 'Male Percent' that calculates the percentage of male population by ethnic minority 
data$`Male_Percent` = (data$Male/data$Population)

# create a new column 'Female Percent' that calculates the percentage of female population by ethnic minority 
data$`Female_Percent` = (data$Female/data$Population)

# format to percent
#data$`Population_Percent` = percent(data$`Population_Percent`, accuracy = 0.0001)
#data$`Male_Percent` = percent(data$Male_Percent, accuracy = 0.1)
#data$`Female_Percent` = percent(data$Female_Percent, accuracy = 0.1)

#5 - Ask research questions that need to be known in the data set
#SOLVE: Calculate how many people in each region and how many percent?

# create table region from Region column
region = table(data$Region)
region
# get all name region
arrRegionNames = names(region)
arrRegionNames
# create new dataframe
df_region = data.frame()

# loop for region names 
for (i in arrRegionNames) {
  
  # calc sum population for region
  regionPopulation = sum(data$Population[data$Region == i])
  malePopulation = sum(data$Male[data$Region == i])
  femalePopulation = sum(data$Female[data$Region == i])
  
  
  
  # cal population percent for each region, male, female
  pRegionPopulation = sum(data$Population_Percent[data$Region == i])
  pMalePopulation = malePopulation/regionPopulation
  pFemalePopulation = femalePopulation/regionPopulation
  
  # Assign value to output
  output = c(i, regionPopulation, 
             percent(pRegionPopulation, accuracy = 0.0001), 
             percent(pMalePopulation, accuracy = 0.01),
             percent(pFemalePopulation, accuracy = 0.01))
  
  # Assign value to df_region by using rbind 
  df_region = rbind(df_region, output)
}
# set name for column in df_region
colnames(df_region)<-c("Region", "Population", "Population.percent", "Male.percent", "Female.percent")

View(df_region)

df_lang_Famly = data.frame()

#create table language famil from language_family column
language_family_table <- table(data$Language_family)
language_family_table

#get all name language family
language_family = names(language_family_table)

# loop for language family name 
for(i in language_family){
  # calculate the population for each Language family
  lanFamilyPopulation = sum(data$Population[data$Language_family == i])
  
  # calculate the percent population for each Language family
  percentPopulation = lanFamilyPopulation/totalPopulation
  
  # Calculate the population by gender for each Language family
  lFMale = sum(data$Male[data$Language_family == i])
  lFFemale = sum(data$Female[data$Language_family == i])
  
  # calculate the percent population by gender for each Language family
  percentMale = lFMale/lanFamilyPopulation
  percentFemale = lFFemale/lanFamilyPopulation
  
  # Assign value to output 
  output = c(i,sum(data$Language_family == i), lanFamilyPopulation, percent(percentPopulation, accuracy = 0.0001), percent(percentMale, accuracy = 0.0001), percent(percentFemale, accuracy = 0.0001))
  
  # Assign value to df_lang_Famly by using rbind 
  df_lang_Famly = rbind(df_lang_Famly, output)
}
# set name for column in df_lang_Famly
colnames(df_lang_Famly)<-c("language_family", "Minority", "Population", "Percentage (population)", "Percentage (Male)", "Percentage (Female)")

View(df_lang_Famly)

View(data)
sapply(data, class) 