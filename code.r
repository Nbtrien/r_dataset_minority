data <- read.csv("F:\\[4 - HKI]Chuyên đề 5\\minority.csv")

colnames(data)
# change column name
data <- setNames(data, c("Id","Order","Minority_Name","Self_title", "Other_names","International_names","Local_group","Distribution","Population","Language","Language_group","Language_branch","Language_family","Region","General_information_about_traditional_costumes","Note"))

# change available value to empty
#data$Local_group<-replace(data$Local_group,data$Local_group == "Not avaible","")
#data$Other_names<-replace(data$Other_names,data$Other_names == "Not avaible","")
data<-replace(data,data == "Not avaible","")

# empty to NA value
data[data == ""] <- NA   

#delete note column
data <- data[,!names(data) %in% c("Note")]
data <- data[,!names(data) %in% c("Language_group")]

data$Language_branch[data$Language_branch %in% c("Thai", "Thai, Sino")] <- "Tay-Thai" 
data$Language_branch[data$Language_branch %in% c("Kra")] <- "Kadai" 
data$Language_branch[data$Language_branch %in% c("Mon Khmer", "Mon - Khmer")] <- "Mon-Khmer"
data$Language_branch[data$Language_branch %in% c("Vietnamese")] <- "Viet-Muong"
data$Language_branch[(is.na(data$Language_branch)) & (data$Language_family == "H'mong -Mien")] <- "H'Mong"
data$Language_branch[is.na(data$Language_branch)] <- "Kadai"

data$Language_family[data$Language_family %in% c("Autro-Asiatic")] <- "Austro-Asiatic"
data$Language_family[data$Language_family %in% c("Tai-Kadai, Sino-Tibetan")] <- "Tai-Kadai"

#lang_branch = NA
#lang_branch <- table(data$Language_branch)
#lang_branch

#lang_fam <- table(data$Language_family)
#lang_fam

#lang_data = NA
#lang_data <- data.frame(data$Minority_Name, data$Language_branch)
#View(lang_data)

language_family_table <- table(data$Language_family)
language_family_table

language_family = names(language_family_table)



for(i in 1:length(language_family)){
  print(language_family[i])
  mi_name <- data$Minority_Name[data$Language_family == language_family[i]]
  print(mi_name)
  cat("\n")
}



View(data)
summary(data)



sapply(data, class)




