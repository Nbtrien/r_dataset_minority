data <- read.csv("D://Năm 4//Kỳ 1//Chuyên đề 5//Code R//GiuaKy//r_dataset_minority//minority.csv")

library(scales)

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


#4 - Tạo biến mới dựa trên biến đã có
# Tính phần trăm dân số của từng dân tộc thiểu số
# tính tổng dân số của dân tộc thiểu số
totalPopulation = sum(data$Population)
totalPopulation
# tạo cột mới 'Population Percent' tính phần trăm dân số từng dân tộc 
data$`Population_Percent` = (data$Population/totalPopulation)

# đổi định dạng sang phần trăm
data$`Population_Percent` = percent(data$`Population_Percent`, accuracy = 0.0001)

#5 - Đặt câu hỏi nghiên cứu vấn đề cần biết trong dataset
# Tính mỗi vùng miền có tổng số bao nhiêu dân, chiếm bao nhiêu phần trăm
# đếm có bao nhiêu dân tộc của mỗi vùng miền
region = table(d$Region)
region
# lấy tên các vùng miền
arrRegionNames = names(region)
arrRegionNames
# tạo dataframe mới
df = data.frame()

# dùng vòng lặp lấy tên các vùng miền để tính tổng dân số của vùng miền đó
for (i in arrRegionNames) {
  # tính tổng dân số của vùng miền theo tên miền
  regionPopulation = sum(data$Population[d$Region == i])
  # tính phần trăm dân số của vùng miền theo tên miền
  pRegionPopulation = regionPopulation/totalPopulation
  # gán output của mỗi lần lặp với từng giá trị
  output = c(i, regionPopulation, percent(pRegionPopulation, accuracy = 0.0001))
  # sử dụng rbind để nối đầu ra của lần lặp vào khung dữ liệu
  df = rbind(df, output)
}
# đặt tên cột cho dataframe
colnames(df)<-c("Region", "Population", "Percent Population")
# hiển thị dataframe
df



