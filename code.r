library(scales)

data <- read.csv("F:\\[4 - HKI]Chuyên đề 5\\middle_term\\minorityclone.csv")
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

lang_family = table(data$Language_family)
lang_family

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
region = table(data$Region)
region
# lấy tên các vùng miền
arrRegionNames = names(region)
arrRegionNames
# tạo dataframe mới
df = data.frame()

# dùng vòng lặp lấy tên các vùng miền để tính tổng dân số của vùng miền đó
for (i in arrRegionNames) {
  # tính tổng dân số của vùng miền theo tên miền
  regionPopulation = sum(data$Population[data$Region == i])
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
  pRegionPopulation = lanFamilyPopulation/totalPopulation
  
  # Assign value to output 
  output = c(i,sum(data$Language_family == i), lanFamilyPopulation, percent(pRegionPopulation, accuracy = 0.0001))
  
  # Assign value to df_lang_Famly by using rbind 
  df_lang_Famly = rbind(df_lang_Famly, output)
}
# set name for column in df_lang_Famly
colnames(df_lang_Famly)<-c("language_family", "Minority", "Population", "Percent Population")
# print df_lang_Famly
df_lang_Famly

View(data)
