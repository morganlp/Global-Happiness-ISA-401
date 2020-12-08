#ISA 401 Final Project
#Morgan Pohl
#Data Cleaning

#SET WORKING DIRECTORY

#Load Packages
library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)
pacman::p_load(tidyquant, wbstats, lubridate, jsonlite, magrittr, rvest)


#HAPPINESS DATA
#Load
happy = read.csv("HappinessData2020.csv")
#Select only necessary data
happy = happy[,1:9]
#Rename columns
names(happy) = c("Country","Year","Score","GDP/Capita","SocialSupport","LifeExpectancy","FreedomChoices","Generosity","Corruption")

#POPOULATION DATA
#Load
pop = read.csv("Population.csv", header= TRUE, skip = 4)
#Remove years for which there is no happiness data
pop = pop[,-(3:49)]
#Remove last 2 columns which hold no information
pop = pop[,-(18:19)]
#Rename columns
names(pop) = c("Country", "CountryCode", "2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
#Change data to long format
pop = pop %>%
  gather(key = "Year", value = "Population", 3:(length(names(pop))))
#Year as a numeric variable
pop$Year = as.numeric(pop$Year)
#Remove Country Codes
pop = pop[,-2]
#Fix Country names)
pop <- pop %>% 
  mutate(Country = case_when(
    str_detect(Country, "Congo, Rep.") ~ "Congo (Brazzaville)",
    str_detect(Country, "Congo, Dem. Rep.") ~ "Congo (Kinshasa)",
    str_detect(Country, "Egypt, Arab Rep.") ~ "Egypt",
    str_detect(Country, "Gambia, The") ~ "Gambia",
    str_detect(Country, "Hong Kong SAR, China") ~ "Hong Kong S.A.R. of China",
    str_detect(Country, "Iran, Islamic Rep.") ~ "Iran",
    str_detect(Country, "Cote d'Ivoire") ~ "Ivory Coast",
    str_detect(Country, "Kyrgyz Republic") ~ "Kyrgyzstan",
    str_detect(Country, "Lao PDR") ~ "Laos",
    str_detect(Country, "North Macedonia") ~ "Macedonia",
    str_detect(Country, "West Bank and Gaza") ~ "Palestinian Territories",
    str_detect(Country, "Russian Federation") ~ "Russia",
    str_detect(Country, "Slovak Republic") ~ "Slovakia",
    str_detect(Country, "Korea, Rep.") ~ "South Korea",
    str_detect(Country, "Eswatini") ~ "Swaziland",
    str_detect(Country, "Syrian Arab Republic") ~ "Syria",
    str_detect(Country, "Venezuela, RB") ~ "Venezuela",
    str_detect(Country, "Yemen, Rep.") ~ "Yemen",
    TRUE ~ Country))

#TOURISM
#Load
tourism = read_xlsx("Arrivals.xlsx", skip = 2)
#Remove columns containing row labels
tourism = tourism[,-(1:3)]  
#Remove Subtitles
tourism = tourism[,-2]
#Remove unnecessary years
tourism = tourism[,-(4:17)]
#Remove notes column
tourism = tourism[,-(19)]
#Remove subheadings
tourism = tourism[,-3]
#Remove blank rows
tourism = tourism[which(tourism$`Basic data and indicators`!= 'NA'|tourism$...6!='NA'),]
#Lag country name to move to same row as data
tourism = tourism %>%
  mutate(`Basic data and indicators`=lag(`Basic data and indicators`))
#Remove subheading
tourism = tourism[,-2]
#Remove blank rows
tourism = tourism[which(tourism$`Basic data and indicators`!= 'NA'),]
#Change to long formated data
tourism = tourism %>% 
  gather(key = "Year", value = "Arrivals", 2:(length(names(tourism))))
#Change arrivals to numeric
tourism$Arrivals=as.numeric(tourism$Arrivals)
#Remane columns
names(tourism) = c("Country", "Year", "Tourists")
#Year as numeric
tourism$Year = as.numeric(tourism$Year)
#Tourists are listed in the thousands
tourism$Tourists = tourism$Tourists * 1000
#Standardize Country Names
tourism <- tourism %>% 
  mutate(Country = case_when(
    str_detect(Country, "BOLIVIA, PLURINATIONAL STATE OF") ~ "BOLIVIA",
    str_detect(Country, "CONGO") ~ "CONGO (BRAZZAVILLE)",
    str_detect(Country, "CONGO, DEMOCRATIC REPUBLIC OF THE") ~ "CONGO (KINSHASA)",
    str_detect(Country, "HONG KONG, CHINA") ~ "HONG KONG S.A.R. OF CHINA",
    str_detect(Country, "IRAN, ISLAMIC REPUBLIC OF") ~ "IRAN",
    str_detect(Country, "COTE D´IVOIRE") ~ "IVORY COAST",
    str_detect(Country, "LAO PEOPLE´S DEMOCRATIC REPUBLIC") ~ "LAOS",
    str_detect(Country, "NORTH MACEDONIA") ~ "MACEDONIA",
    str_detect(Country, "MOLDOVA, REPUBLIC OF") ~ "MOLDOVA",
    str_detect(Country, "RUSSIAN FEDERATION") ~ "RUSSIA",
    str_detect(Country, "KOREA, REPUBLIC OF") ~ "SOUTH KOREA",
    str_detect(Country, "ESWATINI") ~ "SWAZILAND",
    str_detect(Country, "SYRIAN ARAB REPUBLIC") ~ "SYRIA",
    str_detect(Country, "TANZANIA, UNITED REPUBLIC OF") ~ "TANZANIA",
    str_detect(Country, "UNITED STATES OF AMERICA") ~ "UNITED STATES",
    str_detect(Country, "VENEZUELA, BOLIVARIAN REPUBLIC OF") ~ "VENEZUELA",
    str_detect(Country, "VIET NAM") ~ "VIETNAM",
    TRUE ~ Country))

#EXPORTS
#Load
export = read.csv("Exports.csv")
#Select Total exports for individual countries
export = export[which(export$Reporting.Economy != "World"),]
export = export[which(export$Partner.Economy == "World"),]
export = export[which(export$Product.Sector == "Total merchandise"),]
export = export[which(export$Indicator.Code == "ITS_MTV_AX"),]
#Remove category indicators
export = export[,-(1:4)]
#Remove partner economy information
export = export[,-(3:15)]
#Remove variable descriptions
export = export[,-(4:6)]
#Rename columns
names(export) = c("CountryCode", "Country", "Year", "Export(USD)")
#Exports reported in millions of US Dollars
export$`Export(USD)` = export$`Export(USD)`*1000000
#Remove country codes
export = export[,-1]
#Countries reported in uppercase
export$Country = toupper(export$Country)
#Standardize Country Names
export <- export %>% 
  mutate(Country = case_when(
    str_detect(Country, "BAHRAIN, KINGDOM OF") ~ "BAHRAIN",
    str_detect(Country, "BOLIVIA, PLURINATIONAL STATE OF") ~ "BOLIVIA",
    str_detect(Country, "CONGO") ~ "CONGO (BRAZZAVILLE)",
    str_detect(Country, "DEMOCRATIC REPUBLIC OF THE CONGO") ~ "CONGO (KINSHASA)",
    str_detect(Country, "THE GAMBIA") ~ "GAMBIA",
    str_detect(Country, "HONG KONG, CHINA") ~ "HONG KONG S.A.R. OF CHINA",
    str_detect(Country, "CÔTE D'IVOIRE") ~ "IVORY COAST",
    str_detect(Country, "KUWAIT, THE STATE OF") ~ "KUWAIT",
    str_detect(Country, "KYRGYZ REPUBLIC") ~ "KYRGYZSTAN",
    str_detect(Country, "LAO PEOPLE'S DEMOCRATIC REPUBLIC") ~ "LAOS",
    str_detect(Country, "LEBANESE REPUBLIC") ~ "LEBANON",
    str_detect(Country, "NORTH MACEDONIA") ~ "MACEDONIA",
    str_detect(Country, "MOLDOVA, REPUBLIC OF") ~ "MOLDOVA",
    str_detect(Country, "RUSSIAN FEDERATION") ~ "RUSSIA",
    str_detect(Country, "SAUDI ARABIA, KINGDOM OF") ~ "SAUDI ARABIA",
    str_detect(Country, "SLOVAK REPUBLIC") ~ "SLOVAKIA",
    str_detect(Country, "KOREA, REPUBLIC OF") ~ "SOUTH KOREA",
    str_detect(Country, "ESWATINI") ~ "SWAZILAND",
    str_detect(Country, "SYRIAN ARAB REPUBLIC") ~ "SYRIA",
    str_detect(Country, "UNITED STATES OF AMERICA") ~ "UNITED STATES",
    str_detect(Country, "VENEZUELA, BOLIVARIAN REPUBLIC OF") ~ "VENEZUELA",
    str_detect(Country, "VIET NAM") ~ "VIETNAM",
    TRUE ~ Country))

#IMMIGRATION
#Load
imm = read_xlsx("UNImmigration.xlsx", sheet = 2, skip = 15)
#Keep only total immigration information
imm = imm[,1:7]
#Rename columns
names(imm) = c("Year", "SortOrder", "Country", "Notes", "Code", "TypeOfData", "Immigration")
#Remove individual identifier
imm = imm[,-(2)]
#Remove data notes
imm = imm[,-(3:5)]
#Select only data from after 2005
imm = imm[which(imm$Year > 2005),]
#Remove non-country-specific rows
imm = imm[-(1:21),]
imm = imm[-(263:283),]
imm = imm[-(525:545),]
#Countries as uppercase
imm$Country = toupper(imm$Country)
#Standardize Country Names
imm <- imm %>% 
  mutate(Country = case_when(
    str_detect(Country, "BOLIVIA (PLURINATIONAL STATE OF)") ~ "BOLIVIA",
    str_detect(Country, "CONGO") ~ "CONGO (BRAZZAVILLE)",
    str_detect(Country, "DEMOCRATIC REPUBLIC OF THE CONGO") ~ "CONGO (KINSHASA)",
    str_detect(Country, "CZECHIA") ~ "CZECH REPUBLIC",
    str_detect(Country, "IRAN (ISLAMIC REPUBLIC OF)") ~ "IRAN",
    str_detect(Country, "CÔTE D'IVOIRE") ~ "IVORY COAST",
    str_detect(Country, "LAO PEOPLE'S DEMOCRATIC REPUBLIC") ~ "LAOS",
    str_detect(Country, "NORTH MACEDONIA") ~ "MACEDONIA",
    str_detect(Country, "REPUBLIC OF MOLDOVA") ~ "MOLDOVA",
    str_detect(Country, "RUSSIAN FEDERATION") ~ "RUSSIA",
    str_detect(Country, "REPUBLIC OF KOREA") ~ "SOUTH KOREA",
    str_detect(Country, "ESWATINI") ~ "SWAZILAND",
    str_detect(Country, "SYRIAN ARAB REPUBLIC") ~ "SYRIA",
    str_detect(Country, "UNITED REPUBLIC OF TANZANIA") ~ "TANZANIA",
    str_detect(Country, "UNITED STATES OF AMERICA") ~ "UNITED STATES",
    str_detect(Country, "VENEZUELA (BOLIVARIAN REPUBLIC OF)") ~ "VENEZUELA",
    str_detect(Country, "VIET NAM") ~ "VIETNAM",
    TRUE ~ Country))

#JOIN DATA

#Join Happiness and Population
happiness = left_join(happy, pop, by = c("Country"="Country","Year" = "Year"))
happiness$Country = toupper(happiness$Country)

#Join Happiness and Tourism
happiness2 = left_join(happiness, tourism, by = c("Country" = "Country", "Year" = "Year"))

#Join Happiness and Exports
happiness3 = left_join(happiness2, export, by = c("Country" =  "Country", "Year" = "Year"))

#Join Happiness and Immigration
happiness4 = left_join(happiness3, imm, by = c("Country" = "Country", "Year" = "Year"))

#Clean Out Duplicate Rows 
#Congo, South Korea, Venezuela
happiness4 = happiness4[-c(360,362,364,366:368,370:372,374:380,382:384,386:388,390:392,394:400,1537,1543,1548,1825,1827,1829,1831,1833,1836,1837),]

#Export Final Data
final_data = happiness4
#write.csv(final_data, "M:/ISA 401/Final//FinalData.csv")


#Descriptive Statistics
#final_data = read.csv("FinalData.csv")

#Summary of the data
summary(final_data)
#Plot correlations of data
library(corrplot)
#Remove categorical variables
data = final_data[,-(1:2)]
correlation = cor(data, use = "complete.obs")
corrplot(correlation, type ="upper")
#Distribution of key variables
hist(final_data$Score,main = "Histogram of Happiness Scores",xlab = "Happiness Scores", col = "red")
hist(final_data$Export.USD.,breaks = 100, main = "Histogram of Exports",xlab = "Exports (USD)", col = "red")
hist(final_data$Immigration,breaks = 100, main = "Histogram of Immigration",xlab = "Immigrants", col = "red")
hist(final_data$Tourists,breaks = 100, main = "Histogram of Tourism",xlab = "Tourists", col = "red")
hist(final_data$Population,breaks = 100, main = "Histogram of Population",xlab = "Population", col = "red")
#Calculate GDP per Capita before making histogram
final = final_data
final$GDP = 10^final_data$GDP.Capita
hist(final$GDP,breaks = 100, main = "Histogram of GDP",xlab = "GDP per Capita", col = "red")
