#This script contains all code that I experimented with but didn't make it to the final version
#Variable names here are NOT compatible with each other

#Read in individual files
DataSetA <- read_csv("Data/DataSetA.csv")
DataSetB <- read_csv("Data/DataSetB.csv")
DataSetC <- read_csv("Data/DataSetC.csv")
DataSetD <- read_csv("Data/DataSetD.csv")

#Tidy Individual DataSet
DataSetA_tidy <- DataSetA %>% 
  slice(8:2000) %>% #Remove annotations that were automatically created by the CYBERTONGUE
  select(1:3) %>% #Remove ratios column -> Inaccurate values
  rename(time = 1, green = 2, blue = 3) %>% #Rename columns into sensible headers
  mutate(BRET_ratio = as.numeric(green) / as.numeric(blue)) #Calculate and add column with BRET ratios
DataSetB_tidy <- DataSetB %>% 
  slice(8:2000) %>% #Remove annotations that were automatically created by the CYBERTONGUE
  select(1:3) %>% #Remove ratios column -> Inaccurate values
  rename(time = 1, green = 2, blue = 3) %>% #Rename columns into sensible headers
  mutate(BRET_ratio = as.numeric(green) / as.numeric(blue)) #Calculate and add column with BRET ratios

#5 min BRET ratio of individual dataframes
DataFrameA_analysis <- DataFrameA_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 600) %>% 
  filter(row_number() <= 41) %>% 
  summarise(mean(BRET_ratio))

#Create and save tidy function
tidy_function <- function(x) {
  x %>% 
    slice(8:2000) %>%
    select(1:3) %>% 
    rename(time = 1, green = 2, blue = 3) %>% 
    mutate(BRET_ratio = as.numeric(green) / as.numeric(blue))
}

#Calculate BRET ratios
for (i in 1:3) {
  if (i == 1) {
    analysis_A <- subset(AllData_tidy, time >=300 & time <= 320 ) %>% 
      summarise(five_min = mean(BRET_ratio))}
  if (i == 2) {
    DataFrameA_analysis[2] <- subset(DataFrameA_tidy, time >=360 & time <= 380) %>% 
      summarise(six_min = mean(BRET_ratio))}
  if (i == 3) {
    DataFrameA_analysis[3] <- subset(DataFrameA_tidy, time >=420 & time <= 440) %>% 
      summarise(seven_min = mean(BRET_ratio))}
}

