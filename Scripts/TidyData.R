library(tidyverse)
library(cowplot)

#Load files with unknown names from known folder into R studio
FileList <- list.files("data", pattern="*.csv")
setwd("data")
AllData <- lapply(FileList, read_csv)
setwd("C:/#IRS-BRET/Conferences and Workshops/DataSchool FOCUS/Projects/TidyData")

#Extract individual dataframes from nested lists
DataFrameA <- map_df(AllData[1], ~.x) 
DataFrameB <- map_df(AllData[2], ~.x)
DataFrameC <- map_df(AllData[3], ~.x)
DataFrameD <- map_df(AllData[4], ~.x)
DataFrameE <- map_df(AllData[3], ~.x)
DataFrameF <- map_df(AllData[4], ~.x)

#Tidy individual dataframes
DataFrameA_tidy <- DataFrameA %>% 
  slice(8:2000) %>% #Remove annotations that were automatically created by the CYBERTONGUE
  select(1:3) %>% #Remove ratios column -> Inaccurate values
  rename(time = 1, green = 2, blue = 3) %>% #Rename columns into sensible headers
  mutate(BRET_ratio = as.numeric(green) / as.numeric(blue)) #Calculate and add column with BRET ratios

DataFrameB_tidy <- DataFrameB %>% 
  slice(8:2000) %>% #Remove annotations that were automatically created by the CYBERTONGUE
  select(1:3) %>% #Remove ratios column -> Inaccurate values
  rename(time = 1, green = 2, blue = 3) %>% #Rename columns into sensible headers
  mutate(BRET_ratio = as.numeric(green) / as.numeric(blue)) #Calculate and add column with BRET ratios

DataFrameC_tidy <- DataFrameC %>% 
  slice(8:2000) %>% #Remove annotations that were automatically created by the CYBERTONGUE
  select(1:3) %>% #Remove ratios column -> Inaccurate values
  rename(time = 1, green = 2, blue = 3) %>% #Rename columns into sensible headers
  mutate(BRET_ratio = as.numeric(green) / as.numeric(blue)) #Calculate and add column with BRET ratios

DataFrameD_tidy <- DataFrameD %>% 
  slice(8:2000) %>% #Remove annotations that were automatically created by the CYBERTONGUE
  select(1:3) %>% #Remove ratios column -> Inaccurate values
  rename(time = 1, green = 2, blue = 3) %>% #Rename columns into sensible headers
  mutate(BRET_ratio = as.numeric(green) / as.numeric(blue)) #Calculate and add column with BRET ratios

DataFrameE_tidy <- DataFrameE %>% 
  slice(8:2000) %>% #Remove annotations that were automatically created by the CYBERTONGUE
  select(1:3) %>% #Remove ratios column -> Inaccurate values
  rename(time = 1, green = 2, blue = 3) %>% #Rename columns into sensible headers
  mutate(BRET_ratio = as.numeric(green) / as.numeric(blue)) #Calculate and add column with BRET ratios

DataFrameF_tidy <- DataFrameF %>% 
  slice(8:2000) %>% #Remove annotations that were automatically created by the CYBERTONGUE
  select(1:3) %>% #Remove ratios column -> Inaccurate values
  rename(time = 1, green = 2, blue = 3) %>% #Rename columns into sensible headers
  mutate(BRET_ratio = as.numeric(green) / as.numeric(blue)) #Calculate and add column with BRET ratios

#Several BRET ratio calculation in one dataframe for individual dataframes DataFrameA
DataFrameA_analysis_5 <- DataFrameA_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 600, row_number() <= 640) %>%  
  summarise(five_min = mean(BRET_ratio)) 
DataFrameA_analysis_6 <- DataFrameA_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 720, row_number() <= 760) %>%  
  summarise(six_min = mean(BRET_ratio)) 
DataFrameA_analysis_7 <- DataFrameA_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 840, row_number() <= 880) %>%  
  summarise(seven_min = mean(BRET_ratio)) 

#Several BRET ratio calculation in one dataframe for individual dataframes DataFrameB
DataFrameB_analysis_5 <- DataFrameB_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 600, row_number() <= 640) %>%  
  summarise(five_min = mean(BRET_ratio)) 
DataFrameB_analysis_6 <- DataFrameB_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 720, row_number() <= 760) %>%  
  summarise(six_min = mean(BRET_ratio)) 
DataFrameB_analysis_7 <- DataFrameB_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 840, row_number() <= 880) %>%  
  summarise(seven_min = mean(BRET_ratio)) 

#Several BRET ratio calculation in one dataframe for individual dataframes DataFrameC
DataFrameC_analysis_5 <- DataFrameC_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 600, row_number() <= 640) %>%  
  summarise(five_min = mean(BRET_ratio)) 
DataFrameC_analysis_6 <- DataFrameC_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 720, row_number() <= 760) %>%  
  summarise(six_min = mean(BRET_ratio)) 
DataFrameC_analysis_7 <- DataFrameC_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 840, row_number() <= 880) %>%  
  summarise(seven_min = mean(BRET_ratio)) 

#Several BRET ratio calculation in one dataframe for individual dataframes DataFrameD
DataFrameD_analysis_5 <- DataFrameD_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 600, row_number() <= 640) %>%  
  summarise(five_min = mean(BRET_ratio)) 
DataFrameD_analysis_6 <- DataFrameD_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 720, row_number() <= 760) %>%  
  summarise(six_min = mean(BRET_ratio)) 
DataFrameD_analysis_7 <- DataFrameD_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 840, row_number() <= 880) %>%  
  summarise(seven_min = mean(BRET_ratio)) 

#Several BRET ratio calculation in one dataframe for individual dataframes DataFrameD
DataFrameE_analysis_5 <- DataFrameE_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 600, row_number() <= 640) %>%  
  summarise(five_min = mean(BRET_ratio)) 
DataFrameE_analysis_6 <- DataFrameE_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 720, row_number() <= 760) %>%  
  summarise(six_min = mean(BRET_ratio)) 
DataFrameE_analysis_7 <- DataFrameE_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 840, row_number() <= 880) %>%  
  summarise(seven_min = mean(BRET_ratio)) 

#Several BRET ratio calculation in one dataframe for individual dataframes DataFrameD
DataFrameF_analysis_5 <- DataFrameF_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 600, row_number() <= 640) %>%  
  summarise(five_min = mean(BRET_ratio)) 
DataFrameF_analysis_6 <- DataFrameF_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 720, row_number() <= 760) %>%  
  summarise(six_min = mean(BRET_ratio)) 
DataFrameF_analysis_7 <- DataFrameF_tidy %>% 
  select(time, BRET_ratio) %>% 
  filter(row_number() >= 840, row_number() <= 880) %>%  
  summarise(seven_min = mean(BRET_ratio)) 

## Putting BRET ratios together and calculate means and STD
##
BRETratio_1 <- data.frame(DataFrameA_analysis_5, DataFrameA_analysis_6, DataFrameA_analysis_7)
BRETratio_2 <- data.frame(DataFrameB_analysis_5, DataFrameB_analysis_6, DataFrameB_analysis_7)
BRETratio_3 <- data.frame(DataFrameC_analysis_5, DataFrameC_analysis_6, DataFrameC_analysis_7)
BRETratio_4 <- data.frame(DataFrameD_analysis_5, DataFrameD_analysis_6, DataFrameD_analysis_7)
BRETratio_5 <- data.frame(DataFrameE_analysis_5, DataFrameE_analysis_6, DataFrameE_analysis_7)
BRETratio_6 <- data.frame(DataFrameF_analysis_5, DataFrameF_analysis_6, DataFrameF_analysis_7)

BRETratios1 <- full_join(BRETratio_1, BRETratio_2) # Merging all data into same dataframe
BRETratios1 <- full_join(BRETratios, BRETratio_3)
BRETratios2 <- full_join(BRETratio_4, BRETratio_5)
BRETratios2 <- full_join(BRETratios2, BRETratio_6)

BRETratio_summary5min1 <- data.frame(BRET_ratio = mean(BRETratios[[1]], na.rm=TRUE), sd = sd(BRETratios[[1]], na.rm=TRUE), group = "1", minutes = 5)  #Calculate and store means with SDs
BRETratio_summary5min2 <- data.frame(BRET_ratio = mean(BRETratios2[[1]], na.rm=TRUE), sd = sd(BRETratios2[[1]], na.rm=TRUE), group = "2", minutes = 5)
BRETratio_summary6min1 <- data.frame(BRET_ratio = mean(BRETratios[[2]], na.rm=TRUE), sd = sd(BRETratios[[2]], na.rm=TRUE), group = "1", minutes = 6)  #Calculate and store means with SDs
BRETratio_summary6min2 <- data.frame(BRET_ratio = mean(BRETratios2[[2]], na.rm=TRUE), sd = sd(BRETratios2[[2]], na.rm=TRUE), group = "2", minutes = 6)
BRETratio_summary7min1 <- data.frame(BRET_ratio = mean(BRETratios[[3]], na.rm=TRUE), sd = sd(BRETratios[[3]], na.rm=TRUE), group = "1", minutes = 7)  #Calculate and store means with SDs
BRETratio_summary7min2 <- data.frame(BRET_ratio = mean(BRETratios2[[3]], na.rm=TRUE), sd = sd(BRETratios2[[3]], na.rm=TRUE), group = "2", minutes = 7)



BRETratio_summary <- full_join(BRETratio_summary5min1, BRETratio_summary5min2)
BRETratio_summary <- full_join(BRETratio_summary, BRETratio_summary6min1)
BRETratio_summary <- full_join(BRETratio_summary, BRETratio_summary6min2)
BRETratio_summary <- full_join(BRETratio_summary, BRETratio_summary7min1)
BRETratio_summary <- full_join(BRETratio_summary, BRETratio_summary7min2)

## Plotting - Means and error bar
##
graph_analysis <- ggplot(BRETratio_summary, aes(x=minutes, y=BRET_ratio, fill = group)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=BRET_ratio, ymax=BRET_ratio+sd), width=0.2,
                position=position_dodge(.9)) +
  labs(title="BRET Ratio comparison", x="Incubation time [min]", y = "BRET Ratio") +
  theme_bw() +
  scale_fill_manual(values=c('#999999','#E69F00'))

## Plotting - Means and error bar
# First, DataFrame_tidy has to be tidied -> tidier!
DataFrameA_tidier <- DataFrameA_tidy %>% 
  gather(colour, signal, -time)
DataFrameB_tidier <- DataFrameB_tidy %>% 
  gather(colour, signal, -time)
DataFrameC_tidier <- DataFrameC_tidy %>% 
  gather(colour, signal, -time)
DataFrameD_tidier <- DataFrameD_tidy %>% 
  gather(colour, signal, -time)
DataFrameE_tidier <- DataFrameE_tidy %>% 
  gather(colour, signal, -time)
DataFrameF_tidier <- DataFrameF_tidy %>% 
  gather(colour, signal, -time)

# Produce graphs with bioluminescence data as a function of time
Graph_A_1 <- ggplot(subset(DataFrameA_tidier, colour=="blue" | colour=="green"), 
       aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme(legend.position = "none") +
  ylim(0, 40000)

Graph_B_1 <- ggplot(subset(DataFrameB_tidier, colour=="blue" | colour=="green"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme(legend.position = "none") +
  ylim(0, 40000)

Graph_C_1 <- ggplot(subset(DataFrameC_tidier, colour=="blue" | colour=="green"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme(legend.position = "none") +
  ylim(0, 40000)

Graph_D_1 <- ggplot(subset(DataFrameD_tidier, colour=="blue" | colour=="green"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme(legend.position = "none") +
  ylim(0, 40000)

Graph_E_1 <- ggplot(subset(DataFrameE_tidier, colour=="blue" | colour=="green"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme(legend.position = "none") +
  ylim(0, 40000)

Graph_F_1 <- ggplot(subset(DataFrameF_tidier, colour=="blue" | colour=="green"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme(legend.position = "none") +
  ylim(0, 40000)

# Produce graphs with BRET ratio data as a function of time

Graph_A_2 <- ggplot(subset(DataFrameA_tidier, colour=="BRET_ratio"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "BRET ratio") +
  scale_colour_manual(values = "black") + 
  theme(legend.position = "none") +
  ylim(0, 1.5)

Graph_B_2 <- ggplot(subset(DataFrameB_tidier, colour=="BRET_ratio"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "BRET ratio") +
  scale_colour_manual(values = "black") + 
  theme(legend.position = "none") +
  ylim(0, 1.5)

Graph_C_2 <- ggplot(subset(DataFrameC_tidier, colour=="BRET_ratio"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "BRET ratio") +
  scale_colour_manual(values = "black") + 
  theme(legend.position = "none") +
  ylim(0, 1.5)



plot_grid(Graph_A_1, Graph_A_2, Graph_B_1, Graph_B_2, Graph_C_1, Graph_C_2,
          labels = c("A", "","B", "","C", ""),
          nrow = 2,
          rel_heights = c(1.5,1,1.5,1,1.5,1)
          )


