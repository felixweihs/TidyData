library(tidyverse)
library(cowplot)
library(gganimate)
library(gifski)
library(png)
theme_set(theme_bw())

#Load files with unknown names from known folder into R studio
FileList <- list.files(path = "data", pattern="*.csv", full.names = TRUE)
AllData <- map_df(FileList, read_csv, skip = 7, .id = "dataset") 

#Replace Ratio calculation by more exact measurements and group by datasets
AllData_tidy <- AllData %>% 
  mutate(Ratio = as.numeric(Green) / as.numeric(Blue)) %>% 
  mutate(dataset = as.numeric(dataset)) %>% 
  group_by(dataset)

#Analysis
analysis_5 <- subset(AllData_tidy, Time >=300 & Time <= 320) %>% 
      summarise(five_min = mean(Ratio))
analysis_6 <- subset(AllData_tidy, Time >=360 & Time <= 380) %>% 
  summarise(six_min = mean(Ratio))
analysis_7 <- subset(AllData_tidy, Time >=420 & Time <= 440) %>% 
  summarise(seven_min = mean(Ratio))
analysis_8 <- subset(AllData_tidy, Time >=480 & Time <= 500) %>% 
  summarise(eight_min = mean(Ratio))
analysis <- full_join(analysis_5, analysis_6)
analysis <- full_join(analysis, analysis_7)
analysis <- full_join(analysis, analysis_8)

#Mutate datasets into groups
analysis_summary <- analysis %>% 
  mutate(dataset = case_when(
      dataset == "1" ~ "1",
      dataset == "2" ~ "1",
      dataset == "3" ~ "1",
      dataset == "4" ~ "2",
      dataset == "5" ~ "2",
      dataset == "6" ~ "2",
      dataset == "7" ~ "3",
      dataset == "8" ~ "3",
      dataset == "9" ~ "3",
      dataset == "10" ~ "4",
      dataset == "11" ~ "4",
      dataset == "12" ~ "4")) %>% 
  group_by(dataset)

## Putting BRET ratios together and calculate means and SD
analysis_summary5 <- summarise(analysis_summary, BRET_ratio = mean(five_min), SD = sd(five_min)) %>% add_column(minutes = 5)
analysis_summary6 <- summarise(analysis_summary, BRET_ratio = mean(six_min), SD = sd(six_min)) %>% add_column(minutes = 6)
analysis_summary7 <- summarise(analysis_summary, BRET_ratio = mean(seven_min), SD = sd(seven_min)) %>% add_column(minutes = 7)
analysis_summary8 <- summarise(analysis_summary, BRET_ratio = mean(eight_min), SD = sd(eight_min)) %>% add_column(minutes = 8)

BRETratio_summary <- full_join(analysis_summary5, analysis_summary6)
BRETratio_summary <- full_join(BRETratio_summary, analysis_summary7)
BRETratio_summary <- full_join(BRETratio_summary, analysis_summary8)

## Plotting - Means and error bar
##


analysis_summary2 <- analysis_summary %>% 
  gather(group, value, -dataset) %>% 
  mutate(group = case_when(
    group == "five_min" ~ "5",
    group == "six_min" ~ "6",
    group == "seven_min" ~ "7",
    group == "eight_min" ~ "8")) %>% 
  group_by(dataset, group)

rm(Stats1, Stats2)
Stats1 <- summarise(analysis_summary2, mean = mean(value, na.rm=TRUE), SD = sd(value, na.rm=TRUE))
Stats2 <-  analysis_summary2 %>% summarise_each(funs(sum(!is.na(.))))
Stats_table <- full_join(Stats1, Stats2) %>% rename('incubation (min)' = group, 'n' = value) %>% ungroup()


analysis_summary2_lm <- lm(value ~ as.numeric(group), data = analysis_summary2)

graph_analysis2 <- ggplot(analysis_summary2, aes(x = as.numeric(group), y = value, colour = dataset, group = dataset)) +
          geom_point(alpha = 0.5) +
          stat_smooth(method = "lm") + ylim(0,1) +
          labs(x = "Incubation time (min)", y = "BRET ratio") +
          theme(axis.title.x = element_text(size = 14, face = "bold"),
                axis.title.y = element_text(size = 14, face = "bold"),
                legend.title = element_text(size = 20, face = "bold"),
                legend.text = element_text(size = 12, face = "bold"),
                axis.text = element_text(size = 14, face = "bold"),
                legend.key = element_rect(fill = "white"))
graph_analysis2
#Tidy bioluminescence and ratio data
AllData_tidy_signal <- AllData_tidy %>% 
  select(1:4) %>% 
  gather(colour, Signal, -dataset, -Time)

AllData_tidy_ratio <- AllData_tidy %>% 
  select(1,2,5) %>% 
  gather(colour, Ratio, -dataset, -Time)



# Produce graphs with bioluminescence data as a function of time
graph_plot_signal <- ggplot(AllData_tidy_signal, 
       aes(x=as.numeric(Time), y=as.numeric(Signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(face="bold", size=10, colour = "black"),
        strip.background = element_rect(fill="white", colour="black",size=2)) +
  ylim(0, 30000) +
  xlim(0, 600) +
  facet_wrap(~dataset, nrow = 1)

# Produce graphs with BRET ratio data as a function of time
graph_plot_ratio <- ggplot(AllData_tidy_ratio, 
                            aes(x=as.numeric(Time), y=as.numeric(Ratio))) + 
  geom_point() +
  labs(title="", x="time [sec]", y = "BRET Ratio (Green/Blue)") +
  scale_colour_manual(values = c("black")) +
  theme_light() +
  theme(legend.position = "none",
        strip.text = element_text(face="bold", size=10, colour = "black"),
        strip.background = element_rect(fill="white", colour="black",size=2)) +
  ylim(0, 1) +
  xlim(0, 600) +
  facet_wrap(~dataset, nrow = 1)

# Connect signal and ratio graphs
graph_plot_raw <- plot_grid(graph_plot_signal, graph_plot_ratio, 
                            ncol = 1, nrow = 2,
                            rel_heights = c(1,1))
graph_plot_raw





# Create gif for poster and presentation showing 'live' signal and ratio over time
AllData_tidy_signal2 <- AllData_tidy_signal %>% 
  filter(dataset == "6") %>% group_by(colour)

AllData_tidy_ratio2 <- AllData_tidy_ratio%>% 
  filter(dataset == "6")

graph_plot_signal2 <- ggplot(
  AllData_tidy_signal2,
  aes(x=as.numeric(Time), y=as.numeric(Signal), colour = colour)) +
  geom_line(size = 2) +
  scale_colour_manual(values = c("blue", "#31a354")) +
  labs(x = "time (sec)", y = "Bioluminescence signal (A.U.)") +
  theme(legend.position = "top") +
  ylim(0, 25000) +
  xlim(0, 600) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size=60),
        axis.text.y = element_text(size=50))
graph_plot_signal2

graph_plot_ratio2 <- ggplot(
  AllData_tidy_ratio2,
  aes(x=as.numeric(Time), y=as.numeric(Ratio))) +
  geom_line(size = 2) +
  scale_colour_manual(values = c("black")) +
  labs(x = "time (sec)", y = "BRET ratio") +
  theme(legend.position = "bottom",
        text = element_text(size=60),
        axis.text.x = element_text(size=50),
        axis.text.y = element_text(size=50)) +
  ylim(0, 1) +
  xlim(0, 600)
graph_plot_ratio2

newanime <- graph_plot_signal2 + 
  transition_reveal(Time)
newanime2 <- graph_plot_ratio2 + 
  transition_reveal(Time)

newanime_test <- animate(newanime, height = 800, width =1600, fps = 5)
newanime_test2 <- animate(newanime2, height = 800, width =1600, fps = 5)

anim_save("Results/signal_anime.gif", animation = newanime_test)
anim_save("Results/ratio_anime.gif", animation = newanime_test2)

#Save files
ggsave("Results/raw_data.png", plot = graph_plot_raw, height = 10, width = 14)
ggsave("Results/toprow_plot.png", plot = toprow_plot, height = 8, width = 14)

#6 visualisations with the entire data
AllData_tidy_actual <- AllData_tidy %>% 
  gather(colour, Signal, -dataset, -Time) %>% 
  group_by(dataset)

#Outdated results graph
graph_analysis <- ggplot(BRETratio_summary, aes(x=minutes, y=BRET_ratio, fill = dataset)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=BRET_ratio, ymax=BRET_ratio+SD), width=0.2,
                position=position_dodge(.9)) +
  labs(title="BRET Ratio comparison", x="Incubation time [min]", y = "BRET Ratio") +
  theme_bw() +
  scale_fill_manual(values=c('#999999','#E69F00'))