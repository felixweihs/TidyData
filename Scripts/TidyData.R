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
analysis <- full_join(analysis_5, analysis_6)
analysis <- full_join(analysis, analysis_7)

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
      dataset == "9" ~ "3")) %>% 
  group_by(dataset)

## Putting BRET ratios together and calculate means and SD
analysis_summary5 <- summarise(analysis_summary, BRET_ratio = mean(five_min), SD = sd(five_min)) %>% add_column(minutes = 5)
analysis_summary6 <- summarise(analysis_summary, BRET_ratio = mean(six_min), SD = sd(six_min)) %>% add_column(minutes = 6)
analysis_summary7 <- summarise(analysis_summary, BRET_ratio = mean(seven_min), SD = sd(seven_min)) %>% add_column(minutes = 7)

BRETratio_summary <- full_join(analysis_summary5, analysis_summary6)
BRETratio_summary <- full_join(BRETratio_summary, analysis_summary7)

## Plotting - Means and error bar
##
graph_analysis <- ggplot(BRETratio_summary, aes(x=minutes, y=BRET_ratio, fill = dataset)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=BRET_ratio, ymax=BRET_ratio+SD), width=0.2,
                position=position_dodge(.9)) +
  labs(title="BRET Ratio comparison", x="Incubation time [min]", y = "BRET Ratio") +
  theme_bw() +
  scale_fill_manual(values=c('#999999','#E69F00'))

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
        axis.text.x = element_blank()) +
  ylim(0, 30000) +
  xlim(0, 600) +
  facet_wrap(~dataset)

# Produce graphs with BRET ratio data as a function of time
graph_plot_ratio <- ggplot(AllData_tidy_ratio, 
                            aes(x=as.numeric(Time), y=as.numeric(Ratio))) + 
  geom_point() +
  labs(title="", x="time [sec]", y = "BRET Ratio (Green/Blue") +
  scale_colour_manual(values = c("black")) +
  theme_light() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  ylim(0, 1) +
  xlim(0, 600) +
  facet_wrap(~dataset)

# Connect signal and ratio graphs
middlerow_plot <- plot_grid(Graph_A_1, Graph_B_1, Graph_C_1, Graph_D_1, Graph_E_1, Graph_F_1, 
                            labels = c("A", "B","C", "D","E", "F"),
                            nrow = 1,
                            rel_widths = c(1.4,1,1,1,1,1))
bottomrow_plot <- plot_grid(Graph_A_2, Graph_B_2, Graph_C_2, Graph_D_2, Graph_E_2, Graph_F_2, 
                            nrow = 1,
                            rel_widths = c(1.4,1,1,1,1,1))
graph_plot_raw <- plot_grid(middlerow_plot, bottomrow_plot, 
                            ncol = 1,
                            rel_heights = c(1,1))



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
        axis.text.x = element_blank())
graph_plot_signal2

graph_plot_ratio2 <- ggplot(
  AllData_tidy_ratio2,
  aes(x=as.numeric(Time), y=as.numeric(Ratio))) +
  geom_line(size = 2) +
  scale_colour_manual(values = c("black")) +
  labs(x = "time (sec)", y = "BRET ratio") +
  theme(legend.position = "bottom") +
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

# Save files
ggsave("Results/raw_data.png", plot = graph_plot_raw, height = 10, width = 14)
ggsave("Results/toprow_plot.png", plot = toprow_plot, height = 8, width = 14)