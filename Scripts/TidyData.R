library(tidyverse)
library(cowplot)

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
  theme_light() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  ylim(0, 30000) +
  xlim(0, 600)

Graph_B_1 <- ggplot(subset(DataFrameB_tidier, colour=="blue" | colour=="green"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0, 30000) +
  xlim(0, 600)

Graph_C_1 <- ggplot(subset(DataFrameC_tidier, colour=="blue" | colour=="green"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0, 30000) +
  xlim(0, 600)

Graph_D_1 <- ggplot(subset(DataFrameD_tidier, colour=="blue" | colour=="green"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0, 30000) +
  xlim(0, 600)

Graph_E_1 <- ggplot(subset(DataFrameE_tidier, colour=="blue" | colour=="green"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0, 30000) +
  xlim(0, 600)

Graph_F_1 <- ggplot(subset(DataFrameF_tidier, colour=="blue" | colour=="green"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "Bioluminescence signal [A.U.]") +
  scale_colour_manual(values = c("blue", "#31a354")) + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0, 30000) +
  xlim(0, 600)

# Produce graphs with BRET ratio data as a function of time

Graph_A_2 <- ggplot(subset(DataFrameA_tidier, colour=="BRET_ratio"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "BRET ratio") +
  scale_colour_manual(values = "black") + 
  theme_light() +
  theme(legend.position = "none") +
  ylim(0, 1) +
  xlim(0, 600)

Graph_B_2 <- ggplot(subset(DataFrameB_tidier, colour=="BRET_ratio"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "BRET ratio") +
  scale_colour_manual(values = "black") + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0, 1) +
  xlim(0, 600)

Graph_C_2 <- ggplot(subset(DataFrameC_tidier, colour=="BRET_ratio"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "BRET ratio") +
  scale_colour_manual(values = "black") + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0, 1) +
  xlim(0, 600)

Graph_D_2 <- ggplot(subset(DataFrameD_tidier, colour=="BRET_ratio"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "BRET ratio") +
  scale_colour_manual(values = "black") + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0, 1) +
  xlim(0, 600)

Graph_E_2 <- ggplot(subset(DataFrameE_tidier, colour=="BRET_ratio"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "BRET ratio") +
  scale_colour_manual(values = "black") + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0, 1) +
  xlim(0, 600)

Graph_F_2 <- ggplot(subset(DataFrameF_tidier, colour=="BRET_ratio"), 
                    aes(x=as.numeric(time), y=as.numeric(signal))) + 
  geom_point(aes(colour = colour)) +
  labs(title="", x="time [sec]", y = "BRET ratio") +
  scale_colour_manual(values = "black") + 
  theme_light() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(0, 1) +
  xlim(0, 600)

toprow_plot <- plot_grid(NULL, graph_analysis, NULL, 
                         rel_widths = c(0.6,1,0.6),
                         nrow = 1)
middlerow_plot <- plot_grid(Graph_A_1, Graph_B_1, Graph_C_1, Graph_D_1, Graph_E_1, Graph_F_1, 
                            labels = c("A", "B","C", "D","E", "F"),
                            nrow = 1,
                            rel_widths = c(1.4,1,1,1,1,1))
bottomrow_plot <- plot_grid(Graph_A_2, Graph_B_2, Graph_C_2, Graph_D_2, Graph_E_2, Graph_F_2, 
                            nrow = 1,
                            rel_widths = c(1.4,1,1,1,1,1))
graph_plot_raw <- plot_grid (middlerow_plot, bottomrow_plot, 
                         ncol = 1,
                         rel_heights = c(1,1))
ggsave("Results/raw_data.png", plot = graph_plot_raw, height = 10, width = 14)
ggsave("Results/toprow_plot.png", plot = toprow_plot, height = 8, width = 14)