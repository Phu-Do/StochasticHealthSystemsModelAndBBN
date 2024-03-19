# Importing the dataset
# Remember to set the working directory to what is needed
library(readr)
scenario_eclintool_merged <- read_csv("scenario_eclintool_merged.csv")


# Renaming the columns
names(scenario_eclintool_merged)
colnames(scenario_eclintool_merged) <- c("Iteration", "Base_Scenario", "Scenario_1", "Scenario_2", "Scenario_3", "Scenario_4")

str(scenario_eclintool_merged)


# Reshaping data
library(reshape2)

melted_data <- melt(scenario_eclintool_merged, id.vars = "Iteration",
                    variable.name = "Scenario", value.name = "Proportion")

# Define the labels for the legend
legend_labels <- c("Stochastic\nmodel outputs",
                   "10-25% clinicians with\nclinical support tools",
                   "26-50% clinicians with\nclinical support tools",
                   "51-75% clinicians with\nclinical support tools",
                   "76-100% clinicians with\nclinical support tools")

# Plotting the results with custom legend labels
melted_data %>% 
  ggplot(aes(x = Proportion, fill = Scenario)) +
  geom_density(alpha = 0.25, adjust = 2) +
  theme_Publication() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Proportion of tests initiated",
       caption = "Scenarios with clinical support tool",
       y = "Density") +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("dodgerblue", "limegreen", "firebrick", "purple", "orange"),
                    labels = legend_labels) + 
  facet_wrap(~Scenario)


scenario_labels_clintool <- c(Base_Scenario = "Base Scenario",
                              Scenario_1 = "Scenario 1",
                              Scenario_2 = "Scenario 2",
                              Scenario_3 = "Scenario 3",
                              Scenario_4 = "Scenario 4",
                              Scenario_5 = "Scenario 5",
                              Scenario_6 = "Scenario 6",
                              Scenario_7 = "Scenario 7")

# Calculate median, 2.5th and 97.5th percentiles for each scenario
summary_stats <- melted_data %>%
  group_by(Scenario) %>%
  summarize(Median = median(Proportion),
            Percentile_2_5 = quantile(Proportion, 0.025),
            Percentile_97_5 = quantile(Proportion, 0.975))

# Plotting the results with custom legend labels and annotations (facet wrapped)
ggplot(melted_data, aes(x = Proportion, fill = Scenario)) +
  geom_density(alpha = 0.25, adjust = 2) +
  theme_Publication() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Proportion of tests initiated",
       caption = "Scenario with clinical support tool",
       y = "Density") +
  theme(legend.position = "top") +
  geom_vline(data = summary_stats, aes(xintercept = Percentile_2_5, color = Scenario), linetype = "dashed", show.legend = FALSE) +
  geom_vline(data = summary_stats, aes(xintercept = Percentile_97_5, color = Scenario), linetype = "dashed", show.legend = FALSE)+
  geom_text(data = summary_stats, aes(x = Median, label = paste("Median:", round(Median, 3)), color = Scenario),
            y = 20, show.legend =  FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 7) +
  geom_text(data = summary_stats, aes(x = Percentile_2_5 - 0.004, label = paste("2.5%:\n", round(Percentile_2_5, 3)), color = Scenario),
            y = 20, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 7) +
  geom_text(data = summary_stats, aes(x = Percentile_97_5 + 0.004, label = paste("97.5%:\n", round(Percentile_97_5, 3)), color = Scenario),
            y = 20, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 7) +
  scale_fill_manual(values = c("dodgerblue", "limegreen", "firebrick", "purple", "orange"),
                    labels = legend_labels) +
  scale_colour_manual(values = c("darkblue", "darkgreen", "darkred", "magenta4", "darkorange"),
                      labels = legend_labels) + 
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman", size = 20)) +
  facet_wrap(~ Scenario, scales = "free", labeller = labeller(Scenario = scenario_labels_clintool))



# Clinical knowledge scenarios
scenario_clinknow_merged <- read_csv("scenario_clinknow_merged.csv")

# Reorder the columns
scenario_clinknow_merged <- scenario_clinknow_merged %>%
  select(Iteration, `Proportion Test`,
         `Proportion Test (Sim Results Clinknow525.Csv)`,
         `Proportion Test (Sim Results Clinknow2650.Csv)`,
         `Proportion Test (Sim Results Clinknow5175.Csv)`,
         `Proportion Test (Sim Results Clinknow76100.Csv)`)


colnames(scenario_clinknow_merged)

# Changing the column names
colnames(scenario_clinknow_merged) <- c("Iteration", "Base_Scenario", "Scenario_1", "Scenario_2", "Scenario_3", "Scenario_4")

# Melting data
melted_clinknow_data <- melt(scenario_clinknow_merged, id.vars = "Iteration",
                             variable.name = "Scenario", value.name = "Proportion")

# Summary statistics
summary_stats_clinicalknow <- melted_clinknow_data %>%
  group_by(Scenario) %>%
  summarize(Median = median(Proportion),
            Percentile_2_5 = quantile(Proportion, 0.025),
            Percentile_97_5 = quantile(Proportion, 0.975))


# Define the labels for the legend
legend_labels_clinical <- c("Stochastic\nmodel outputs",
                            "10-25% Improvement through\nclinical education",
                            "26-50% Improvement through\nclinical education",
                            "51-75% Improvement through\nclinical education",
                            "76-100% Improvement through\nclinical education")

scenario_labels_clinknow <- c(Base_Scenario = "Base Scenario",
                              Scenario_1 = "Scenario 1",
                              Scenario_2 = "Scenario 2",
                              Scenario_3 = "Scenario 3",
                              Scenario_4 = "Scenario 4",
                              Scenario_5 = "Scenario 5",
                              Scenario_6 = "Scenario 6",
                              Scenario_7 = "Scenario 7")

# Plotting the results with custom legend labels
melted_clinknow_data %>% 
  ggplot(aes(x = Proportion, fill = Scenario)) +
  geom_density(alpha = 0.25, adjust = 2) +
  theme_Publication() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Proportion of tests initiated",
       caption = "Scenarios with improvement in clinical knowledge",
       y = "Density") +
  scale_fill_manual(values = c("dodgerblue", "limegreen", "firebrick", "purple", "orange"),
                    labels = legend_labels_clinical) +
  facet_wrap(~ Scenario, scales = "free", labeller = labeller(Scenario = scenario_labels_clinknow)) +
  geom_text(data = summary_stats_clinicalknow, aes(x = Median, label = paste("Median:", round(Median, 3)), color = Scenario),
            y = 15, show.legend =  FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 7) +
  geom_text(data = summary_stats_clinicalknow, aes(x = Percentile_2_5 - 0.004, label = paste("2.5%:\n", round(Percentile_2_5, 3)), color = Scenario),
            y = 10, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 7) +
  geom_text(data = summary_stats_clinicalknow, aes(x = Percentile_97_5 + 0.004, label = paste("97.5%:\n", round(Percentile_97_5, 3)), color = Scenario),
            y = 10, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 7) +
  scale_fill_manual(values = c("dodgerblue", "limegreen", "firebrick", "purple", "orange"),
                    labels = legend_labels_clinical) +
  scale_colour_manual(values = c("darkblue", "darkgreen", "darkred", "magenta4", "darkorange"),
                      labels = legend_labels_clinical) +
  geom_vline(data = summary_stats_clinicalknow, aes(xintercept = Percentile_2_5, color = Scenario), linetype = "dashed", show.legend = FALSE) +
  geom_vline(data = summary_stats_clinicalknow, aes(xintercept = Percentile_97_5, color = Scenario), linetype = "dashed", show.legend = FALSE) +
  theme(legend.position = "top",
        text = element_text(family = "HelveticaNeueLT Pro 55 Roman", size = 20)) 


# Mandates scenario data
scenario_mandates_merged <- read_csv("scenario_mandates_merged.csv")

scenario_mandates_merged$`Policy Test` <- NULL


# Reordering the columns
scenario_mandates_merged <- scenario_mandates_merged %>%
  select(Iteration, `Proportion Test`, starts_with("Policy Test"))

colnames(scenario_mandates_merged)
# Column names
colnames(scenario_mandates_merged) <- c("Iteration", "Base_Scenario", "Mandate_1", 
                                        "Mandate_2", "Mandate_3", "Mandate_4", 
                                        "Mandate_5", "Mandate_6", "Mandate_7")

# Melting data for mandates
melted_mandate_data <- melt(scenario_mandates_merged, id.vars = "Iteration",
                            variable.name = "Scenario", value.name = "Proportion")

unique(melted_mandate_data$Scenario)
# Summary statistics
summary_stats_mandates <- melted_mandate_data %>%
  group_by(Scenario) %>%
  summarize(Median = median(Proportion),
            Percentile_2_5 = quantile(Proportion, 0.025),
            Percentile_97_5 = quantile(Proportion, 0.975))

# Visualising results
# Define the labels for the legend
legend_labels_mandates <- c("Stochastic\nmodel outputs",
                            "60/30/10\nSplit",
                            "50/40/10\nSplit",
                            "40/50/10\nSplit",
                            "30/60/10\nSplit",
                            "20/70/10\nSplit",
                            "10/80/10\nSplit",
                            "5/90/5\nSplit")

scenario_labels_mandate <- c(Base_Scenario = "Base Scenario",
                             Mandate_1 = "Mandate 1",
                             Mandate_2 = "Mandate 2",
                             Mandate_3 = "Mandate 3",
                             Mandate_4 = "Mandate 4",
                             Mandate_5 = "Mandate 5",
                             Mandate_6 = "Mandate 6",
                             Mandate_7 = "Mandate 7")

# Plotting the results with custom legend labels
melted_mandate_data %>% 
  ggplot(aes(x = Proportion, fill = Scenario)) +
  geom_density(alpha = 0.25, adjust = 2) +
  theme_Publication() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Proportion of tests initiated",
       caption = "Mandate scenario with varying adherence levels",
       y = "Density") +
  theme(legend.position = "top",
        text = element_text(size = 22)) + 
  facet_wrap(~ Scenario, scales = "free", labeller = labeller(Scenario = scenario_labels_mandate)) +
  geom_text(data = summary_stats_mandates %>% filter(Scenario != "Base_Scenario" & Scenario != "Mandate_6" & Scenario != "Mandate_7"), 
            aes(x = Median, y = 4, label = paste("Median:", round(Median, 3)), color = Scenario), 
            show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 6) +
  geom_text(data = summary_stats_mandates %>% filter(Scenario == "Base_Scenario"), 
            aes(x = Median, y = 25, label = paste("Median:", round(Median, 3)), color = Scenario), 
            show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 6) +
  geom_text(data = summary_stats_mandates %>% filter(Scenario %in% c("Mandate_6", "Mandate_7")), 
            aes(x = Median, y = 4, label = paste("Median:", round(Median, 3)), color = Scenario), 
            show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 6) +
  geom_text(data = summary_stats_mandates %>% filter(Scenario != "Base_Scenario"), aes(x = Percentile_2_5 - 0.005, label = paste("2.5%:\n", round(Percentile_2_5, 3)), color = Scenario),
            y = 4, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 6) +
  geom_text(data = summary_stats_mandates %>% filter(Scenario != "Base_Scenario"), aes(x = Percentile_97_5 + 0.005, label = paste("97.5%:\n", round(Percentile_97_5, 3)), color = Scenario),
            y = 4, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 6) +
  geom_text(data = summary_stats_mandates %>% filter(Scenario == "Base_Scenario"), aes(x = Percentile_2_5 - 0.004, label = paste("2.5%:\n", round(Percentile_2_5, 3)), color = Scenario),
            y = 20, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 6) +
  geom_text(data = summary_stats_mandates %>% filter(Scenario == "Base_Scenario"), aes(x = Percentile_97_5 + 0.004, label = paste("97.5%:\n", round(Percentile_97_5, 3)), color = Scenario),
            y = 20, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 6) +
  scale_fill_manual(values = c("#1E90FF", "#32CD32", "#B22222", "#800080", "#FFA500", 
                               "#00BFFF", "#3CB371", "#FF6347", "#6A5ACD"),
                    labels = legend_labels_mandates) +
  scale_colour_manual(values = c("#104E8B", "#21610B", "#7E2217", "#4B0082", "#C35200", 
                                 "#00688B", "#1874CD", "#CD4F39", "#473C8B"),
                      labels = legend_labels_mandates)+
  geom_vline(data = summary_stats_mandates, aes(xintercept = Percentile_2_5, color = Scenario), linetype = "dashed", show.legend = FALSE) +
  geom_vline(data = summary_stats_mandates, aes(xintercept = Percentile_97_5, color = Scenario), linetype = "dashed", show.legend = FALSE)


# Dual interventions
scenario_dual <- read_csv("dualscenario.csv")

colnames(scenario_dual) <- c("Iteration", "Base_Scenario", "Scenario_1", "Scenario_2",
                             "Scenario_3", "Scenario_4")


# Melting data
melted_dual_data <- melt(scenario_dual, id.vars = "Iteration",
                         variable.name = "Scenario", value.name = "Proportion")

# Summary statistics
summary_stats_dual <- melted_dual_data %>%
  group_by(Scenario) %>%
  summarize(Median = median(Proportion),
            Percentile_2_5 = quantile(Proportion, 0.025),
            Percentile_97_5 = quantile(Proportion, 0.975))

# Define the labels for the legend
legend_labels_dual <- c("Stochastic\nmodel outputs",
                        "10-25% Improvement\nand Clinical Tool",
                        "26-50% Improvement\nand Clinical Tool",
                        "51-75% Improvement\nand Clinical Tool",
                        "76-100% Improvement\nand Clinical Tool")


# Plotting the results with custom legend labels and annotations (facet wrapped)
p1 = ggplot(melted_dual_data, aes(x = Proportion, fill = Scenario)) +
  geom_density(alpha = 0.25, adjust = 2) +
  theme_Publication() +
  scale_y_continuous(expand = c(0,0)) +
  labs(caption = "Low Adherence",
       y = "Density",
       tag = "A)",
       x = "") +
  theme(legend.position = "top") +
  geom_vline(data = summary_stats_dual, aes(xintercept = Percentile_2_5, color = Scenario), linetype = "dashed", show.legend = FALSE) +
  geom_vline(data = summary_stats_dual, aes(xintercept = Percentile_97_5, color = Scenario), linetype = "dashed", show.legend = FALSE)+
  geom_text(data = summary_stats_dual, aes(x = Median, label = paste("Median:", round(Median, 3)), color = Scenario),
            y = 17, show.legend =  FALSE, family = "HelveticaNeueLT Pro 55 Roman",  size = 8) +
  geom_text(data = summary_stats_dual %>%  filter(Scenario != "Base_Scenario"), aes(x = Percentile_2_5 - 0.010, label = paste("2.5%:\n", round(Percentile_2_5, 3)), color = Scenario),
            y = 15, show.legend  = FALSE, family = "HelveticaNeueLT Pro 55 Roman",  size = 8) +
  geom_text(data = summary_stats_dual %>%  filter(Scenario != "Base_Scenario"), aes(x = Percentile_97_5 + 0.010, label = paste("97.5%:\n", round(Percentile_97_5, 3)), color = Scenario),
            y = 15, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman",  size = 8) +
  geom_text(data = summary_stats_dual %>%  filter(Scenario == "Base_Scenario"), aes(x = Percentile_2_5 - 0.005, label = paste("2.5%:\n", round(Percentile_2_5, 3)), color = Scenario),
            y = 35, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman",  size = 8) +
  geom_text(data = summary_stats_dual %>%  filter(Scenario == "Base_Scenario"), aes(x = Percentile_97_5 + 0.005, label = paste("97.5%:\n", round(Percentile_97_5, 3)), color = Scenario),
            y = 35, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman",  size = 8) +
  scale_fill_manual(values = c("dodgerblue", "limegreen", "firebrick", "purple", "orange"),
                    labels = legend_labels_dual) +
  scale_colour_manual(values = c("darkblue", "darkgreen", "darkred", "magenta4", "darkorange"),
                      labels = legend_labels_dual) + 
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman", size = 20)) +
  facet_wrap(~ Scenario, scales = "free", labeller = labeller(Scenario = scenario_labels_clintool))
p1

summary_stats_dual

# Dual interventions
scenario_dual_high <- read_csv("scenario_dual_high.csv")

scenario_dual_high = scenario_dual_high %>% 
  select(Iteration,`Proportion Test`,
         `Proportion Test (Sim Results Dual1High.Csv)`,
         `Proportion Test (Sim Results Dual2High.Csv)`,
         `Proportion Test (Sim Results Dual3High.Csv)`,
         `Proportion Test (Sim Results Dual4High.Csv)`)

# Renaming the dataset columns
colnames(scenario_dual_high) <- c("Iteration", "Base_Scenario", "Scenario_1", "Scenario_2",
                                  "Scenario_3", "Scenario_4")


# Melting data
melted_dual_data_high <- melt(scenario_dual_high, id.vars = "Iteration",
                              variable.name = "Scenario", value.name = "Proportion")

# Summary statistics
summary_stats_dual_high <- melted_dual_data_high %>%
  group_by(Scenario) %>%
  summarize(Median = median(Proportion),
            Percentile_2_5 = quantile(Proportion, 0.025),
            Percentile_97_5 = quantile(Proportion, 0.975))

# Define the labels for the legend
legend_labels_dual <- c("Stochastic\nmodel outputs",
                        "10-25% Improvement\nand Clinical Tool",
                        "26-50% Improvement\nand Clinical Tool",
                        "51-75% Improvement\nand Clinical Tool",
                        "76-100% Improvement\nand Clinical Tool")


# Plotting the results with custom legend labels and annotations (facet wrapped)
p2 = ggplot(melted_dual_data_high, aes(x = Proportion, fill = Scenario)) +
  geom_density(alpha = 0.25, adjust = 2) +
  theme_Publication() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Proportion of tests initiated",
       caption = "High adherence scenario",
       y = "Density",
       tag = "B)") +
  theme(legend.position = "top") +
  geom_vline(data = summary_stats_dual_high, aes(xintercept = Percentile_2_5, color = Scenario), linetype = "dashed", show.legend = FALSE) +
  geom_vline(data = summary_stats_dual_high, aes(xintercept = Percentile_97_5, color = Scenario), linetype = "dashed", show.legend = FALSE)+
  geom_text(data = summary_stats_dual_high, aes(x = Median, label = paste("Median:", round(Median, 3)), color = Scenario),
            y = 17, show.legend =  FALSE, family = "HelveticaNeueLT Pro 55 Roman",  size = 8) +
  geom_text(data = summary_stats_dual_high %>%  filter(Scenario != "Base_Scenario"), aes(x = Percentile_2_5 - 0.0085, label = paste("2.5%:\n", round(Percentile_2_5, 3)), color = Scenario),
            y = 15, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman",  size = 8) +
  geom_text(data = summary_stats_dual_high %>%  filter(Scenario != "Base_Scenario"), aes(x = Percentile_97_5 + 0.0085, label = paste("97.5%:\n", round(Percentile_97_5, 3)), color = Scenario),
            y = 15, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 8) +
  geom_text(data = summary_stats_dual_high %>%  filter(Scenario == "Base_Scenario"), aes(x = Percentile_2_5 - 0.007, label = paste("2.5%:\n", round(Percentile_2_5, 3)), color = Scenario),
            y = 25, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman",  size = 8) +
  geom_text(data = summary_stats_dual_high %>%  filter(Scenario == "Base_Scenario"), aes(x = Percentile_97_5 + 0.007, label = paste("97.5%:\n", round(Percentile_97_5, 3)), color = Scenario),
            y = 25, show.legend = FALSE, family = "HelveticaNeueLT Pro 55 Roman", size = 8) +
  scale_fill_manual(values = c("dodgerblue", "limegreen", "firebrick", "purple", "orange"),
                    labels = legend_labels_dual) +
  scale_colour_manual(values = c("darkblue", "darkgreen", "darkred", "magenta4", "darkorange"),
                      labels = legend_labels_dual) + 
  theme(text = element_text(family = "HelveticaNeueLT Pro 55 Roman", size = 20),
        legend.position = "none") +
  facet_wrap(~ Scenario, scales = "free", labeller = labeller(Scenario = scenario_labels_clintool))

p2


# Joining the two plots
library(gridExtra)
library(grid)
library(cowplot)



grid.arrange(p1, p2)