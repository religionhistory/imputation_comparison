# Visualize results

# Load functions
source("../project_support.R")

# Load overall data
overall <- list.files(path = "./../output/study1/avg_overall_metrics/", pattern = "*.csv", full.names = T, recursive = T)
overall <- lapply(overall, read_csv)
overall <- bind_rows(overall)

# Split condition
overall <- overall %>%
  separate(Condition, c("Condition", "Missing (%)")) %>%
  mutate(Method = ifelse(Method == "random_forest", "missForest",
                  ifelse(Method == "MICE", "MICE - Default", 
                  ifelse(Method == "pmm", "MICE - PMM", 
                  ifelse(Method == "sample_mi", "MICE - Random Sample", 
                  ifelse(Method == "cart", "MICE - CART",
                  ifelse(Method == "rf_mi", "MICE - Random Forest", Method)))))))

# Split into conditions
mcar <- overall %>%
  filter(grepl("MCAR", Condition)) %>%
  mutate(label = if_else(`Missing (%)` == max(`Missing (%)`), as.character(Method), NA_character_))
mar <- overall %>%
  filter(grepl("MAR", Condition)) %>%
  mutate(label = if_else(`Missing (%)` == max(`Missing (%)`), as.character(Method), NA_character_))
mnar <- overall %>%
  filter(grepl("MNAR", Condition)) %>%
  mutate(label = if_else(`Missing (%)` == max(`Missing (%)`), as.character(Method), NA_character_))

# Plot overall NRMSE Figures
overall_plots(mcar, "mcar", study = 1)
overall_plots(mar, "mar", study = 1)
overall_plots(mnar, "mnar", study = 1)

# Create overall figure for paper
nrmse_mcar <- ggplot(mcar, aes(x = `Missing (%)`, y = `Mean NRMSE`, shape = Method, group = Method)) +
  geom_line() +
  geom_errorbar(aes(ymin=`Mean NRMSE`-`NRMSE SE`, ymax=`Mean NRMSE`+`NRMSE SE`), width=0.07) +
  geom_point(aes(color = Method), size = 2.5) +
  geom_text_repel(data = subset(mcar, `Mean NRMSE` > 0.7), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = -1.5, direction = "y", hjust = 1) +
  geom_text_repel(data = subset(mcar, `Mean NRMSE` <= 0.7), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = 3.5, direction = "y", hjust = 1) +
  xlab("MCAR Missing (%)") +
  theme_classic() +
  theme(
    axis.text = element_text(colour = "black", size=11),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  scale_shape_manual(values=c(17, 15, 16, 6, 8, 7, 3, 10, 13, 14)) +
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")) 
nrmse_mar <- ggplot(mar, aes(x = `Missing (%)`, y = `Mean NRMSE`, shape = Method, group = Method)) +
  geom_line() +
  geom_errorbar(aes(ymin=`Mean NRMSE`-`NRMSE SE`, ymax=`Mean NRMSE`+`NRMSE SE`), width=0.07) +
  geom_point(aes(color = Method), size = 2.5) +
  geom_text_repel(data = subset(mar, `Mean NRMSE` > 0.6), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = -1.5, direction = "y", hjust = 1) +
  geom_text_repel(data = subset(mar, `Mean NRMSE` <= 0.6), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = 2.4, direction = "y", hjust = 1) +
  xlab("MAR Missing (%)") +
  theme_classic() +
  theme(
    axis.text = element_text(colour = "black", size=11),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  scale_shape_manual(values=c(17, 15, 16, 6, 8, 7, 3, 10, 13, 14)) +
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"))
nrmse_mnar <- ggplot(mnar, aes(x = `Missing (%)`, y = `Mean NRMSE`, shape = Method, group = Method)) +
  geom_line() +
  geom_errorbar(aes(ymin=`Mean NRMSE`-`NRMSE SE`, ymax=`Mean NRMSE`+`NRMSE SE`), width=0.07) +
  geom_point(aes(color = Method), size = 2.5) +
  geom_text_repel(data = subset(mnar, `Mean NRMSE` > 0.5), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = -1.5, direction = "y", hjust = 1) +
  geom_text_repel(data = subset(mnar, `Mean NRMSE` <= 0.5), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = 2.3, direction = "y", hjust = 1) +
  xlab("MNAR Missing (%)") +
  theme_classic() +
  theme(
    axis.text = element_text(colour = "black", size=11),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  scale_shape_manual(values=c(17, 15, 16, 6, 8, 7, 3, 10, 13, 14)) +
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"))
mcar_percent_bias <- ggplot(mcar, aes(x = `Missing (%)`, y = `Mean Percent Bias`, shape = Method, group = Method)) +
  geom_line() +
  geom_errorbar(aes(ymin=`Mean Percent Bias`-`Percent Bias SE`, ymax=`Mean Percent Bias`+`Percent Bias SE`), width=0.07) +
  geom_point(aes(color = Method), size = 2.5) +
  geom_text_repel(data = subset(mcar, `Mean Percent Bias` > 0.4), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = -1, direction = "y", hjust = 1) +
  geom_text_repel(data = subset(mcar, `Mean Percent Bias` <= 0.4), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = 2, direction = "y", hjust = 1) +
  xlab("MCAR Missing (%)") +
  theme_classic() +
  theme(
    axis.text = element_text(colour = "black", size=11),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  scale_shape_manual(values=c(17, 15, 16, 6, 8, 7, 3, 10, 13, 14)) +
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"))
mar_percent_bias <- ggplot(mar, aes(x = `Missing (%)`, y = `Mean Percent Bias`, shape = Method, group = Method)) +
  geom_line() +
  geom_errorbar(aes(ymin=`Mean Percent Bias`-`Percent Bias SE`, ymax=`Mean Percent Bias`+`Percent Bias SE`), width=0.07) +
  geom_point(aes(color = Method), size = 2.5) +
  geom_text_repel(data = subset(mar, `Mean Percent Bias` > 0.16), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = -1.5, direction = "y", hjust = 1) +
  geom_text_repel(data = subset(mar, `Mean Percent Bias` <= 0.16), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = 1.5, direction = "y", hjust = 1) +
  xlab("MAR Missing (%)") +
  theme_classic() +
  theme(
    axis.text = element_text(colour = "black", size=11),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  scale_shape_manual(values=c(17, 15, 16, 6, 8, 7, 3, 10, 13, 14)) +
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"))
mnar_percent_bias <- ggplot(mnar, aes(x = `Missing (%)`, y = `Mean Percent Bias`, shape = Method, group = Method)) +
  geom_line() +
  geom_errorbar(aes(ymin=`Mean Percent Bias`-`Percent Bias SE`, ymax=`Mean Percent Bias`+`Percent Bias SE`), width=0.07) +
  geom_point(aes(color = Method), size = 2.5) +
  geom_text_repel(data = subset(mnar, `Mean Percent Bias` > 0.12), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = -1.5, direction = "y", hjust = 1) +
  geom_text_repel(data = subset(mnar, `Mean Percent Bias` <= 0.12), aes(label = label, segment.square = FALSE, segment.inflect = FALSE), nudge_x = 1.75, direction = "y", hjust = 1) +
  xlab("MNAR Missing (%)") +
  theme_classic() +
  theme(
    axis.text = element_text(colour = "black", size=11),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) +
  scale_shape_manual(values=c(17, 15, 16, 6, 8, 7, 3, 10, 13, 14)) +
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"))
#ggarrange(nrmse_mcar, nrmse_mar, nrmse_mnar, mcar_percent_bias, mar_percent_bias, mnar_percent_bias, labels = c("A", "B", "C", "D", "E", "F"), ncol = 3, nrow = 2,
#          common.legend = TRUE, legend = "top")
ggarrange(nrmse_mcar, mcar_percent_bias, nrmse_mar, mar_percent_bias, nrmse_mnar, mnar_percent_bias, labels = c("A", "B", "C", "D", "E", "F"), ncol = 2, nrow = 3,
          common.legend = TRUE, legend = "bottom")
# Save output
ggsave("./../figures/study1/overall/study_1_fig.pdf", width = 10, height = 10)

# Find the overall rank of imputation methods
mcar_rank <- rank_imp(data = mcar, study = 1, condition = "MCAR")
mar_rank <- rank_imp(data = mar, study = 1, condition = "MAR")
mnar_rank <- rank_imp(data = mnar, study = 1, condition = "MNAR")

# Load variable data
variable <- list.files(path = "./../output/study1/avg_variable_metrics/", pattern = "*.csv", full.names = T, recursive = T)
variable <- lapply(variable, read_csv)
variable <- bind_rows(variable)
questions <- read_csv("../data/drh_v6_poll.csv") 

# Join with metadata
variable <- variable %>%
  separate(Condition, c("Condition", "Missing (%)")) %>%
  mutate(Method = ifelse(Method == "random_forest", "missForest",
                  ifelse(Method == "MICE", "MICE - Default", 
                  ifelse(Method == "pmm", "MICE - PMM", 
                  ifelse(Method == "sample_mi", "MICE - Random Sample", 
                  ifelse(Method == "cart", "MICE - CART",
                  ifelse(Method == "rf_mi", "MICE - Random Forest", Method))))))) %>%
  mutate(`Question ID` = gsub("(.+?)(\\_.*)", "\\1", var)) %>%
  mutate(`Question ID` = as.numeric(`Question ID`)) %>%
  left_join(questions, by = "Question ID")

# Split into conditions
mcar_var <- variable %>%
  filter(grepl("MCAR", Condition))
mar_var <- variable %>%
  filter(grepl("MAR", Condition))
mnar_var <- variable %>%
  filter(grepl("MNAR", Condition))

# Plot variable NRMSE Figures
variable_plots(mcar_var, "mcar", study = 1)
variable_plots(mar_var, "mar", study = 1)
variable_plots(mnar_var, "mnar", study = 1)

# Find the average mean NRMSE and percent bias
mar_av <- mar_var %>% 
  group_by(Question, `Question ID`, `Missing (%)`) %>% 
  summarise(`Mean NRMSE` = mean(`Mean NRMSE`), `Mean Percent Bias` = mean(`Mean Percent Bias`), .groups = "keep") 
mcar_av <- mcar_var %>% 
  group_by(Question, `Question ID`, `Missing (%)`) %>% 
  summarise(`Mean NRMSE` = mean(`Mean NRMSE`), `Mean Percent Bias` = mean(`Mean Percent Bias`), .groups = "keep") 
mnar_av <- mnar_var %>% 
  group_by(Question, `Question ID`, `Missing (%)`) %>% 
  summarise(`Mean NRMSE` = mean(`Mean NRMSE`), `Mean Percent Bias` = mean(`Mean Percent Bias`), .groups = "keep") 

