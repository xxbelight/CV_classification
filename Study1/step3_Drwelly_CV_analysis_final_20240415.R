rm(list = ls())
library(tidyverse)

# Set the working directory to where the CSV files are located
setwd('/Users/mialim/Desktop/oasis/20240430_final_paper')

# Read the datasets
before <- read.csv('individual_before_fastsurfer_swift_20240430_final.csv')
after <- read.csv('individual_after_fastsurfer_swift_20240430_final.csv')

# Merge the datasets by Subject_ID and ROI
combined <- full_join(before, after, by = c("Subject_ID", "ROI"), suffix = c("_before", "_after"))

# Perform a paired t-test for each ROI and calculate the mean difference and its standard error
t_test_results <- combined %>%
  group_by(ROI) %>%
  summarise(
    Mean_CV_Before = mean(CV_before, na.rm = TRUE),
    Mean_CV_After = mean(CV_after, na.rm = TRUE),
    Mean_Difference = Mean_CV_After - Mean_CV_Before,
    SE_Difference = sd(CV_after - CV_before, na.rm = TRUE) / sqrt(n()),
    t_statistic = t.test(CV_before, CV_after, paired = TRUE)$statistic,
    p_value = t.test(CV_before, CV_after, paired = TRUE)$p.value,
    .groups = 'drop'
  ) %>%
  mutate(
    p_adjusted = p.adjust(p_value, method = "fdr"),
    Significant_FDR = p_adjusted < 0.05
  ) %>%
  arrange(desc(Mean_Difference))  # Sort by descending mean difference

# Create the plot with all ROIs, filled based on FDR adjusted significance
plot_all <- ggplot(t_test_results, aes(x = reorder(ROI, -Mean_Difference), y = Mean_Difference, fill = Significant_FDR)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean_Difference - SE_Difference, ymax = Mean_Difference + SE_Difference), width = 0.2) +
  scale_fill_manual(values = c("Sky Blue", "Dark Blue")) +
  coord_flip() +
  labs(title = "Mean CV Difference by ROI",
       x = "ROI",
       y = "Mean CV Difference (After - Before)") +
  theme_minimal()

# Display the plot
print(plot_all)

# Save the plot
ggsave("roi_cv_difference_plot_all_Fastsurfer_20240430_final.pdf", plot_all, width = 10, height = 8, units = "in")

# Print the full descriptive statistical summary for all ROIs including FDR adjustment
full_summary <- t_test_results %>%
  select(ROI, Mean_CV_Before, Mean_CV_After, Mean_Difference, SE_Difference, t_statistic, p_value, p_adjusted, Significant_FDR)

# Print the full summary
print(full_summary)






# 가장 차이 높은것 골라내기! 

# '4th_ventricle'
subjects_4th_ventricle <- combined %>%
  filter(ROI == "th-Ventricle") %>%
  mutate(Difference_CV = CV_after - CV_before) %>%
  arrange(desc(Difference_CV)) %>%
  select(Subject_ID, CV_before, CV_after, Difference_CV)


head(subjects_4th_ventricle)
print(subjects_4th_ventricle)

