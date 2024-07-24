rm(list = ls())
# Load the required libraries
library(tidyverse)

# Set working directory
setwd('/Users/mialim/Desktop/oasis/CV_analysis')

# dataset  Setting 하기 
before <- read.csv('individual_DRWELLY_before.csv')
after <- read.csv('individual_DRWELLY_after.csv')

# Merge the datasets by Subject_ID and ROI
combined <- full_join(before, after, by = c("Subject_ID", "ROI"), suffix = c("_before", "_after"))

# Perform a paired t-test for each ROI and calculate differences
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
  mutate(Significant = p_value < 0.05) %>%
  arrange(-abs(Mean_Difference))  # Sort by absolute descending mean difference

# Plot the mean differences with error bars for all ROIs
plot_all <- ggplot(t_test_results, aes(x = reorder(ROI, -abs(Mean_Difference)), y = Mean_Difference, fill = Significant)) +
  geom_col(show.legend = FALSE) +
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
#ggsave("roi_cv_difference_plot_all.pdf", plot_all, width = 10, height = 8, units = "in")

# Output descriptive statistical summary for significant results
significant_summary <- t_test_results %>%
  filter(Significant) %>%
  select(ROI, Mean_CV_Before, Mean_CV_After, Mean_Difference, SE_Difference, t_statistic, p_value)

# Print the significant statistical summary
print(significant_summary)

