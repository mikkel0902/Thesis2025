library(readr)
library(dplyr)
library(ggplot2)

theme_apa <- function(base_size = 13, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = base_size),
      axis.title.y = element_text(size = base_size),
      axis.text = element_text(size = base_size - 2),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.major = element_line(color = "grey85"),
      panel.grid.minor = element_blank()
    )
}


# --- Step 1: Load and Prepare Data ---
# Read combined event metadata
combined_data <- read_delim("combineddata_event.csv", delim = ";", locale = locale(decimal_mark = ",")) %>%
  mutate(report_trading_day = as.Date(report_trading_day, format = "%d/%m/%Y"))

# Merge metadata into CAR_multiple_windows
CAR_full <- CAR_multiple_windows %>%
  left_join(combined_data, by = c("ticker" = "ticker", "Event_Date" = "report_trading_day"))

# Create group variables
median_breach_size <- median(CAR_full$total_affected, na.rm = TRUE)
median_market_cap <- median(CAR_full$market_cap_m, na.rm = TRUE)

CAR_full <- CAR_full %>%
  mutate(
    breach_size_group = ifelse(total_affected <= median_breach_size, "Small", "Large"),
    market_cap_group = ifelse(market_cap_m <= median_market_cap, "Small Cap", "Large Cap")
  )

CAR_full$weekday <- as.factor(CAR_full$weekday)

CAR_full <- CAR_full %>%
  mutate(year_group = case_when(
    year <= 2019 ~ "early",
    year <= 2022 ~ "modern",
    year >= 2023 ~ "recent"
  )) %>%
  mutate(year_group = factor(year_group, levels = c("early", "modern", "recent")))

# Reorder breach type and sector for prettier plots
CAR_full$breach_type <- reorder(CAR_full$breach_type, CAR_full$CAR_1, FUN = median)
CAR_full$breach_type2 <- reorder(CAR_full$breach_type2, CAR_full$CAR_1, FUN = median)
CAR_full$overall_sector <- reorder(CAR_full$overall_sector, CAR_full$CAR_1, FUN = median)

# --- Step 2: Visualizations ---

# Boxplots
ggplot(CAR_full, aes(x = breach_type2, y = CAR_1)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(title = "CAR (+1)) Across Breach Type 2", x = "Breach Type 2", y = "CAR (+1)") +
  theme_apa()

ggplot(CAR_full, aes(x = overall_sector, y = CAR_1)) +
  geom_boxplot(fill = "coral", color = "black") +
  labs(title = "CAR (+1) Across Overall Sectors", x = "Overall Sector", y = "CAR (+1)") +
  theme_apa()

ggplot(CAR_full, aes(x = breach_size_group, y = CAR_1)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "CAR (+1) Across Overall Sectors", x = "Overall Sector", y = "CAR (+1)") +
  theme_apa()

# Kruskal-Wallis Tests
kruskal.test(CAR_1 ~ breach_type2, data = CAR_full)
kruskal.test(CAR_1 ~ overall_sector, data = CAR_full)

# Boxplots and Wilcoxon for breach size and market cap
ggplot(CAR_full, aes(x = breach_size_group, y = CAR_1)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "CAR (+1) by Breach Size", x = "Breach Size", y = "CAR (+1)") +
  theme_apa()

wilcox.test(CAR_1 ~ breach_size_group, data = CAR_full)

ggplot(CAR_full, aes(x = market_cap_group, y = CAR_1)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "CAR (+1) by Market Cap", x = "Market Cap Group", y = "CAR (+1)") +
  theme_minimal(base_size = 14) + theme(plot.title = element_text(face = "bold", hjust = 0.5))

wilcox.test(CAR_1 ~ market_cap_group, data = CAR_full)

# --- Step 3: AAR & CAAR Computation with Confidence Bands ---

AAR_results <- event_window10 %>%
  group_by(Relative_Day) %>%
  summarise(
    Avg_AR = mean(AR, na.rm = TRUE),
    SD_AR = sd(AR, na.rm = TRUE),
    N = n(),
    SE_AR = SD_AR / sqrt(N)
  ) %>%
  arrange(Relative_Day) %>%
  mutate(
    CAAR = cumsum(Avg_AR),
    Upper = Avg_AR + 1.96 * SE_AR,
    Lower = Avg_AR - 1.96 * SE_AR
  )

# Plot AAR & CAAR
ggplot(AAR_results, aes(x = Relative_Day)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "black", alpha = 0.2) +
    geom_line(aes(y = Avg_AR, color = "AAR"), size = 1.2) +
  geom_line(aes(y = CAAR, color = "CAAR"), size = 1.2) +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_color_manual(values = c("AAR" = "gray40", "CAAR" = "steelblue")) +
  labs(title = expression(bar(AR)~"&"~CAAR~"with 95% Confidence Bands"),
       x = "Days Relative to Event", y = "Return (%)") +
  theme_minimal(base_size = 14) +
  theme_apa()

# --- Step 4: CAAR Split by Breach Size and Market Cap ---

CARs10_with_metadata <- CARs10 %>%
  left_join(combined_data, by = c("ticker" = "ticker", "Event_Date" = "report_trading_day")) %>%
  mutate(
    breach_size_group = ifelse(total_affected <= median_breach_size, "Small", "Large"),
    market_cap_group = ifelse(market_cap_m <= median_market_cap, "Small Cap", "Large Cap")
  )

# CAAR by breach size
CAAR_by_breach_size <- CARs10_with_metadata %>%
  group_by(breach_size_group, Relative_Day) %>%
  summarise(Avg_AR = mean(AR, na.rm = TRUE)) %>%
  group_by(breach_size_group) %>%
  mutate(CAAR = cumsum(Avg_AR))

ggplot(CAAR_by_breach_size, aes(x = Relative_Day, y = CAAR, color = breach_size_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Large" = "skyblue", "Small" = "steelblue")) +
  labs(title = "CAAR by Breach Size Group", x = "Days Relative to Event", y = "Cumulative AAR") +
  theme_minimal(base_size = 14) +
  theme_apa()

# CAAR by market cap
CAAR_by_market_cap <- CARs10_with_metadata %>%
  group_by(market_cap_group, Relative_Day) %>%
  summarise(Avg_AR = mean(AR, na.rm = TRUE)) %>%
  group_by(market_cap_group) %>%
  mutate(CAAR = cumsum(Avg_AR))

ggplot(CAAR_by_market_cap, aes(x = Relative_Day, y = CAAR, color = market_cap_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Large Cap" = "skyblue", "Small Cap" = "steelblue")) +
  labs(title = "CAAR by Market Cap Group", x = "Days Relative to Event", y = "Cumulative AAR") +
  theme_minimal(base_size = 14) +
  theme_apa()





# Create a list of variables you want to summarize
variables_to_summarize <- list(
  "AAR" = event_window10$AR,
  "CAR_10_10" = CAR_multiple_windows$CAR_10_10,
  "CAR_4_3" = CAR_multiple_windows$CAR_4_3,
  "CAR_1" = CAR_multiple_windows$CAR_1,
  "CAR_1_3" = CAR_multiple_windows$CAR_1_3
)



# Function to compute statistics for each measure
compute_statistics <- function(x) {
  t_test <- t.test(x)
  wilcox_test <- wilcox.test(x, mu = 0)
  
  tibble(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Std_Dev = sd(x, na.rm = TRUE),
    t_statistic = t_test$statistic,
    t_p_value = t_test$p.value,
    wilcox_p_value = wilcox_test$p.value,
    z_statistic = mean(x, na.rm = TRUE) / (sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))),
    z_p_value = 2 * (1 - pnorm(abs(mean(x, na.rm = TRUE) / (sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))))),
    N = sum(!is.na(x))
  )
}

# Apply the function to each variable
descriptive_table <- bind_rows(lapply(variables_to_summarize, compute_statistics), .id = "Measure")

# Add significance levels to CAR and AAR summary
descriptive_table <- descriptive_table %>%
  mutate(
    Significance = case_when(
      t_p_value < 0.001 ~ "***",
      t_p_value < 0.01 ~ "**",
      t_p_value < 0.05 ~ "*",
      t_p_value < 0.1 ~ ".",
      TRUE ~ ""
    )
  )
# View it
print(descriptive_table)


# Daily Event Study Table based on your AAR and CAAR calculations

daily_tests <- event_window10 %>%
  group_by(Relative_Day) %>%
  summarise(
    Avg_AR = mean(AR, na.rm = TRUE),
    SD_AR = sd(AR, na.rm = TRUE),
    N = n(),
    SE_AR = SD_AR / sqrt(N),
    t_stat = t.test(AR, mu = 0)$statistic,
    t_p_value = t.test(AR, mu = 0)$p.value,
    wilcox_p_value = wilcox.test(AR, mu = 0)$p.value
  ) %>%
  arrange(Relative_Day) %>%
  mutate(
    CAAR = cumsum(Avg_AR)
  )

# Add significance levels
daily_tests <- daily_tests %>%
  mutate(
    Significance = case_when(
      t_p_value < 0.001 ~ "***",
      t_p_value < 0.01 ~ "**",
      t_p_value < 0.05 ~ "*",
      t_p_value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    # Format the numbers: 2 decimals, no scientific notation
    Avg_AR = format(round(Avg_AR * 100, 6), decimal.mark = ","),
    CAAR = format(round(CAAR * 100, 6), decimal.mark = ","),
    t_stat = format(round(t_stat, 6), decimal.mark = ","),
    t_p_value = format(round(t_p_value, 6), decimal.mark = ","),
    wilcox_p_value = format(round(wilcox_p_value, 6), decimal.mark = ","),
    SE_AR = format(round(SE_AR * 100, 6), decimal.mark = ","),
    SD_AR = format(round(SD_AR * 100, 6), decimal.mark = ",")
  )


# View it
print(daily_tests)

write_csv(CAR_full, "eventstudybreach.csv")

