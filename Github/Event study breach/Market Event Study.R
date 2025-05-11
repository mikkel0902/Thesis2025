# Load packages
library(dplyr)
library(broom)
library(tidyr)
library(readr)
library(ggplot2)

# ================================================
# 1. Load and Preprocess Data
# ================================================
event_data <- read_delim("event_study_raw.csv", delim = ";", locale = locale(decimal_mark = ",")) %>%
  mutate(
    Event_Date = as.Date(Event_Date, format = "%d-%m-%Y"),
    Calendar_Date = as.Date(Calendar_Date, format = "%d-%m-%Y"),
    RET = as.numeric(as.character(RET)),
    Market_RET = as.numeric(as.character(Market_RET))
  )

# ================================================
# 2. Estimation Window and Market Model Estimation
# ================================================
estimation_data <- event_data %>%
  filter(Relative_Day >= -155 & Relative_Day <= -30)

market_models <- estimation_data %>%
  group_by(ticker, Event_Date) %>%
  group_modify(~ tidy(lm(RET ~ Market_RET, data = .x))) %>%
  select(ticker, Event_Date, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename(alpha = `(Intercept)`, beta = Market_RET)

# ================================================
# 3. Calculate Abnormal Returns (AR)
# ================================================
calc_event_window <- function(data, start_day, end_day) {
  event_data %>%
    filter(Relative_Day >= start_day & Relative_Day <= end_day) %>%
    left_join(market_models, by = c("ticker", "Event_Date")) %>%
    mutate(AR = RET - (alpha + beta * Market_RET))
}

event_window10 <- calc_event_window(event_data, -10, 10)
event_window4  <- calc_event_window(event_data, -4, 3)
event_window1  <- calc_event_window(event_data, 1, 1)
event_window3 <- calc_event_window(event_data, 1, 3)

# ================================================
# 4. Calculate CARs
# ================================================
CAR_multiple_windows <- event_window10 %>%
  group_by(ticker, Event_Date) %>%
  summarise(
    CAR_10_10 = sum(AR[Relative_Day >= -10 & Relative_Day <= 10], na.rm = TRUE),
    CAR_4_3 = sum(AR[Relative_Day >= -4 & Relative_Day <= 3], na.rm = TRUE),
    CAR_1 = sum(AR[Relative_Day >= 1 & Relative_Day <= 1], na.rm = TRUE),
    CAR_1_3 = sum(AR[Relative_Day >= 1 & Relative_Day <= 3], na.rm = TRUE),
  )


# Cumulative sum for plotting
calc_CAR_series <- function(data) {
  data %>%
    group_by(ticker, Event_Date) %>%
    arrange(Relative_Day) %>%
    mutate(CAR = cumsum(AR))
}

CARs10 <- calc_CAR_series(event_window10)
CARs4  <- calc_CAR_series(event_window4)
CARs1  <- calc_CAR_series(event_window1)
CARs3  <- calc_CAR_series(event_window3)

# ================================================
# 5. Average CARs and CAAR
# ================================================
average_CARs <- CAR_multiple_windows %>%
  summarise(
    Avg_CAR_10_10 = mean(CAR_10_10, na.rm = TRUE),
    Avg_CAR_4_3 = mean(CAR_4_3, na.rm = TRUE),
    Avg_CAR_1 = mean(CAR_1, na.rm = TRUE),
    Avg_CAR_1_3 = mean(CAR_1_3, na.rm = TRUE)
  )

print(average_CARs)

CAAR_10 <- event_window10 %>%
  group_by(Relative_Day) %>%
  summarise(Avg_AR = mean(AR, na.rm = TRUE)) %>%
  arrange(Relative_Day) %>%
  mutate(CAAR = cumsum(Avg_AR))

# ================================================
# 6. Distribution Plots
# ================================================
ggplot(event_window10, aes(x = AR)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "grey100", color = "black") +
  geom_density(color = "black", size = 1) +
  labs(title = "Distribution of Abnormal Returns (ARs)", x = "AR", y = "Density") +
  theme_apa()

ggplot(CAR_multiple_windows, aes(x = CAR_10_10)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "grey100", color = "black") +
  geom_density(color = "black", size = 1) +
  labs(title = "Distribution of Cumulative Abnormal Returns (CAR_10_10)", x = "CAR", y = "Density") +
  theme_apa()

# ================================================
# 7. Hypothesis Tests
# ================================================
# T-tests on CARs
t_test_10_10 <- t.test(CAR_multiple_windows$CAR_10_10)
t_test_4_3 <- t.test(CAR_multiple_windows$CAR_4_3)
t_test_1 <- t.test(CAR_multiple_windows$CAR_1)
t_test_1_3 <- t.test(CAR_multiple_windows$CAR_1_3)

# T-tests and Wilcoxon test daily
daily_tests <- event_window10 %>%
  group_by(Relative_Day) %>%
  summarise(
    t_stat = t.test(AR, mu = 0)$statistic,
    p_val_t = t.test(AR, mu = 0)$p.value,
    p_val_wilcox = wilcox.test(AR, mu = 0)$p.value
  )

car_tests <- tibble(
  Window = c("-10 to +10", "-4 to +3", "+1", "+1 to +3"),
  Mean_CAR = c(
    mean(CAR_multiple_windows$CAR_10_10, na.rm = TRUE),
    mean(CAR_multiple_windows$CAR_4_3, na.rm = TRUE),
    mean(CAR_multiple_windows$CAR_1, na.rm = TRUE),
    mean(CAR_multiple_windows$CAR_1_3, na.rm = TRUE)
  ),
  t_stat = c(
    t_test_10_10$statistic,
    t_test_4_3$statistic,
    t_test_1$statistic,
    t_test_1_3$statistic
  ),
  p_value = c(
    t_test_10_10$p.value,
    t_test_4_3$p.value,
    t_test_1$p.value,
    t_test_1_3$p.value
  )
) %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    p_value < 0.1 ~ ".",
    TRUE ~ ""
  ))


print(car_tests)

# T-test on CAAR
t_test_caar <- t.test(CAAR_10$Avg_AR)
print(t_test_caar)

# Z-test on CAAR
z_stat_caar <- mean(CAAR_10$Avg_AR, na.rm = TRUE) / (sd(CAAR_10$Avg_AR, na.rm = TRUE) / sqrt(nrow(CAAR_10)))
p_value_z <- 2 * (1 - pnorm(abs(z_stat_caar)))

cat("Z-statistic:", z_stat_caar, "\n")
cat("p-value:", p_value_z, "\n")

# Wilcoxon tests
wilcox_test_car_4 <- wilcox.test(CAR_multiple_windows$CAR_4_3, mu = 0)
print(wilcox_test_car_4)

wilcox_test_caar <- wilcox.test(CAAR_10$Avg_AR, mu = 0)
print(wilcox_test_caar)

# ================================================
# 8. CAAR and CAR Plots
# ================================================
CAR_mean_plot <- CARs10 %>%
  group_by(Relative_Day) %>%
  summarise(
    Mean_CAR = mean(CAR, na.rm = TRUE),
    SD = sd(CAR, na.rm = TRUE),
    N = n(),
    SE = SD / sqrt(N),
    Upper = Mean_CAR + 1.96 * SE,
    Lower = Mean_CAR - 1.96 * SE
  )

ggplot(CAR_mean_plot, aes(x = Relative_Day, y = Mean_CAR)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Average CAR at Reporting (-10 to +10) Across Events",
    x = "Relative Day",
    y = "Average Cumulative Abnormal Return"
  ) +
  theme(text = element_text(family = "Times New Roman"))

# CAAR plot
ggplot(CAAR_10, aes(x = Relative_Day, y = CAAR)) +
  geom_line(size = 1) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Cumulative Average Abnormal Return (CAAR)",
    subtitle = "-10 to +10 Days Around Event",
    x = "Days Relative to Event",
    y = "CAAR"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# ================================================
# 9. Export Data
# ================================================
write_csv(CAAR_10, "CAAR_10.csv")
write_csv(CAR_multiple_windows, "EventStudyResults2.csv")
write_csv(CARs10, "CARS10.csv")
write_csv(CARs5, "CARS5.csv")
write_csv(CARs1, "CARS1.csv")
write_csv(CARs1_1, "CARS1_1.csv")
write_csv(CARs1_3, "CARS1_3.csv")
