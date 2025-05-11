library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(readr)
library(DescTools)
library(tibble)
library(car)
library(lmtest)
library(sandwich)

# --- Load Data ---
CAR_full <- read_delim("car_fullfinal.csv", delim = ";", locale = locale(decimal_mark = ","))
CAR_full$weekday <- as.factor(CAR_full$weekday)
str(CAR_full)


# --- Initial Model ---
model1_cyber_change <- lm(cyber_change ~ CAR_1 + sentiment_change + year_group + weekday +
                            breach_type2 + d_since_breach + total_affected + employee +
                            market_cap_m + roa + roe + debt_to_equity + cash_ratio +
                            overall_sector + year_since_ipo, data = CAR_full)
summary(model1_cyber_change)
coeftest(model1_cyber_change, vcov = vcovHC(model1_cyber_change, type = "HC1"))


# --- Diagnostics ---
vif(model1_cyber_change)
bptest(model1_cyber_change)
shapiro.test(residuals(model1_cyber_change))
plot(model1_cyber_change, which = 1:4)
influencePlot(model1_cyber_change)

#interaction model plus robust
model1_cyber_change_interact <- lm(cyber_change ~ total_affected * CAR_1 + CAR_1 + sentiment_change + year_group + weekday + breach_type2 + d_since_breach + total_affected + employee + market_cap_m + roa + roe + debt_to_equity + cash_ratio + overall_sector + year_since_ipo, data = CAR_full)
summary(model1_cyber_change_interact)
robust_result1 <- coeftest(model1_cyber_change_interact, vcov = vcovHC(model1_cyber_change_interact, type = "HC1"))
robust_result1 = tidy(robust_result1)
write.csv2(robust_result1, "robust_result1.csv", row.names = FALSE)
# --- Diagnostics ---

vif(model1_cyber_change_interact)
plot(model1_cyber_change_interact, which = 1)   # Residuals vs Fitted
plot(model1_cyber_change_interact, which = 2)   # Q-Q Plot
plot(model1_cyber_change_interact, which = 3)   # Scale-Location
plot(model1_cyber_change_interact, which = 4)   # Cook's Distance
bptest(model1_cyber_change_interact)           
shapiro.test(residuals(model1_cyber_change_interact))
influencePlot(model1_cyber_change_interact)




# --- Initial Model ---
model1_cyber_score <- lm(cyber_score ~ CAR_1 + sentiment_change + year_group + weekday +
                           breach_type2 + d_since_breach + total_affected + employee +
                           market_cap_m + roa + roe + debt_to_equity + cash_ratio +
                           overall_sector + year_since_ipo, data = CAR_full)
summary(model1_cyber_score)

# --- Diagnostics ---
vif(model1_cyber_score)
bptest(model1_cyber_score)
shapiro.test(residuals(model1_cyber_score))
plot(model1_cyber_score, which = 1:4)
influencePlot(model1_cyber_score)
coeftest(model1_cyber_score, vcov = vcovHC(model1_cyber_score, type = "HC1"))




model_cyber_score_int <- lm(cyber_score ~ total_affected * CAR_1 + CAR_1 + sentiment_change + year_group + weekday +
                                breach_type2 + d_since_breach + total_affected + employee +
                                market_cap_m + roa + roe + debt_to_equity + cash_ratio +
                                overall_sector + year_since_ipo, data = CAR_full)
summary(model_cyber_score_int)
robust_result <- coeftest(model_cyber_score_int, vcov = vcovHC(model_cyber_score_int, type = "HC1"))
robust_result = tidy(robust_result)
write.csv2(robust_result, "robust_result.csv", row.names = FALSE)

vif(model_cyber_score_int)
bptest(model_cyber_score_int)
shapiro.test(residuals(model_cyber_score_int))
plot(model_cyber_score_int, which = 1:4)
influencePlot(model_cyber_score_int)
