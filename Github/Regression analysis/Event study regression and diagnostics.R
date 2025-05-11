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
CAR_full$weekday <- factor(CAR_full$weekday)
str(CAR_full$weekday)
str(CAR_full)


# --- Initial Model ---
eventstudy <- lm(CAR_1 ~ year_group + weekday +
                            breach_type2 + d_since_breach + total_affected + employee +
                            market_cap_m + roa + roe + debt_to_equity + cash_ratio +
                            overall_sector + year_since_ipo, data = CAR_full)
summary(eventstudy)

# --- Diagnostics ---
vif(eventstudy)
plot(eventstudy, which = 1)   # Residuals vs Fitted
plot(eventstudy, which = 2)   # Q-Q Plot
plot(eventstudy, which = 3)   # Scale-Location
plot(eventstudy, which = 4)   # Cook's Distance
bptest(eventstudy)           
shapiro.test(residuals(eventstudy))
influencePlot(eventstudy)
coeftest(eventstudy, vcov = vcovHC(eventstudy, type = "HC1"))


#with interaction term
eventstudy2 <- lm(CAR_1 ~ total_affected * roe + year_group + weekday +
                   breach_type2 + d_since_breach + total_affected + employee +
                   market_cap_m + roa + roe + debt_to_equity + cash_ratio +
                   overall_sector + year_since_ipo, data = CAR_full)
summary(eventstudy2)
event_robust <- coeftest(eventstudy2, vcov = vcovHC(eventstudy2, type = "HC1"))
event_robust = tidy(event_robust)
write.csv2(event_robust, "event_robust.csv", row.names = FALSE)
# --- Diagnostics ---
vif(eventstudy2)
plot(eventstudy2, which = 1)   # Residuals vs Fitted
plot(eventstudy2, which = 2)   # Q-Q Plot
plot(eventstudy2, which = 3)   # Scale-Location
plot(eventstudy2, which = 4)   # Cook's Distance
bptest(eventstudy2)           
shapiro.test(residuals(eventstudy2))
influencePlot(eventstudy2)
