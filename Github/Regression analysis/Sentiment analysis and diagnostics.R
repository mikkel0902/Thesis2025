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
par(family = "serif")

# --- Data Preparation ---
CAR_full <- read_delim("car_fullfinal.csv", delim = ";", locale = locale(decimal_mark = ","))
CAR_full$weekday <- as.factor(CAR_full$weekday)
str(CAR_full)


# --- Initial Model ---
model_sentiment <- lm(sentiment ~ CAR_1 + cyber_score + year_group + weekday + 
                        breach_type2 + d_since_breach + total_affected + employee + 
                        market_cap_m + roa + roe + debt_to_equity + cash_ratio + 
                        overall_sector + year_since_ipo, data = CAR_full)
summary(model_sentiment)

# --- Diagnostics ---
vif(model_sentiment)
plot(model_sentiment, which = 1)   # Residuals vs Fitted
plot(model_sentiment, which = 2)   # Q-Q Plot
plot(model_sentiment, which = 3)   # Scale-Location
plot(model_sentiment, which = 4)   # Cook's Distance
bptest(model_sentiment)           
shapiro.test(residuals(model_sentiment))
influencePlot(model_sentiment)
# --- Robust SE ---
coeftest(model_sentiment, vcov = vcovHC(model_sentiment, type = "HC1"))

#Sentiment cleaned, with intercept
model_sentiment_int <- lm(sentiment ~ total_affected * year_since_ipo + CAR_1 + cyber_score + year_group + weekday + 
                              breach_type2 + d_since_breach + total_affected + employee + 
                              market_cap_m + roa + roe + debt_to_equity + cash_ratio + 
                              overall_sector + year_since_ipo, data = CAR_full)
summary(model_sentiment_int)
robust_sent1 <- coeftest(model_sentiment_int, vcov = vcovHC(model_sentiment_int, type = "HC1"))
robust_sent1 = tidy(robust_sent1)
write.csv2(robust_sent1, "robust_sent1.csv", row.names = FALSE)
# --- Diagnostics ---
vif(model_sentiment_int)
plot(model_sentiment_int, which = 1)   # Residuals vs Fitted
plot(model_sentiment_int, which = 2)   # Q-Q Plot
plot(model_sentiment_int, which = 3)   # Scale-Location
plot(model_sentiment_int, which = 4)   # Cook's Distance
bptest(model_sentiment_int)           
shapiro.test(residuals(model_sentiment_int))
influencePlot(model_sentiment_int)




#Sentiment Change model
model_sentiment_change <- lm(sentiment_change ~ CAR_1 + cyber_score + year_group + 
                               weekday + breach_type2 + d_since_breach + total_affected + 
                               employee + market_cap_m + roa + roe + debt_to_equity + 
                               cash_ratio + overall_sector + year_since_ipo, data = CAR_full)
summary(model_sentiment_change)

# --- Diagnostics ---
vif(model_sentiment_change)
plot(model_sentiment_change, which = 1:4)
bptest(model_sentiment_change)
shapiro.test(residuals(model_sentiment_change))
influencePlot(model_sentiment_change)
# --- Robust SE ---
coeftest(model_sentiment_change, vcov = vcovHC(model_sentiment_change, type = "HC1"))

#Sentiment change cleaned, with intercept
model_sentiment_change_int <- lm(sentiment_change ~ total_affected * year_since_ipo + CAR_1 + cyber_score + year_group + weekday + 
                                  breach_type2 + d_since_breach + total_affected + employee + 
                                  market_cap_m + roa + roe + debt_to_equity + cash_ratio + 
                                  overall_sector + year_since_ipo, data = CAR_full)
summary(model_sentiment_change_int)
robust_sent <- coeftest(model_sentiment_change_int, vcov = vcovHC(model_sentiment_change_int, type = "HC1"))
robust_sent = tidy(robust_sent)
write.csv2(robust_sent, "robust_sent.csv", row.names = FALSE)
# --- Diagnostics ---
vif(model_sentiment_change_int)
plot(model_sentiment_change_int, which = 1)   # Residuals vs Fitted
plot(model_sentiment_change_int, which = 2)   # Q-Q Plot
plot(model_sentiment_change_int, which = 3)   # Scale-Location
plot(model_sentiment_change_int, which = 4)   # Cook's Distance
bptest(model_sentiment_change_int)           
shapiro.test(residuals(model_sentiment_change_int))
influencePlot(model_sentiment_change_int)
