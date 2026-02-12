library(tidyverse)
library(patchwork)
library(ordinal)
library(performance)
library(DHARMa)
library(car)
library(sjstats)
library(lattice)

baseline = read_csv("data/baseline.csv")
daily_data = read_csv("data/daily_data.csv")
weather = read_csv("data/weather_reduced.csv")

#Merging Datasets
daily_baseline = left_join(daily_data, baseline, by = "Study ID")
data = left_join(daily_baseline, weather, by = c("day" ,  "location"))

data$ordered = factor(data$`Total-F`, #replace
                       levels = sort(unique(data$Q3)), # Ensures correct order
                       ordered = TRUE)

model_ordinal = clmm(ordered ~  
                        IC1 + IC2 + IC3  + IC4 +
                        (1 + IC1 + IC2 + IC3  + IC4 | `Study ID`),
                      model = F, 
                      link = "logit",
                      data = data, 
                      control = clmm.control( maxIter = 1000, gradTol = 1e-4)
)
summary(model_ordinal)

# extract all necessary info
ct = summary(model_ordinal)$coefficients
OR_table = data.frame(
  Predictor = rownames(ct),
  Estimate  = ct[, "Estimate"],
  SE        = ct[, "Std. Error"],
  OR        = exp(ct[, "Estimate"]),
  Lower_CI  = exp(ct[, "Estimate"] - 1.96 * ct[, "Std. Error"]),
  Upper_CI  = exp(ct[, "Estimate"] + 1.96 * ct[, "Std. Error"]),
  p_value   = ct[, "Pr(>|z|)"]
)
OR_table[-1] = round(OR_table[-1], 3)


#proportional odds assumption
model_clm <- clm(ordered ~ IC1 + IC2 + IC4 + IC3, data = data)
p_odds_test <- nominal_test(model_clm)
print(p_odds_test)

#multicollinearity
mod_vif <- lm(as.numeric(ordered) ~ IC1 + IC2 + IC4 + IC3, data = data)
vif_values <- vif(mod_vif)
print(vif_values)

# homogenity of variance for random effects 
re_effects <- ranef(model_ordinal)$`Study ID`[,1]  # First column for intercept
shapiro.test(re_effects)  # Shapiro-Wilk
qqnorm(re_effects); qqline(re_effects, col = "red")
hist(re_effects, breaks = 20, main = "Random Effects Distribution")

#linearity of logit 
cutpoints <- levels(data$ordered)[-nlevels(data$ordered)]

for(i in seq_along(cutpoints)) {
  data$binary <- as.numeric(data$ordered <= cutpoints[i])
  data[[paste0("logit_", i)]] <- predict(glm(binary ~ IC1 + IC2 + IC3 + IC4, 
                                             family = binomial, data = data))
}
# Modern tidy pivot_longer (drop logit_ cols after)
logit_cols <- paste0("logit_", seq_along(cutpoints))
plot_data <- data %>%
  select(all_of(c("IC1", "IC2", "IC3", "IC4", logit_cols))) %>%
  pivot_longer(cols = all_of(logit_cols), names_to = "cutpoint", 
               names_prefix = "logit_", values_to = "logit") %>%
  mutate(cutpoint = factor(cutpoint, levels = cutpoints))
# Single faceted plot
plot_data %>%
  pivot_longer(c(IC1:IC4), names_to = "IC", values_to = "value") %>%
  ggplot(aes(value, logit, color = cutpoint)) +
  geom_point(alpha = 0.3) + geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~IC, scales = "free_x", 
             labeller = labeller(IC = c("IC1" = "Rain&Storm", "IC2" = "Hot&Humid", 
                                        "IC3" = "Clear&Bright", "IC4" = "Dry&Windy"))) +
  labs(color = "Threshold") + theme_minimal()


#performance metrics 

#aic, bic, loglikelihood
model_aic <- AIC(model_ordinal)
model_bic <- BIC(model_ordinal)
model_logLik <- as.numeric(logLik(model_ordinal))

#condition number 
hess <- model_ordinal$Hessian
eigenvalues <- eigen(hess)$values
condition_number <- max(abs(eigenvalues)) / min(abs(eigenvalues))

# RE variance
re_var <- as.numeric(VarCorr(model_ordinal)$`Study ID`[1,1])

# ICC
icc_value <- re_var / (re_var + (pi^2 / 3))

#Pseudo r-square
null_model <- clmm(ordered ~ 1 + (1 | `Study ID`), 
                   data = data, 
                   link = "logit",
                   control = clmm.control(maxIter = 1000, gradTol = 1e-4))
lnL_full <- as.numeric(logLik(model_ordinal))
lnL_null <- as.numeric(logLik(null_model))
n <- nobs(model_ordinal)
r2_cox_snell <- 1 - exp((2/n) * (lnL_null - lnL_full))
r2_nagelkerke <- r2_cox_snell / (1 - exp((2/n) * lnL_null))

# likelihood ratio test
anova_results <- anova(null_model, model_ordinal)
lrt_p_value <- na.omit(anova_results$`Pr(>Chisq)`)[1]

#performance summary 
performance_metrics <- data.frame(
  Metric = c("AIC", "BIC", "Log-Likelihood", "Condition Number", 
             "Intercept Variance", "ICC", "Cox & Snell R²", 
             "Nagelkerke R²", "LRT p-value"),
  Value = round(c(
    as.numeric(model_aic)[1],
    as.numeric(model_bic)[1],
    as.numeric(model_logLik)[1],
    as.numeric(condition_number)[1],
    as.numeric(re_var)[1],
    as.numeric(icc_value)[1],
    as.numeric(r2_cox_snell)[1],
    as.numeric(r2_nagelkerke)[1],
    as.numeric(lrt_p_value)[1]
  ), 3)
)

print(performance_metrics)
