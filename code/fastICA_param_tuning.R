library(fastICA)
library(e1071)     # for kurtosis
library(infotheo)  # for mutual information 
library(tidyverse) # for read_csv and data manipulation
library(pracma)
library(dplyr)

weather =  read_csv("data/full_weather.csv")

ica_variables = weather %>%
  select("temperature_2m_mean (°C)","shortwave_radiation_sum (MJ/m²)",
         "et0_fao_evapotranspiration (mm)", "daylight_duration (s)",
         "sunshine_duration (s)", "precipitation_sum (mm)",
         "precipitation_hours (h)", "cloud_cover_mean (%)",
         "dew_point_2m_mean (°C)","wind_gusts_10m_mean (m/s)",
         "wind_speed_10m_mean (m/s)" )

weather_scaled <- scale(ica_variables)

set.seed(123)
n_comp <- 4
ica <- fastICA(weather_scaled, n.comp = n_comp, alg.typ = "deflation", 
               fun = "logcosh", method = "C", maxit = 1000)

#mean kurtosis 
kurt_vals <- apply(ica$S, 2, kurtosis)
mean_kurt  <- mean(abs(kurt_vals))

# mean mutual information 
S_disc <- discretize(ica$S, nbins = ceiling(sqrt(nrow(ica$S))))
mi_pairs <- combn(ncol(S_disc), 2, function(x) {
  mutinformation(S_disc[, x[1]], S_disc[, x[2]])
})
mean_mi <- mean(mi_pairs)

#average correlation 
corr_matrix <- cor(ica$S)
avg_abs_corr <- mean(abs(corr_matrix[upper.tri(corr_matrix)]))

# stability 
n_boot <- 100
stab_matrix <- matrix(NA, n_boot, n_comp)

for (i in 1:n_boot) {
  boot_idx <- sample(1:nrow(weather_scaled), replace = TRUE)
  boot_ica <- fastICA(weather_scaled[boot_idx, ], n.comp = n_comp, method = "C")
  rot <- procrustes(as.matrix(boot_ica$S), as.matrix(ica$S))
  cor_matrix <- abs(cor(rot$P, ica$S))
  stab_matrix[i, ] <- apply(cor_matrix, 2, max)
}
mean_stability <- mean(stab_matrix, na.rm = TRUE)

# explained variance 
reconstructed <- ica$S %*% ica$A
explained_var <- 1 - (sum((weather_scaled - reconstructed)^2) / sum(weather_scaled^2))

# negentropy approx. 
negentropy_approx <- mean((kurt_vals^2) / 48)

# summary 
results <- data.frame(
  Metric = c("n", "Mean Abs Kurtosis", "Mean Mutual Info", "Avg Abs Correlation", 
             "Stability (Bootstrap)", "Explained Var Ratio", "Negentropy Approx"),
  Value = round(c(n_comp, mean_kurt, mean_mi, avg_abs_corr, mean_stability, explained_var, negentropy_approx), 3)
)

print(results)