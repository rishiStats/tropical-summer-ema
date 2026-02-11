library(fastICA)
library(tidyverse)

weather =  read_csv("data/full_weather.csv")

ica_variables = weather %>%
  select("temperature_2m_mean (°C)","shortwave_radiation_sum (MJ/m²)",
         "et0_fao_evapotranspiration (mm)", "daylight_duration (s)",
         "sunshine_duration (s)", "precipitation_sum (mm)",
         "precipitation_hours (h)", "cloud_cover_mean (%)",
         "dew_point_2m_mean (°C)","wind_gusts_10m_mean (m/s)",
         "wind_speed_10m_mean (m/s)" )

weather_scaled <- scale(ica_variables)

#running ICA
set.seed(123)
n_comp <- 4
ica <- fastICA(weather_scaled, n.comp = n_comp, alg.typ = "deflation", 
               fun = "logcosh", method = "C", maxit = 1000)

#loading matrix
loadings <- as.data.frame(t(ica$A))
colnames(loadings) <- paste0("IC", 1:n_comp)
rownames(loadings) <- colnames(weather_scaled)
clean_loadings <- loadings %>%
  mutate(across(everything(), ~ ifelse(abs(.x) < 0.4, "-", as.character(round(.x, 2)))))
print(clean_loadings)

#relative contributions 
contrib <- t(round(prop.table(abs(ica$A), margin = 1) * 100, 1))
dimnames(contrib) <- list(colnames(weather_scaled), paste0("IC", 1:n_comp))
print(contrib)

#add names 
colnames(ica$S) <- paste0("IC", 1:n_comp)

# compiling with lag values 
weather_fin <- weather %>%
  select(1:6) %>%
  cbind(ica$S) %>%
  arrange(day) %>%
  # Create 1, 2, and 3-day lags for ALL ICs at once
  mutate(across(starts_with("IC"), 
                list(lag1 = ~lag(.x, 1), 
                     lag2 = ~lag(.x, 2), 
                     lag3 = ~lag(.x, 3)),
                .names = "{.col}_{.fn}"))


# reconstruction error
recon_error <- mean(abs(weather_scaled - (ica$S %*% ica$A)))
print(recon_error)

write_csv(weather_fin, "data/weather_reduced.csv")