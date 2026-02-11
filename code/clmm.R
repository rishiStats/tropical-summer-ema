library(readxl)
library(dplyr)
library(ordinal)

baseline = read_csv("data/baseline.csv")
daily_data = read_csv("data/daily_data.csv")
weather = read_csv("data/weather_reduced.csv")

#Merging Datasets
daily_baseline = left_join(daily_data, baseline, by = "Study ID")
data = left_join(daily_baseline, weather_fin, by = c("day" ,  "location"))

data$ordered <- factor(data$`Total-F`, #replace
                       levels = sort(unique(data$Q3)), # Ensures correct order
                       ordered = TRUE)

model_ordinal <- clmm(ordered ~  
                        IC1 + IC2 + IC3  + IC4 +
                        (1 + IC1 + IC2 + IC3  + IC4 | `Study ID`),
                      model = F, 
                      link = "logit",
                      data = data, 
                      control = clmm.control( maxIter = 1000, gradTol = 1e-4)
)
summary(model_ordinal)