library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

weather = read_csv("data/weather_reduced.csv")

component_labels <- c("IC1" = "Rain and Storm", 
                      "IC2" = "Hot and Humid", 
                      "IC3" = "Clear and Bright", 
                      "IC4" = "Dry and Windy")


weather_long <- weather %>%
  select(location, time, IC1, IC2, IC3, IC4) %>%
  pivot_longer(cols = starts_with("IC"), 
               names_to = "Component", 
               values_to = "Value") %>%
  mutate(Component = factor(Component, levels = paste0("IC", 1:4),
                            labels = component_labels),
         location = toTitleCase(location))  

#time-series across the years
p1 <- ggplot(weather_long, aes(x = time, y = Value, color = Component)) +
  geom_line(linewidth = 0.8) +
  facet_grid(Component ~ location, scales = "free_y") +
  scale_color_manual(values = 1:4) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Independent Components (June 2024 - May 2025)",
       x = "Month",
       y = "Signal Value") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)


start_date <- as.Date("2025-03-29")
end_date <- as.Date("2025-04-27")

weather_filtered <- weather_long %>%
  filter(time >= start_date & time <= end_date) %>%
  group_by(location) %>%
  mutate(day_number = as.numeric(time - min(time)) + 1) %>%
  ungroup()

# Chennai vs Chengalpattu comparision 
p2 <- ggplot(weather_filtered, aes(x = day_number, y = Value, color = Component)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ location, ncol = 1) +
  scale_color_manual(values = 1:4, labels = component_labels) +
  labs(title = "Independent Components during Study Period (29-Mar-2025 to 27-Apr-2025)",
       x = "Days",
       y = "Signal Value",
       color = "Component") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9),
        strip.text = element_text(size = 11, face = "bold"),
        plot.title = element_text(hjust = 0.5))

print(p2)

# side by side comparision 
p3 <- ggplot(weather_filtered, aes(x = day_number, y = Value, color = Component)) +
  geom_line(linewidth = 1.2) +
  facet_grid(Component ~ location, scales = "free_y") +
  scale_color_manual(values = 1:4) +
  labs(title = "Independent Components during Study Period (29-Mar-2025 to 27-Apr-2025)",
       x = "Days",
       y = "Signal Value") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(hjust = 0.5))

print(p3)