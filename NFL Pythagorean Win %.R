library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggrepel)

nfl_data <-read_excel("NFL Team Stats.xlsx")

nfl_data <- nfl_data %>%
  mutate(Pyth_Win_Per = (PF ^ 2 + 0.5 * (PF == PA)) / (PF ^ 2 + PA ^ 2 + (PF == PA) * 0.5))

nfl_data <- nfl_data %>% mutate(residuals_pyt = Win_Per - Pyth_Win_Per)

nfl_data %>% summarize(rmse = sqrt(mean(residuals_pyt^2)))

top_teams <- nfl_data %>% arrange(desc(residuals_pyt)) %>% head(5)
bottom_teams <- nfl_data %>% arrange(residuals_pyt) %>% head(5)

ggplot(nfl_data, aes(x = PD, y = residuals_pyt)) + 
  geom_point(alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = 5) +
  labs(title = "2014 - 2023 NFL Pythagorean Wins",
       x = "Point Differential",
       y = "Pythagorean Win Residuals") +
  theme_classic() +
  scale_x_continuous(breaks = seq(-200, 200, by = 50)) +
  geom_point(data = top_teams, color = "forestgreen") +
  geom_text_repel(data = top_teams, color = "forestgreen",
                    aes(label = paste(Year, Team)), size = 3) +
  geom_point(data = bottom_teams, color = "red") +
  geom_text_repel(data = bottom_teams, color = "red",
                  aes(label = paste(Year, Team)), size = 3)

plot_ly(nfl_data, x = ~PD, y = ~residuals_pyt, text = ~paste("Team:", Team, "<br>Year:", Year),
        type = 'scatter', mode = 'markers')