#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(readxl)
#
#
#
#| cache: true
births_tibble <- read_xlsx("data/us_births_1994_2014.xlsx") |>
  mutate(
    day_of_week = factor(
      day_of_week,
      levels = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),
      ordered = TRUE
    )
  )
#
#
#
#
#
births_tibble |>
  group_by(month, date_of_month) |>
  summarize(avg_births = mean(births), .groups = "drop") |>
  mutate(month = factor(month, levels = 12:1)) |>
  ggplot(aes(x = date_of_month, y = month, fill = avg_births)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(
    title = "Average Births by Month and Date",
    subtitle = "Fewer births on holidays like Christmas and New Year's Day",
    x = "Date of Month",
    y = "Month",
    fill = "Average Births"
  )
#
#
#
#| cache: true
births_tibble |>
  filter(month == 12) |>
  mutate(
    date_group = case_when(
      date_of_month == 25 ~ "christmas",
      date_of_month %in% 20:24 | date_of_month %in% 27:30 ~ "surrounding",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(date_group)) |>
  group_by(year, date_group) |>
  summarize(avg_births = mean(births), .groups = "drop") |>
  pivot_wider(names_from = date_group, values_from = avg_births) |>
  mutate(christmas_pct = (christmas / surrounding) * 100) |>
  select(year, christmas, surrounding, christmas_pct) |>
  summary()
#
#
#
#
births_tibble |>
  filter(month == 12, date_of_month == 25) |>
  select(year, day_of_week) |>
  distinct() |>
  left_join(
    births_tibble |>
      filter(month == 12) |>
      mutate(
        date_group = case_when(
          date_of_month == 25 ~ "christmas",
          date_of_month %in% 20:24 | date_of_month %in% 27:30 ~ "surrounding",
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(date_group)) |>
      group_by(year, date_group) |>
      summarize(avg_births = mean(births), .groups = "drop") |>
      pivot_wider(names_from = date_group, values_from = avg_births) |>
      mutate(pct_of_baseline = (christmas / surrounding) * 100),
    by = "year"
  ) |>
  ggplot(aes(x = year, y = pct_of_baseline, color = day_of_week)) +
  geom_line() +
  geom_point(size = 3) +
  labs( 
    title = "Christmas Day Births as a Percentage of Baseline",
    x = "Year",
    y = "Percentage of Baseline (%)",
    color = "Day of Week"
  )
#
#
#
births_model <- lm(births ~ factor(year) + factor(month) + day_of_week, data = births_tibble)
summary(births_model)$r.squared
#
#
#
births_adjusted <- births_tibble |>
  mutate(pct_resid = residuals(births_model) / mean(births) * 100)
#
#
#
calendar_resid <- births_adjusted |>
  filter(!(month == 2 & date_of_month == 29)) |>
  group_by(month, date_of_month) |>
  summarize(pct_resid = mean(pct_resid), .groups = "drop") |>
  mutate(date = make_date(2001, month, date_of_month))
#
#
#
#
#
#
