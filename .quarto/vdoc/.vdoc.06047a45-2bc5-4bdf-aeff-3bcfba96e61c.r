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
  summarize(mean_pct_resid = mean(pct_resid), .groups = "drop") |>
  mutate(calendar_date = make_date(2001, month, date_of_month))

#
#
#
#
holiday_dips <- tibble::tribble(
  ~calendar_date, ~mean_pct_resid, ~holiday,
  as.Date("2001-12-25"), -39.4, "Christmas Day",
  as.Date("2001-07-04"), -26.1, "Independence Day",
  as.Date("2001-12-24"), -25.9, "Christmas Eve",
  as.Date("2001-01-01"), -24.3, "New Year's Day"
)

calendar_resid |>
  ggplot(aes(x = calendar_date, y = mean_pct_resid)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(
    data = holiday_dips,
    aes(label = holiday),
    vjust = -0.6,
    size = 3
  ) +
  labs(
    title = "Average Birth Residuals by Calendar Date",
    x = "Calendar Date",
    y = "Mean Percent Residual"
  )
#
#
#
#| cache: true
basketball_tibble <- read_excel("data/nba_recruits.xlsx") |>
  mutate(
    tier = factor(
      tier,
      levels = c("Never played", "Brief career", "Solid career", "All-Star level", "Superstar"),
      ordered = TRUE
    )
  ) |>
  mutate(
    recruit_group = factor(
      recruit_group,
      levels = c("#1–10", "#11–25", "#26–50", "#51–100", "Outside top 100"),
      ordered = TRUE
    )
  ) 
#
#
#
basketball_tibble |>
  ggplot(aes(x = recruit_group, fill = tier)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(
    title = "Career Tier by Recruit Group",
    x = "Recruit Group",
    y = "Proportion of Players",
    fill = "Career Tier"
  )
#
#
#
#
#
#
