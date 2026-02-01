############################################################
# Applied Econometrics Coursework
# Question 1: Harmonising, Visualising, and Interpreting Data
# Group 16 – Sub-industry: Steel
############################################################

# -------------------------
# Packages
# -------------------------
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# =========================================================
# Q1(a): US Federal Funds Rate
# =========================================================

fed <- read_csv("FEDFUNDS.csv")

fed_clean <- fed %>%
  mutate(
    date = ymd(observation_date),
    fedfunds = as.numeric(FEDFUNDS)
  ) %>%
  filter(!is.na(date), !is.na(fedfunds))

p1a <- ggplot(fed_clean, aes(x = date, y = fedfunds)) +
  geom_line() +
  labs(
    title = "US Federal Funds Rate",
    x = "Date",
    y = "Percent"
  ) +
  theme_minimal()

ggsave("Q1a_FedFunds.png", p1a, width = 9, height = 4.5, dpi = 300)

# =========================================================
# Q1(b): Market Excess Returns (Mkt - RF)
# =========================================================

ff <- read_csv("F-F_Research_Data_Factors.csv", skip = 3)

ff_clean <- ff %>%
  rename(ym = `...1`) %>%
  mutate(
    ym = as.character(ym),
    date = ymd(paste0(ym, "01")),
    mktrf = as.numeric(`Mkt-RF`)
  ) %>%
  filter(!is.na(date), !is.na(mktrf))

p1b <- ggplot(ff_clean, aes(x = date, y = mktrf)) +
  geom_line() +
  labs(
    title = "Market Excess Returns (Mkt - RF)",
    x = "Date",
    y = "Percent"
  ) +
  theme_minimal()

ggsave("Q1b_MktRF.png", p1b, width = 9, height = 4.5, dpi = 300)

# =========================================================
# Q1(c): Returns Across 12 Industry Portfolios
# =========================================================

ind12 <- read_csv("12_Industry_Portfolios.csv", skip = 11)

ind12_clean <- ind12 %>%
  rename(ym = 1) %>%
  mutate(
    ym = as.character(ym),
    date = ymd(paste0(ym, "01"))
  ) %>%
  select(-ym) %>%
  pivot_longer(
    cols = -date,
    names_to = "industry",
    values_to = "return"
  ) %>%
  mutate(return = as.numeric(return)) %>%
  filter(!is.na(date), !is.na(return))

p1c <- ggplot(ind12_clean, aes(x = date, y = return, colour = industry)) +
  geom_line(alpha = 0.7) +
  labs(
    title = "Returns Across 12 Industry Portfolios",
    x = "Date",
    y = "Percent",
    colour = "Industry"
  ) +
  theme_minimal()

ggsave("Q1c_12Industries.png", p1c, width = 10, height = 5, dpi = 300)

# =========================================================
# Q1(d): Steel Sub-Industry Returns (Group 16)
# =========================================================

ind49 <- read_csv("49_Industry_Portfolios.csv", skip = 11)

steel_clean <- ind49 %>%
  rename(ym = 1) %>%
  mutate(
    ym = as.character(ym),
    date = ymd(paste0(ym, "01")),
    steel = as.numeric(Steel)
  ) %>%
  select(date, steel) %>%
  filter(!is.na(date), !is.na(steel))

p1d <- ggplot(steel_clean, aes(x = date, y = steel)) +
  geom_line() +
  labs(
    title = "Steel Industry Returns",
    x = "Date",
    y = "Percent"
  ) +
  theme_minimal()

ggsave("Q1d_Steel.png", p1d, width = 9, height = 4.5, dpi = 300)

############################################################
# End of Question 1
############################################################
