## Trabajo final de asignatura Data Sience
### Alumna: Sofya Khamova

install.packages("readxl")
library(readxl)

birth <- read_excel("birth rate.xlsx")
gdp <- read_excel("pib total por paises.xlsx")

names(birth)
names(gdp)

names(birth)[names(birth) == "TIME"] <- "country"
names(birth)[names(birth) == "2024"] <- "year"

names(birth)

names(gdp)[names(gdp) == "Country Name"] <- "country"
names(gdp)[names(gdp) == "2024 [YR2024]"] <- "year"

names(gdp)

library(tidyverse)

birth_2024 <- birth %>%
  select(country, year) %>%
  rename(birth_rate = year) %>%
  filter(country != "geo (labels)")

gdp_2024 <- gdp %>%
  select(country, year) %>%
  rename(gdp = year)

birth_2024$birth_rate <- as.numeric(birth_2024$birth_rate)
gdp_2024$gdp <- as.numeric(gdp_2024$gdp)

data <- merge(
  birth_2024,
  gdp_2024,
  by = "country"
)

head(data)
dim(data)

library(ggplot2)

ggplot(data, aes(x = gdp, y = birth_rate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "GDP and Birth Rate (2024)",
    x = "GDP",
    y = "Birth Rate"
  )

cor(data$gdp, data$birth_rate, use = "complete.obs")

model <- lm(birth_rate ~ gdp, data = data)

summary(model)

data$log_gdp <- log(data$gdp)

ggplot(data, aes(x = log_gdp, y = birth_rate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "GDP (log) and Birth Rate 2024",
    x = "Log GDP",
    y = "Birth Rate"
  )