library(DBI)
library(RPostgres)
library(targets)
library(dplyr)
library(ggplot2)
library(yardstick)
library(vip)
library(readr)
library(tune)
library(tidyr)
library(workflows)
library(parsnip)
library(rsample)
library(themis)
library(corrplot)
library(caret)
library(nnet)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "postgres",
  host = "aws-0-eu-west-2.pooler.supabase.com",
  user = "pgstudent.rvdwflidqvcvffdccwrh",
  password = "0%jkXK^tjMZwuG",
  port=5432
)

dbListTables(con)

fire <- tbl(con, "fire_rescue_extrication_casualties") %>% collect()
finance <- tbl(con, "stats19_by_financial_year") %>% collect()
dbDisconnect(con)

glimpse(fire)
glimpse(finance)

df <- fire %>%
  left_join(finance, by = "financial_year")
write_csv(df, "fire.csv")

head(df, 10)
str(df)

sapply(df, function(x) sum(is.na(x)))

df <- df %>%
  mutate(across(c(extrication, sex, age_band, casualty_severity, financial_year), as.factor))

table(df$extrication)
table(df$sex)
table(df$age_band)
table(df$casualty_severity)
table(df$extrication, df$sex)
table(df$extrication, df$age_band)

ggplot(df, aes(sex)) +
  geom_bar() +
  labs(title = "Sex Counts")

ggplot(df, aes(age_band)) +
  geom_bar() +
  labs(title= "Age Band Counts")

# Extrication by sex
ggplot(df, aes(x = sex, fill = extrication)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Extrication Method by Sex")

# Extrication by age band
ggplot(df, aes(x = age_band, fill = extrication)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Extrication Method by Age Band")

colSums(df == "Unknown")
str(df)

df_clean <- df %>%
  filter(sex != "Unknown", age_band != "Unknown", extrication != "Unknown")
df_clean <- droplevels(df_clean)

df_clean <- df_clean %>%
  mutate(extrication_rate = n_casualties / number_of_stat19_reported_casualties)

str(df_clean)
table(df_clean$extrication)

rate_summary <- df_clean %>%
  group_by(age_band, sex) %>%
  summarise(mean_rate = mean(extrication_rate, na.rm = TRUE))

ggplot(rate_summary, aes(x = age_band, y = mean_rate, fill = sex)) +
  geom_col(position = "dodge") +
  labs(title = "Mean Extrication Rate by Age Band and Sex",
       x = "Age Band", y = "Mean Extrication Rate") +
  scale_y_continuous(labels = scales::percent)

ggplot(df_clean, aes(x = sex, fill = extrication)) +
  geom_bar(position = "fill") +
  facet_wrap(~age_band) +
  labs(y="Proportion", title="Extrication Method by Age and Sex")

# Multinormial
model <- multinom(extrication ~ sex + age_band, data = df_clean)
summary(model)

model_int <- multinom(extrication ~ sex * age_band, data = df_clean)
summary(model_int)

glm_model <- glm(
  cbind(n_casualties, number_of_stat19_reported_casualties - n_casualties) ~ sex + age_band,
  family = binomial,
  data = df_clean
)
summary(glm_model)

AIC(model, model_int, glm_model)

str(df_clean)

