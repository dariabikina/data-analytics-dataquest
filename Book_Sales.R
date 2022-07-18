library(tidyverse)
library(lubridate)
df <- read.csv("https://query.data.world/s/3jk3izpnbj6n5fyrcs4jcghzdwxo2p", header=TRUE, stringsAsFactors=FALSE)
df_without_nas <- df %>% filter(is.na(user_submitted_review)!=TRUE)
df_without_na_purchases <- df_without_nas %>% filter(is.na(total_purchased) !=TRUE)
mean_books_purchased_in_an_order <- round(mean(df_without_na_purchases$total_purchased), digits=0)
df_without_nas <- df_without_nas %>% mutate(new_total_purchased = ifelse(is.na(total_purchased)==TRUE, mean_books_purchased_in_an_order, total_purchased))
unique_reviews <- unique(df_without_nas$user_submitted_review)

positive_or_not <- function(review) {
  case_when(str_detect(review, "not recommend") ~ "Negative",
    str_detect(review, "Awesome") ~ "Positive",
    str_detect(review, "OK") ~ "Neutral",
    str_detect(review, "okay") ~ "Neutral",
    str_detect(review, "Never read a better book") ~ "Positive",
    str_detect(review, "learned a lot") ~ "Positive",
    str_detect(review, "not needed") ~ "Negative",
    str_detect(review, "other books were better") ~ "Negative",
    str_detect(review, "Hated") ~ "Negative",
    TRUE ~ as.character(review))}

df_without_nas <- df_without_nas %>% mutate(review = positive_or_not(user_submitted_review))

df_without_nas$date <- mdy(df_without_nas$date)

df_without_nas <- df_without_nas %>% mutate(`New Program Active` = ifelse(date < ymd("2019-07-01"), "No", "Yes"))

sales_before_and_after <- df_without_nas %>% group_by(`New Program Active`) %>% summarise(`Books sold` = sum(new_total_purchased))

df_business <- df_without_nas %>% filter(customer_type == "Business")

df_individual <- df_without_nas %>% filter(customer_type == "Individual")

sales_business <- df_business %>% group_by(`New Program Active`) %>% summarise(`Books sold` = sum(new_total_purchased))

sales_individual <- df_individual %>% group_by(`New Program Active`) %>% summarise(`Books sold` = sum(new_total_purchased))

df_positive <- df_without_nas %>% filter(review == "Positive")

df_negative <- df_without_nas %>% filter(review == "Negative")

df_neutral <- df_without_nas %>% filter(review == "Neutral")

sales_positive <- df_positive %>% group_by(`New Program Active`) %>% summarise(`Books sold` = sum(new_total_purchased))

sales_negative <- df_negative %>% group_by(`New Program Active`) %>% summarise(`Books sold` = sum(new_total_purchased))

sales_neutral <- df_neutral %>% group_by(`New Program Active`) %>% summarise(`Books sold` = sum(new_total_purchased))

df_positive_and_neutral <- rbind(df_positive, df_neutral)

sales_positive_and_neutral <- df_positive_and_neutral %>% group_by(`New Program Active`) %>% summarise(`Books sold` = sum(new_total_purchased))
