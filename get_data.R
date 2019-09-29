library(tidyverse)
library(lettercase)
library(magrittr)

file_list <- list.files(path="data/")

df <- readr::read_csv(paste0('data/', file_list[1]), skip = 3)

df_additional_1 <-readr::read_csv(paste0('data/', file_list[2]))
df_additional_2 <-readr::read_csv(paste0('data/', file_list[3]))

names(df) %<>% make.names()

df %<>%
  select(-Indicator.Code, -Indicator.Name) %>%
  gather(year, value, -Country.Name, -Country.Code) %>%
  mutate(year = parse_number(year))

df %>%
  group_by(Country.Name) %>%
  summarise(all = n(),
            nas = sum(is.na(value))) %>%
  mutate(na_percentage = nas/all) %>%
  arrange(na_percentage)

p <- df %>%
  ggplot() + 
  geom_point(aes(x = year, y = value,
                color = Country.Name)) +
  guides(color = FALSE) + 
  xlim(2000, 2020) 
ggplotly(p)


top_20_countries <- df %>%
  filter(year >= 2000) %>%
  group_by(Country.Name) %>%
  summarise(all = n(),
            nas = sum(is.na(value))) %>%
  mutate(na_percentage = nas/all) %>%
  arrange(na_percentage) %>%
  head(20) %>%
  pull(Country.Name)


df %>%
  filter(Country.Name %in% top_20_countries) %>%
  ggplot() +
  geom_point(aes(x = year, 
                 y = value,
                 color = Country.Name)) +
  xlim(2000, 2020)


df %>%
  #filter(year >= 2000) %>%
  group_by(year) %>%
  summarise(all = n(),
            nas = sum(is.na(value)),
            min_value = min(value, na.rm = T),
            max_value = max(value, na.rm = T)) %>%
  filter(all != nas) %>%
  ggplot() +
  geom_ribbon(aes(x = year,
                  ymin = min_value,
                  ymax = max_value),
              fill = 'gray') +
  theme_classic() +
  labs(title = 'Minimum and maximum yearly scores')
 