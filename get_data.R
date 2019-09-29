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


p_main <- df %>%
  #filter(year >= 2000) %>%
  group_by(year) %>%
  summarise(all = n(),
            nas = sum(is.na(value)),
            min_value = min(value, na.rm = T),
            max_value = max(value, na.rm = T),
            avg_value = mean(value, na.rm = T)) %>%
  filter(all != nas) %>%
  ggplot() +
  geom_ribbon(aes(x = year,
                  ymin = min_value,
                  ymax = max_value),
              fill = 'gray') +
  geom_text(aes(x = max(year)-1,
                y = max(avg_value),
                label = 'Average over time'),
            color = 'gray30') + 
  geom_text(aes(x = max(year) - 1,
                y = max(max_value),
                label = 'Maximum values over time'),
            color = 'gray30') +
  geom_text(aes(x = max(year) - 1,
                y = min(min_value),
                label = 'Minimum values over time'),
            color = 'gray30') +
  geom_point(aes(x = year,
                 y = avg_value)) +
  theme_classic() +
  labs(title = 'Minimum and maximum yearly scores',
       subtitle = 'Scores based on women participating in the three decisions (own health care, major household purchases, and visiting family) 
(% of women age 15-49)',
       y = 'Scores',
       x = 'Year') 

ggsave('plot.png', p_main,width = 25, height = 13, units = 'cm') 
