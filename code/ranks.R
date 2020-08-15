df <- trade_data_raw %>% 
  as_tibble() %>% 
  filter(partner_iso %in% c("ind", "usa", "chn")) 


df

trade_data_raw %>% 
  count(partner_fullname_english) %>% 
  filter(str_detect(partner_fullname_english, "India"))

points <- df %>% 
  group_by(year, reporter_fullname_english) %>% 
  mutate(import_rank = dense_rank(-import_value_usd),
         export_rank = dense_rank(-export_value_usd)) %>% 
  ungroup() %>% 
  mutate(period = case_when(
    between(year, 1962, 1971) ~ "1962-1971",
    between(year, 1972, 1991) ~ "1972-1991",
    between(year, 1992, 2008) ~ "1992-2008",
    between(year, 2009, 2018) ~ "2009-2018"
  )) 


table <- points %>% 
  filter(!(reporter_fullname_english %in% c("China",
                                            "India (excludes Sikkim until 1975)",
                                            "USA, Puerto Rico and US Virgin Islands (excludes Virgin Islands until 1981)"))) %>% 
  arrange(reporter_iso, year) %>% 
  pivot_longer(export_value_usd:export_rank) %>% 
  unite(col = title,
        partner_fullname_english, name, remove = FALSE) %>% 
  select(-c(name, starts_with('partner'))) %>% 
  pivot_wider(names_from = title) %>%
  select(-reporter_iso)

write_csv(table, "outputs/overview_table.csv")

table %>% 
  filter(!(reporter_fullname_english %in% c("China",
                                            "India (excludes Sikkim until 1975)",
                                            "USA, Puerto Rico and US Virgin Islands (excludes Virgin Islands until 1981)"))) %>% 
  write_csv("outputs/table.csv")



table %>% 
  group_by(reporter_fullname_english, period) %>% 
  summarise(across(4:last_col(),
                   sum)) %>% 
  view()




table %>% 
  filter(!(reporter_fullname_english %in% c("China",
                                            "India (excludes Sikkim until 1975)",
                                            "USA, Puerto Rico and US Virgin Islands (excludes Virgin Islands until 1981)"))) %>% 
  # group_by(period) %>% 
  pivot_longer(ends_with('rank')) %>% 
  select(year:period, name, value) %>% 
  count(year, name, value) %>% 
  ungroup() %>% 
  unite(title, c("name", "value")) %>% 
  pivot_wider(names_from = title, values_from = n) %>% 
  write_csv("outputs/trade_ranks_wide.csv")
  # separate(name, sep = "_", into = c("partner", "trade_type"), extra = "merge") %>% 
  



table %>% 
  filter(!(reporter_fullname_english %in% c("China",
                                            "India (excludes Sikkim until 1975)",
                                            "USA, Puerto Rico and US Virgin Islands (excludes Virgin Islands until 1981)"))) %>% 
  # group_by(period) %>% 
  pivot_longer(ends_with('rank')) %>% 
  select(year:period, name, rank = value) %>% 
  separate(name, sep = "_", into = c("partner", "trade_type"), extra = "merge") %>% 
  count(period, partner, trade_type, rank) %>% 
  unite(title, c("partner", "trade_type")) %>% 
  mutate(title = str_replace(title, "rank", "country_years")) %>% 
  pivot_wider(names_from = "title", values_from = "n") %>% 
  mutate(rank = case_when(
    rank == 1 ~ "1 (first place)",
    rank == 2 ~ "2 (second place)",
    rank == 3 ~ "3 (third place)",
    is.na(rank) ~ "Didn't trade"
  )) %>% 
  write_csv("outputs/points_summary.csv")



points %>% 
  arrange(reporter_iso, year) %>% 
  pivot_longer(export_value_usd:export_points) %>% 
  filter(period == "2009-2018") %>% 
  pivot_wider() %>% 
  mutate(across(contains("value"),
                ~./1e9)) %>% 
  select(year, period, reporter_fullname_english, partner_fullname_english, starts_with('import')) %>% 
  filter(year == "2011")
  



table %>% 
  distinct(reporter_fullname_english) %>% 
  filter(str_detect(reporter_fullname_english, "USA"))


table %>% 
  filter(!(reporter_fullname_english %in% c("China",
                                            "India (excludes Sikkim until 1975)",
                                            "USA, Puerto Rico and US Virgin Islands (excludes Virgin Islands until 1981)"))) %>%  
  pivot_longer(ends_with("usd")) %>% 
  select(year:period, name, value) %>% 
  separate(name, sep = "_", into = c("partner", "trade_type"), extra = "merge") %>% 
  count(period, partner, trade_type, wt = value) %>% 
  mutate(n = n/1e9) %>% 
  unite(title, c("partner", "trade_type")) %>% 
  pivot_wider(names_from = "title", values_from = "n") %>% 
  write_csv("outputs/summary_values_billions.csv")


points %>% 
  filter(!(reporter_fullname_english %in% c("China",
                                            "India (excludes Sikkim until 1975)",
                                            "USA, Puerto Rico and US Virgin Islands (excludes Virgin Islands until 1981)"))) %>% 
  arrange(reporter_iso, year) %>% 
  pivot_longer(export_value_usd:export_points) %>% 
  filter(name == "export_value_usd") %>% 
  count(year, period, partner_fullname_english, wt = value) %>% 
  cah_plot(aes(x = year, y = n, colour = partner_fullname_english)) +
  geom_line() +
  facet_wrap(~period,
             scales="free") +
  theme(legend.position = "bottom") +
  scale_y_continuous(label = dollar_format(scale = 1/1e9,
                                           suffix = " billion")) +
  labs(x = NULL,
       y = "Export value USD",
       title = "Total value of exports from selected countries, to Indian Ocean littoral countries, over time",
       col = "Destination country") +
  scale_colour_wsj()

cah_save("outputs/exports_windows.png")



points %>% 
  filter(!(reporter_fullname_english %in% c("China",
                                            "India (excludes Sikkim until 1975)",
                                            "USA, Puerto Rico and US Virgin Islands (excludes Virgin Islands until 1981)"))) %>% 
  arrange(reporter_iso, year) %>% 
  pivot_longer(export_value_usd:export_points) %>% 
  filter(name == "export_value_usd") %>% 
  count(year, period, partner_fullname_english, wt = value) %>% 
  cah_plot(aes(x = year, y = n, colour = partner_fullname_english)) +
  geom_line() +
  geom_vline(data = tibble(date = c(1971,
                                    1991,
                                    2008)),
             aes(xintercept = date),
             linetype = 2) +
  theme(legend.position = "bottom") +
  scale_y_continuous(label = dollar_format(scale = 1/1e9,
                                           suffix = " billion")) +
  labs(x = NULL,
       y = "Export value USD",
       title = "Total value of exports from selected countries, to Indian Ocean littoral countries, over time",
       col = "Destination country") +
  scale_colour_wsj()


cah_save("outputs/exportss_continuous.png")



arg <- points %>% 
  filter(!(reporter_fullname_english %in% c("China",
                                            "India (excludes Sikkim until 1975)",
                                            "USA, Puerto Rico and US Virgin Islands (excludes Virgin Islands until 1981)"))) %>% 
  arrange(reporter_iso, year) %>% 
  pivot_longer(export_value_usd:export_rank, values_to = "rank") %>% 
  filter(str_detect(name, "rank")) %>% 
  rename(trade_type = name) %>% 
  count(year, period, partner_fullname_english, trade_type, rank, name = "count_of_rank") %>% 
  group_by(year, period, trade_type) %>% 
  filter(rank == 1) %>% 
  mutate(rank_of_ranks = dense_rank(-count_of_rank)) %>% 
  arrange(year, trade_type, rank_of_ranks) %>% 
  mutate(position = case_when(
    rank_of_ranks == 1 ~ "Most frequent number 1 trading partner",
    rank_of_ranks == 2 ~ "Second most frequent number 1 trading partner",
    rank_of_ranks == 3 ~ "Third most frequent number 1 trading partner",
    # is.na(rank_of_ranks) ~ "Most frequent number 1 trading partner",
  ))

arg %>% 
  filter(trade_type == "import_rank") %>% 
  ungroup() %>% 
  cah_plot(aes(x = year, y = position, col = partner_fullname_english)) +
  geom_line() +
  theme(legend.position = "bottom") +
  scale_colour_wsj()
