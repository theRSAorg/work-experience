
#### Plots ####

# 1
lad_summary <- london_data %>%
  group_by(lad20nm, lad20cd) %>%
  summarise(
    avg_distance = mean(average_distance)
  ) %>%
  ungroup() %>%
  mutate(
    lad20nm = forcats::fct_reorder(lad20nm, desc(avg_distance))
  ) %>%
  arrange(avg_distance)

# 2
ggplot(lad_summary, aes(avg_distance, lad20nm)) +
  geom_col()

