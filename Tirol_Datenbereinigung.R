# Import Bevölkerungsdaten Tirol im Jahresdurchschnitt
pop_tirol <- read.csv2("data/Bevölkerung_Jahresdurchschnitt_Tirol.csv", dec = ",")

pop_tirol$gemnr <- iconv(pop_tirol$gemnr, from = "", to = "UTF-8", sub = "")

pop_tirol <- pop_tirol %>% 
  mutate(nr = str_sub(gemnr, -7))

pop_tirol$nr <- str_remove_all(pop_tirol$nr, "[<>]")
pop_tirol$gemnr <- str_remove_all(pop_tirol$gemnr, "<\\d+>")

pop_tirol <- pop_tirol %>% 
  select(jahr, gemnr, anz, nr)

pop_tirol <- pop_tirol %>% 
  rename(gemname = gemnr,
         yearmean_pop = anz,
         gemnr = nr)

pop_tirol <- pop_tirol %>% 
  select(jahr, gemnr, yearmean_pop) %>% 
  drop_na()

length(unique(pop_tirol$gemnr))

# Matrei am Brenner <70370>	 Ab 01.01.2022 aus den Gemeinden Matrei (70327), Mühlbachl (70330) und Pfons (70341)

gem_merge <- c(70327, 70330, 70341)

matrei_merge <- pop_tirol %>%
  filter(gemnr %in% gem_merge) %>%
  group_by(jahr) %>%
  summarise(yearmean_pop = sum(yearmean_pop, na.rm = TRUE)) %>%
  mutate(
    name  = "Matrei am Brenner",
    gemnr = "70370") %>%
  select(jahr, gemnr, yearmean_pop)

glimpse(matrei_merge)
glimpse(pop_tirol)


pop_tirol <- bind_rows(pop_tirol, matrei_merge)

pop_tirol <- pop_tirol %>%
  filter(!gemnr %in% gem_merge) %>%
  bind_rows(matrei_merge)


write.csv2(pop_tirol, "data/Bevölkerung_Jahresdurchschnitt_Tirol2.csv")