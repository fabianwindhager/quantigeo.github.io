library(tidyverse)


bgld <- read_csv2("data/pop_midyear/001_bgld_pop_midyear.csv")
karnt <- read.csv2("data/pop_midyear/002_karnt_pop_midyear.csv")
noe1 <- read.csv2("data/pop_midyear/003_noe_1_pop_midyear.csv")
noe2 <- read.csv2("data/pop_midyear/003_noe_2_pop_midyear.csv")
ooe1 <- read.csv2("data/pop_midyear/004_ooe_1_pop_midyear.csv")
ooe2 <- read.csv2("data/pop_midyear/004_ooe_2_pop_midyear.csv")
slzbg <- read.csv2("data/pop_midyear/005_slzbg_pop_midyear.csv")
stmk1 <- read.csv2("data/pop_midyear/006_stmk_1_pop_midyear.csv")
stmk2 <- read.csv2("data/pop_midyear/006_stmk_2_pop_midyear.csv")
stmk3 <- read.csv2("data/pop_midyear/006_stmk_3_pop_midyear.csv")
tirol <- read.csv2("data/pop_midyear/007_tirol_pop_midyear.csv")
vrlbg <- read.csv2("data/pop_midyear/008_vrlbg_pop_midyear.csv")
wien <- read.csv2("data/pop_midyear/009_wien_pop_midyear.csv")


?rbind

dta <- rbind(bgld,
             karnt,
             noe1,
             noe2,
             ooe1,
             ooe2,
             slzbg,
             stmk1,
             stmk2,
             stmk3,
             tirol,
             vrlbg,
             wien)

dta$gemnr <- iconv(dta$gemnr, from = "", to = "UTF-8", sub = "")
dta <- dta %>% 
  mutate(nr = str_sub(gemnr, -7))
dta$nr <- str_remove_all(dta$nr, "[<>]")
dta$gemnr <- str_remove_all(dta$gemnr, "<\\d+>")
dta <- dta %>% 
  select(jahr, gemnr, n, nr)
dta <- dta %>% 
  rename(gemname = gemnr,
         yearmean_pop = n,
         gemnr = nr)
dta <- dta %>% 
  select(jahr, gemnr, yearmean_pop)
length(unique(dta$gemnr))


df$variable[df$variable == "old"] <- "new"

# Gemeinden Änderungen

gemchange <- read.csv2("data/pop_midyear/gemchange.csv") %>%
  drop_na()

gemchange <- gemchange %>% 
  rename(gemnr_alt = Gemeindekennziffer.alt,
         gemnr_neu = Gemeindekennziffer.neu,
         gemname_alt = Gemeinde.alt,
         gemname_neu = Gemeinde.neu,
         year = in.Kraft.seit,
         grund = Änderungsgrund) %>% 
  select(gemnr_alt,gemname_alt, gemnr_neu, gemname_neu, year, grund) %>% 
  mutate(jahr = str_sub(year, -4)) %>% 
  select(-year) %>% 
  filter(grund != "Teilung")


glimpse(gemchange)

# Doppelte Werte

unique(gemchange$gemnr_alt[duplicated(gemchange$gemnr_alt)])
unique(gemchange$gemnr_neu[duplicated(gemchange$gemnr_neu)])

table(gemchange$gemnr_alt)
table(gemchange$gemnr_neu)

dup_alt <- gemchange %>%
  filter(gemnr_alt %in% gemnr_alt[duplicated(gemnr_alt)])

dup_neu <- gemchange %>%
  filter(gemnr_neu %in% gemnr_alt[duplicated(gemnr_neu)])

gemchange <- gemchange %>% 
  filter(!gemnr_alt %in% c("61040", "62227", "62248", "62336", "62347", "62349") &
          !gemnr_neu %in% c("61040", "62227", "62248", "62336", "62347", "62349"))

gemchange <- gemchange %>% 
  select(gemnr_alt, gemnr_neu)
  

gemchange$gemnr_neu <- as.character(gemchange$gemnr_neu)
dta <- left_join(dta, gemchange, by = c("gemnr" = "gemnr_neu"))

