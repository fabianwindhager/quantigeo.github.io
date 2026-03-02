library(tidyverse)


dta1 <- read.csv2("C:/Users/fwindhager/001_PROJECTS/002_CURB/bevstand_tirol_geschl_5alter__0_44j.csv", fileEncoding = "latin1")
dta2 <- read.csv2("C:/Users/fwindhager/001_PROJECTS/002_CURB/bevstand_tirol_geschl_5alter__45_100j.csv", fileEncoding = "latin1")
dta3 <- read.csv2("C:/Users/fwindhager/001_PROJECTS/002_CURB/bevstand_tirol_2002_geschl_5alter__0_44j.csv", fileEncoding = "latin1")
dta4 <- read.csv2("C:/Users/fwindhager/001_PROJECTS/002_CURB/bevstand_tirol_2002_geschl_5alter__45_100j.csv", fileEncoding = "latin1")


dta <- rbind(dta1, 
             dta2,
             dta3,
             dta4)

dta$Anzahl[dta$Anzahl == "-"] <- "0"
names(dta)
dta <- dta %>% 
  rename(jahr = 1,
         gemnr = 2,
         geschl = 3,
         alter = 4,
         n = 6) %>% 
  select(-Werte)


dta$gemnr <- iconv(dta$gemnr, from = "", to = "UTF-8", sub = "")

dta <- dta %>% 
  mutate(nr = str_sub(gemnr, -7))

dta$nr <- str_remove_all(dta$nr, "[<>]")
dta$gemname <- str_remove_all(dta$gemnr, "<\\d+>")
dta <- dta %>% 
  select(-gemnr) %>% 
  rename(gemnr = nr) %>% 
  select(jahr, gemnr, gemname, geschl, alter, n)


# Gemtypen

typ <- read.csv2("data/003_gliederungen_nach_städtischen_und_ländlichen_gebieten(1).csv")

typ$gemnr <- as.character(typ$gemnr)

typ <- typ %>% 
  rename(gemname = Name,
         typ = Wert) %>% 
  select(-gemname)


## Recoding typ$typ into typ$typ_rec
typ$typ <- typ$typ %>%
  as.character() %>%
  fct_recode(
    "Städte" = "101",
    "Städte" = "102",
    "Städte" = "103",
    "Regionalzentren" = "210",
    "Regionalzentren" = "220",
    "Suburbaner Raum" = "310",
    "Suburbaner Raum" = "320",
    "Suburbaner Raum" = "330",
    "Ländlicher Raum" = "410",
    "Ländlicher Raum" = "420",
    "Ländlicher Raum" = "430"
  )


dta <- left_join(dta, typ, by = "gemnr")
dta$n <- as.numeric(dta$n)
dta$alter <- as.factor(dta$alter)
dta$typ <- as.factor(dta$typ)

glimpse(dta)
write.csv(dta, "data/Bevölkerungspyramide.csv", row.names = FALSE)

# Poppyramid

pyramid <- dta %>%
  mutate(pop_pyramid = ifelse(geschl=="männlich", -n, n))

pyramid <- pyramid %>%
  mutate(alter=fct_inorder(alter))

ggplot(pyramid, aes(x=pop_pyramid, y=alter, fill=typ)) + 
  geom_col() +
  scale_fill_manual(
    values = c("Städte" = "#b35e6b",
               "Suburbaner Raum" = "#f5ca82",
               "Regionalzentren" = "#fdf453",
               "Ländlicher Raum" = "#84b870"))+
  scale_x_continuous(labels=abs) +
  geom_vline(xintercept=0, color="black") +
  labs(x="Bevölkerung",
       y="Altersgruppe",
       fill="",
       title = "Bevölkerungspyramide Tirol nach Raumtyp 2002/2025")+
  facet_grid(.~jahr)+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="text", x=-15300, y=20, label="Männer", size=5) +
  annotate(geom="text", x=15300, y=20, label="Frauen", size=5) 
