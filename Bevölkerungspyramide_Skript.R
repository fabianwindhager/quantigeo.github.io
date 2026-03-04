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
typ2 <- read.csv2("data/004_gliederungen_nach_städtischen_und_ländlichen_gebieten.csv")


typ$gemnr <- as.character(typ$gemnr)
typ2$gemnr <- as.character(typ2$gemnr)


typ <- typ %>% 
  select(-gemname)

typ2 <- typ2 %>% 
  select(-gemname)

typ <- left_join(typ, typ2, by = "gemnr")



## Recoding typ$typ into typ$typ_rec
typ$typ <- typ$typ %>%
  as.character() %>%
  fct_recode(
    "Urbaner Raum" = "101",
    "Urbaner Raum" = "102",
    "Urbaner Raum" = "103",
    "Regionalzentren" = "210",
    "Regionalzentren" = "220",
    "Suburbaner Raum" = "310",
    "Suburbaner Raum" = "320",
    "Suburbaner Raum" = "330",
    "Ländlicher Raum" = "410",
    "Ländlicher Raum" = "420",
    "Ländlicher Raum" = "430"
  )


## Recoding typ$typ into typ$typ_rec
typ$typ2 <- typ$typ2 %>%
  as.character() %>%
  fct_recode(
    "Städte" = "1",
    "Suburban" = "2",
    "Ländliche Gebiete" = "3"
  )



dta <- left_join(dta, typ, by = "gemnr")
dta$n <- as.numeric(dta$n)
dta$alter <- as.factor(dta$alter)
dta$typ <- as.factor(dta$typ)
dta$typ2 <- as.factor(dta$typ2)


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
    values = c("Urbaner Raum" = "#b35e6b",
               "Suburbaner Raum" = "#f5ca82",
               "Regionalzentren" = "#fdf453",
               "Ländlicher Raum" = "#84b870"))+
  scale_x_continuous(labels=abs) +
  geom_vline(xintercept=0, color="black") +
  labs(x="Bevölkerung",
       y="Altersgruppe",
       fill="",
       title = "Bevölkerungspyramide Tirol nach Raumtyp 2002/2025; \n Gliederung: Urban-Rural-Typologie der Statistik Austria")+
  facet_grid(.~jahr)+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="text", x=-15300, y=20, label="Männer", size=5) +
  annotate(geom="text", x=15300, y=20, label="Frauen", size=5) 


ggplot(pyramid, aes(x=pop_pyramid, y=alter, fill=typ2)) + 
  geom_col() +
  scale_fill_manual(
    values = c(
      "Städte" = "#b35e6b",
      "Suburban" = "#fdf453",
      "Ländliche Gebiete" = "#84b870"))+
  scale_x_continuous(labels=abs) +
  geom_vline(xintercept=0, color="black") +
  labs(x="Bevölkerung",
       y="Altersgruppe",
       fill="",
       title = "Bevölkerungspyramide Tirol nach Raumtyp 2002/2025; \n Gliederung: Grad der Urbanisierung der Europäischen Kommission")+
  facet_grid(.~jahr)+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate(geom="text", x=-15300, y=20, label="Männer", size=5) +
  annotate(geom="text", x=15300, y=20, label="Frauen", size=5) 



# Dependency Ratio
# (dependents/working-age pop) *100
# working age = 15-64

names(dta)

dep <- dta %>% 
  group_by(jahr, gemnr, gemname, alter, typ, typ2) %>% 
  summarise(n = sum(n)) %>% 
  ungroup()


## Recoding dep$alter into dep$alter_rec
dep$depend <- dep$alter %>%
  fct_recode(
    "dependent" = "10 bis 14 Jahre",
    "dependent" = "100 Jahre und älter",
    "working" = "15 bis 19 Jahre",
    "working" = "20 bis 24 Jahre",
    "working" = "25 bis 29 Jahre",
    "working" = "30 bis 34 Jahre",
    "working" = "35 bis 39 Jahre",
    "working" = "40 bis 44 Jahre",
    "working" = "45 bis 49 Jahre",
    "dependent" = "5 bis 9 Jahre",
    "working" = "50 bis 54 Jahre",
    "working" = "55 bis 59 Jahre",
    "working" = "60 bis 64 Jahre",
    "dependent" = "65 bis 69 Jahre",
    "dependent" = "70 bis 74 Jahre",
    "dependent" = "75 bis 79 Jahre",
    "dependent" = "80 bis 84 Jahre",
    "dependent" = "85 bis 89 Jahre",
    "dependent" = "90 bis 94 Jahre",
    "dependent" = "95 bis 99 Jahre",
    "dependent" = "bis 4 Jahre"
  )


dep1 <- dep %>% 
  group_by(jahr, gemnr, typ, depend) %>% 
  summarise(n = sum(n)) %>% 
  pivot_wider(names_from = depend,
              values_from = n) %>% 
  ungroup() %>% 
  mutate(dep_ratio = (dependent/working) * 100)

ggplot(dep1, aes(x = as.factor(jahr), y = dep_ratio, group = jahr))+
  geom_boxplot()+
  facet_wrap(typ~.)
