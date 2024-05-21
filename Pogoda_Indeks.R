library(dplyr)
DanePogodowe <- read.csv("DanePogodowe.csv")
Kraje <- read.csv("DanePogodoweMini.csv")
Indeks <- read.csv("Indeks1.csv")
NazwyId <- read.csv("NazwyId.csv")
Kontynenty <- read.csv("Kontynenty.csv")

# dane pogodowe do indeksu szczęścia

# liczmy potrzebne wartości
# ilość słońca w godzinach
# montly_precipitate_sum = (sum(rain_sum..mm.) + sum(snowfall_sum..cm.))/10 dzielimy przez 10 bo średnia z 10 lat
dane_miesieczne <- DanePogodowe %>%
  mutate(year = strftime(time, "%Y"),
         month = strftime(time, "%m")) %>%
  inner_join(NazwyId, by = c("location_id")) %>%
  group_by(location_id, month, Country.name) %>% 
  summarise(mean_monthly_temp = mean(temperature_2m_mean...C.), monthly_min_temp = min(temperature_2m_min...C.),
            monthly_max_temp = max(temperature_2m_max...C.), monthly_amplitude = monthly_max_temp - monthly_min_temp,
            montly_precipitate_sum = (sum(rain_sum..mm.) + sum(snowfall_sum..cm.))/10,
            mean_monthly_dayligth_duration = mean(daylight_duration..s.)/3600,  mean_monthly_sunshine_duration = mean(sunshine_duration..s.)/3600) %>%
  as.data.frame()
colnames(dane_miesieczne)[3] <- "country_name"

# liczymy dane roczne
dane_roczne <- dane_miesieczne %>%
  group_by(location_id, country_name) %>%
  summarise(mean_temp_year = mean(mean_monthly_temp), warmest_month = max(mean_monthly_temp), coldest_month = min(mean_monthly_temp), maks_aplitude = max(monthly_amplitude), 
            max_max_temp = max(monthly_max_temp), min_min_temp = min(monthly_min_temp), year_precipitate_sum = sum(montly_precipitate_sum),
            year_mean_dayligth = mean(mean_monthly_dayligth_duration), year_mean_sunshine_duration = mean(mean_monthly_sunshine_duration)) %>% as.data.frame()

# łączymy dane roczne z indeksem szczęścia
pogodaIndeks <- dane_roczne %>%
  inner_join(Indeks[c("CountryName", "LadderScore")], by = c("country_name" ="CountryName"))

# dodajemy kontynenty
pogodaIndeks <- pogodaIndeks %>%
  inner_join(Kontynenty[c("Entity", "Countries.Continents")], by = c("country_name" = "Entity")) %>%
  rename(Continents = Countries.Continents)

write.csv(pogodaIndeks, "C:\\Users\\Kasia\\Desktop\\PDU\\PracaDomowa2\\aplikcja1\\pogodaIndeks")
# test czy średnie liczone po miesiącach są takie same jak po średnich rocznych
# test <- DanePogodowe %>%
#   mutate(year = strftime(time, "%Y"),
#          month = strftime(time, "%m")) %>%
#   inner_join(NazwyId, by = c("location_id")) %>%
#   group_by(location_id, Country.name) %>% 
#   summarise(mean_monthly_temp = mean(temperature_2m_mean...C.), monthly_min_temp = min(temperature_2m_min...C.),
#             monthly_max_temp = max(temperature_2m_max...C.), monthly_amplitude = monthly_max_temp - monthly_min_temp,
#             montly_precipitate_sum = (sum(rain_sum..mm., na.rm = TRUE) + sum(snowfall_sum..cm., na.rm = TRUE))/10,
#             mean_monthly_dayligth_duration = mean(daylight_duration..s.)/3600,  mean_monthly_sunshine_duration = mean(sunshine_duration..s.)/3600) %>%
#   as.data.frame()
# head(test)