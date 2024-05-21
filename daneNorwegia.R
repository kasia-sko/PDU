# dane Norwegia białe noce
Norwegia <- read.csv("Norwegia.csv")
nazwy_norwegia <- read.csv("DaneNorw.csv")
polska_dlugosc_dnia <- read.csv("DlugoscPolski.csv")


norwegia <- Norwegia %>%
  inner_join(nazwy_norwegia[c("city", "location_id")], by = "location_id")
biale_noce <- norwegia[norwegia$daylight_duration..s. >= 66600,]

biale_noce1 <- biale_noce %>%
  group_by(city) %>%
  summarise(ilosc = n(), poczatek = min(time), koniec = max(time)) %>% as.data.frame()
biale_noce1
dni_polarne <- norwegia[norwegia$daylight_duration..s. == 86400,]
dni_polarne <- dni_polarne %>%
  group_by(city) %>%
  summarise(ilosc = n(), poczatek = min(time), koniec = max(time)) %>% as.data.frame()

noce_polarne <- norwegia[norwegia$daylight_duration..s. == 0,]
noce_polarne <- noce_polarne %>%
  group_by(city) %>%
  summarise(ilosc = n()) %>% as.data.frame()

srednie_dlugosci_dnia <- norwegia %>%
  group_by(city) %>%
  summarise(srednia_dl_dnia = mean(daylight_duration..s.)/3600)%>% as.data.frame()



# wykresy

# wykres długości dnia w Polsce, Svalbardzie, Oslo, Lofoty
svalbard_dl_dnia <- norwegia[norwegia$city == "Svalbard", "daylight_duration..s."]
olso_dl_dnia <- norwegia[norwegia$city == "Oslo", "daylight_duration..s."]
lofoten_dl_dnia <- norwegia[norwegia$city == "Lofoten", "daylight_duration..s."]


plot.new()
matplot(1:365, cbind(polska_dlugosc_dnia$daylight_duration..s., svalbard_dl_dnia, olso_dl_dnia, lofoten_dl_dnia), type="l", col=c("#B43757", "#29AB87", "#6AAFc9", "#FFCC66"), lty=1,
        xlab="Miesiąc", ylab="Długość dnia [s]", xaxt="n", lwd = 2)
months <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
month_midpoints <- cumsum(months) - months / 2
axis(1, at=month_midpoints, labels=month.abb, line=-0.25, lwd=0, lwd.ticks = 1)
legend("bottom", c("Polska", "Svalbard", "Olso", "Lofoty"),
       lty=1, col=c("#B43757","#29AB87", "#6AAFc9", "#FFCC66"), lwd = 2, bg="#F9F6D2")

# równonoc!!!!

# wykres ilości nocy i dni polarnych
d_polarne <- dni_polarne$ilosc
names(d_polarne) <- dni_polarne$city
plot.new()
barplot(d_polarne, col = "#FAB9FA", ylim = c(0,max(d_polarne)*1.2))
n_polarne <- noce_polarne$ilosc
n_polarne <- c("Bodo" = 0, n_polarne)
names(n_polarne)[c(2:6)] <- noce_polarne$city
barplot(n_polarne, col = adjustcolor("#B779B7", alpha.f = 0.5), add = TRUE)
legend("topright", c("dni polarne", "noce polarne"), pch = 16, pt.cex = 2, col=c("#FAB9FA","#B779B7"), bg="#F9F6D2")
# Dodanie wartości na słupkach 
text(x = barplot(d_polarne, plot=FALSE), y = d_polarne, label = d_polarne, pos = 3, cex = 0.8, col = "black")
text(x = barplot(n_polarne, plot=FALSE), y = n_polarne, label = n_polarne, pos = 3, cex = 0.8, col = "black")                   
