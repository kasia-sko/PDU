migracje <- read.csv("migracje.csv", fileEncoding = "Windows-1250")
migracje
nazwyKrajow <- read.csv("countries.csv")
head(nazwyKrajow,20)
nazwyKrajow$pl
srednie_migracje <- migracje %>%
  group_by(Nazwa) %>%
  summarise(srednia_emigracja = mean(c(EMI16, EMI17, EMI18, EMI19,EMI20, EMI21, EMI22), na.rm = TRUE), 
            srednie_saldo = mean(c(SALDO16,SALDO17, SALDO18, SALDO19, SALDO20, SALDO21, SALDO22), na.rm = TRUE))%>% as.data.frame()
srednie_migracje
migracje_pogodowe <- srednie_migracje %>%
  inner_join(pogodaIndeks, by = c("Nazwa" = "country_name"))
head(migracje_pogodowe)


