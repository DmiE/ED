install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")

# libraries 
library(ggplot2)
library(dplyr)
library(lubridate)

raw <- read.csv("../athlete_events.csv")
summary(raw)

weight <- raw$Weight

# Waga sportowców danej dyscypliny
weight_filtered_raw <- raw %>% filter((Weight >= 35 & Age >= 18) | (Weight <= 160 & Age < 18)) #wywalamy osoby powyzej 18 wazace ponizej 35 i ponizej 18 wazace ponad 180kg 
waga_do_dyscypliny_raw <- data.frame(weight_filtered_raw$Sport, weight_filtered_raw$Weight, sex = weight_filtered_raw$Sex) # utworzenie zmiennej - Dyscyplina - Waga - PLec
waga_do_dyscypliny_bez_na <- waga_do_dyscypliny_raw %>% filter(!is.na(weight_filtered_raw.Weight)) # utworzenie zmiennej - Dyscyplina - Waga - Plec bez NA

summary(waga_do_dyscypliny_bez_na)

waga_do_dyscypliny_bez_na %>% group_by(sport = raw.Sport, sex)  %>% summarise(weight = mean(raw.Weight)) -> waga_do_dyscypliny # Srednia waga do dyscypliny (osobno K i M)

sorted <- waga_do_dyscypliny[order(waga_do_dyscypliny$weight), ] # waga_do_dyscypliny posortowana po wadze

ggplot(data = waga_do_dyscypliny, aes(x = sport, y = weight, fill = sex)) + geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Dla Mezczyzn

waga_do_dyscypliny_M <- waga_do_dyscypliny[(waga_do_dyscypliny$sex == "M"),]
waga_do_dyscypliny_M_sorted <- waga_do_dyscypliny_M[order(waga_do_dyscypliny_M$weight), ]

ggplot(data = waga_do_dyscypliny_M_sorted, aes(x = reorder(sport,-weight), y = weight)) + geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Dla Kobiet

waga_do_dyscypliny_F <- waga_do_dyscypliny[(waga_do_dyscypliny$sex == "F"),]
waga_do_dyscypliny_F_sorted <- waga_do_dyscypliny_F[order(waga_do_dyscypliny_F$weight), ]

ggplot(data = waga_do_dyscypliny_F_sorted, aes(x = reorder(sport,-weight), y = weight)) + geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Obliczenie BMI zawodnikow

BMI_Dyscyplina <- data.frame(Sport = raw$Sport, BMI = (raw$Weight/(raw$Height/100)^2), Sex = raw$Sex) # Dyscyplina - BMI - Plec
BMI_Dyscyplina_bez_NA <- BMI_Dyscyplina %>% filter(!is.na(BMI)) # Dyscyplina - BMI - Plec -- Bez NA
BMI_Dyscyplina_filtered <- BMI_Dyscyplina_bez_NA %>% filter(BMI>=12 & BMI<= 42) # Wartosci BMI ponizej 12(mega skrajna niedowaga) i powyzej 42(mega skrajna otylosc) usuniete

BMI_Dyscyplina_filtered %>% group_by(Sport, Sex)  %>% summarise(BMI_MEAN = mean(BMI)) -> Srednie_BMI_do_Sportu
ggplot(data = BMI_Dyscyplina_filtered, aes(x = Sport, y = BMI, fill = Sex)) + geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary(BMI_Dyscyplina_filtered)

# BMI DO ZDOBYTYCH MEDALI W DANEJ DYSCYPLINIE

BMI_Dyscyplina_Medal_Plec <- data.frame(Sport = raw$Sport, BMI = round((raw$Weight/(raw$Height/100)^2), digits = 1), Sex = raw$Sex, Medal = raw$Medal) # Dyscyplina - BMI - Plec - Medal
BMI_Dyscyplina_Medal_Plec_bez_NA <- BMI_Dyscyplina_Medal_Plec %>% filter((!is.na(BMI)) & (!is.na(Medal))) # Dyscyplina - BMI - Plec -- Bez NA

BMI_Dla_Sportu_i_Plci <- BMI_Dyscyplina_Medal_Plec_bez_NA[(BMI_Dyscyplina_Medal_Plec_bez_NA$Sex == "M") & (BMI_Dyscyplina_Medal_Plec_bez_NA$Sport == "Cycling"),]
# suma medali danego typu dla danego BMI

ggplot(data = BMI_Dla_Sportu_i_Plci, aes(x = BMI, y = Medal, fill = Medal)) + geom_bar(stat="identity", position=position_dodge()) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary(BMI_Dyscyplina_Medal_Plec_bez_NA)

