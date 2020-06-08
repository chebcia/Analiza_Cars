library(ggplot2)
library(visreg)
library(knitr)
library(corrplot)
library(ggcorrplot)
library(dplyr)
library(knitr)
library(readr)
library(xlsx)
library(ggdendro)
library(tidyr)

#odczyt danych
Autko <- read_csv("Autko.csv")
## Przygotowanie danych do dalszej analizy
#Zamiana nazwy kolumn na odpowiednie nazwy
names(Autko)[1] <- "mpg"
names(Autko)[2] <- "cylinders"
names(Autko)[3] <- "displacement"
names(Autko)[4] <- "horsepower"
names(Autko)[5] <- "weight"
names(Autko)[6] <- "acceleration"
names(Autko)[7] <- "model_year"
names(Autko)[8] <- "origin"
names(Autko)[9] <- "car_name"

#Zmienienie na odpowiednie typy danych
Autko$car_name<-as.character(Autko$car_name)
Autko$cylinders = Autko$cylinders %>% factor(labels = sort(unique(Autko$cylinders)))
Autko$model_year = Autko$model_year %>% factor(labels = sort(unique(Autko$model_year)))
Autko$origin = Autko$origin %>% factor(labels = sort(unique(Autko$origin)))
Autko$horsepower<-as.numeric(Autko$horsepower)
#Oddzielenie marki i modelu 
Autko <- separate(Autko, col = c("car_name"), into = c("brand", "model"), sep = " ", extra = "merge")

#Zmamiana "?" na wartosci N/A
Autko[Autko == "?"] <- NA
#Zast?powanie warto?ci NULL ?redni? 
Autko$horsepower <- ifelse(is.na(Autko$horsepower), mean(Autko$horsepower, na.rm=TRUE), Autko$horsepower)

#Zamiana brand?w na odpowiednie nazwy
Autko$brand[Autko$brand == "chevroelt"] <- "chevrolet"
Autko$brand[Autko$brand == "maxda"] <- "mazda"
Autko$brand[Autko$brand == "vokswagen"] <- "volkswagen"
Autko$brand[Autko$brand == "toyouta"] <- "toyota"
Autko$brand[Autko$brand == "vw"] <- "volkswagen"
Autko$brand[Autko$brand == "mercedes-benz"] <- "mercedes"
#Zamiana na factory
Autko$brand = Autko$brand %>%
  factor(labels = sort(unique(Autko$brand)))


#////////////////////////////////////////////////////////////////////////////////////////////////////////

library(e1071)

funkcja_dominanta <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

funkcja_pomiary_statystyczne <- function(wektor){
  srednia_arytm <- mean(wektor)
  mediana <- median(wektor)
  dominanta <- funkcja_dominanta(wektor)
  kwartyl1 <- as.numeric(quantile(wektor, probs = 0.25))
  kwartyl3 <- as.numeric(quantile(wektor, probs = 0.75))
  wariancja <- var(wektor)
  wariancja_Obc <- wariancja * (length(wektor) - 1)/length(wektor)
  odchyl_stand <- sqrt(wariancja)
  odchyl_stand_Obc <- sqrt (wariancja_Obc)
  odchyl_przec <- sum(abs(wektor - srednia_arytm))/length(wektor)
  odchyl_cwiart <- (kwartyl3 - kwartyl1)/2
  klas_wspol_zmienn <- odchyl_stand/srednia_arytm
  pozyc_wspol_zmienn <- odchyl_cwiart/mediana
  kurtoza <- kurtosis(wektor)
  eksces <- kurtoza - 3
  wsk_asymetrii <- kwartyl3 - (2 * mediana) + kwartyl1
  wspol_asymertii <- wsk_asymetrii / (2 * odchyl_cwiart)
  dol_typ_obsz_zmienn <- srednia_arytm - odchyl_stand
  gor_typ_obsz_zmienn <- srednia_arytm + odchyl_stand
  
  rezultat <- c(srednia_arytm, mediana, dominanta, kwartyl1, kwartyl3, wariancja,
                wariancja_Obc, odchyl_stand, odchyl_stand_Obc, odchyl_przec,
                odchyl_cwiart, klas_wspol_zmienn, pozyc_wspol_zmienn, kurtoza,
                eksces, wsk_asymetrii, wspol_asymertii, dol_typ_obsz_zmienn,
                gor_typ_obsz_zmienn)
  
  return(rezultat)
}

mpg_ <- round( funkcja_pomiary_statystyczne(Autko$mpg), digits = 4)
acceleration_ <- round(funkcja_pomiary_statystyczne(Autko$acceleration), digits = 4)
displacement_ <- round(funkcja_pomiary_statystyczne(Autko$displacement), digits = 4)
horsepower_ <-  round(funkcja_pomiary_statystyczne(Autko$horsepower), digits = 4)
weight_ <- round(funkcja_pomiary_statystyczne(Autko$weight), digits = 4)

tmp <- data.frame(mpg_, acceleration_,displacement_, horsepower_, weight_)
row.names(tmp) <- c("srednia arytmetyczna: ", "mediana: ", "dominanta: ", "kwartyl 0.25: ", "kwartyl 0.75: ",
                    "wariancja: ", "wariancja obciazona: ", "odchylenie standardowe: ",
                    "odchylenie standardowe obciazone: ", "odchylenie przecietne: ",
                    "odchylenie cwiartkowe: ", "klasyczny wspolczynnik zmiennosci: ",
                    "pozycyjny wspolczynnik zmiennosci: ", "kurtoza: ", "eksces: ",
                    "wskaznik asymetrii: ", "wspolczynnik asymetrii: ",
                    "dolna granica typowego obszaru zmiennosci: ",
                    "gorna granica typowego obszaru zmiennosci: ")

print(tmp)

#///////////////////////////////////////////////////////////////////////////////////////////////////////

wynik <- function(x)
{
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  zakres <- as.numeric(max - min)
  ilosc <- as.numeric(nrow(Autko))
  pierwiastek <- sqrt(ilosc)
  pierwiastek <- ceiling(pierwiastek)
  szer <- zakres / pierwiastek
  szer <-szer[1]
  pkt = seq(min, max, by = szer)
  rezultat <- c(min, max, szer, pkt)
  return(rezultat)
}
#dane zapisane w wektorze
mpg<-wynik(Autko$mpg)
dis<-wynik(Autko$displacement)
kg<-wynik(Autko$weight)
pow<-wynik(Autko$horsepower)
acc<-wynik(Autko$acceleration)
#przedzia?y
przedzialmpg <- cut(Autko$mpg, mpg[-c(1,2,3)], right = FALSE, include.lowest = TRUE)
przedzialdis <- cut(Autko$displacement, dis[-c(1,2,3)], right = FALSE, include.lowest = TRUE)
przedzialpow <- cut(Autko$horsepower, pow[-c(1,2,3)], right = FALSE, include.lowest = TRUE)
przedzialkg <- cut(Autko$weight, kg[-c(1,2,3)], right = FALSE, include.lowest = TRUE)
przedzialacc <- cut(Autko$acceleration, acc[-c(1,2,3)], right = FALSE, include.lowest = TRUE)
# szeregi rozdzielcze
szeregmpg <- table(przedzialmpg)
szeregdis <- table(przedzialdis)
szeregpow <- table(przedzialpow)
szeregkg <- table(przedzialkg)
szeregacc <- table(przedzialacc)
#histogramy
ggplot(Autko, aes(x=mpg)) + geom_histogram(breaks =  mpg[-c(1,2,3)] , aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram mpg")
ggplot(Autko, aes(x=displacement)) + geom_histogram(breaks = dis[-c(1,2,3)], aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram displacement")
ggplot(Autko, aes(x=horsepower)) +  geom_histogram(breaks = pow[-c(1,2,3)], aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram horsepower")
ggplot(Autko, aes(x=weight)) + geom_histogram(breaks = kg[-c(1,2,3)], aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram weight")
ggplot(Autko, aes(x=acceleration)) + geom_histogram(breaks = acc[-c(1,2,3)], aes(y=..density..),  colour="black", fill="white") + geom_density(alpha=.2, fill="blue") + labs(title = "Histogram acceleration")


barplot(table(Autko$cylinders), main = "Cylindry", xlab = "Ilość cylindrów", col = "sky blue")
barplot(table(Autko$model_year), main = "Rok produkcji", xlab = "Rok", col = "sky blue")
barplot(table(Autko$origin), main = "Pochodzenie", xlab = "Numer", col = "sky blue")


####################################################################################
  

ggplot(Autko,  aes(x=model_year, y=mpg, fill=model_year)) + 
  geom_boxplot()  +
  xlab("Rok") + ylab("MPG")+
   theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +
  theme(plot.title = element_text(size = 15,  face= 'bold' )) +
  labs(title = "Boxplot Rok i MPG") + scale_y_continuous(breaks =  seq(0,50, by=5)) + 
  scale_fill_discrete(name = "", labels = c("Rok 1970", "Rok 1971",  "Rok 1972", "Rok 1973", "Rok 1974",
                                            "Rok 1975", "Rok 1976", "Rok 1977", "Rok 1978", "Rok1979",
                                            "Rok 1980", "Rok 1981", "Rok 1982"))+ stat_summary(fun.y=mean, geom="point", shape=20, size=8, color="red", fill="red") 



Autko%>%ggplot(aes(x=origin,y=mpg))+geom_boxplot()+geom_jitter(aes(color=cylinders),width = 0.1)+ 
  scale_x_discrete(labels = c("Origin 1", "Origin 2","Origin 3"))+
  scale_color_brewer(name="Cylindry",type = "qual", palette=3)+scale_fill_manual() + 
  xlab("") + ylab("MPG")+
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +
  theme(plot.title = element_text(size = 15,  face= 'bold' ))+ labs(title = "Boxplot Origin i MPG z zaznaczonymi cylindrami")


ggplot(Autko, aes(cylinders,weight,fill=cylinders)) +
  geom_boxplot()  +   theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +  xlab("Cylindry") + 
  ylab("Waga") + labs(title = "Boxplot Cylindrów i Wagi") +
 stat_summary(fun.y=mean, geom="point", shape=20, size=8, color="red", fill="red") 




Autko%>%ggplot(aes(x=origin,y=weight))+geom_boxplot()+geom_jitter(aes(color=cylinders),width = 0.1)+ 
  scale_x_discrete(labels = c("Origin 1", "Origin 2","Origin 3"))+
  scale_color_brewer(name="Cylindry",type = "qual", palette=3)+scale_fill_manual() + 
  xlab("") + ylab("Waga")+
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +
  theme(plot.title = element_text(size = 15,  face= 'bold' ))+ labs(title = "Boxplot Origin i Weight z zaznaczonymi cylindrami")


#////////////////////////////////////////////////////////////////////////////////////////////////////////

# PRZEDZIAŁY UFNOŚCI
przedzialy <- function(x)
{
  srednia = mean(x, na.rm =  TRUE)
  sd <- sd(x, na.rm = TRUE)
  przedz90norm  <- round(srednia+c(-1, 1)*sd/sqrt(398)*qnorm(.95), 2)
  przedz95norm <- round(srednia+c(-1, 1)*sd/sqrt(398)*qnorm(.975), 2)
  przedz99norm <- round(srednia+c(-1, 1)*sd/sqrt(398)*qnorm(.995), 2)
  przedz90war <- round(sqrt(sd*398/qchisq(c(1-.05,.05), 397)), 2)
  przedz95war <- round(sqrt(sd*398/qchisq(c(1-.025,.025), 397)), 2)
  przedz99war <- round(sqrt(sd*398/qchisq(c(1-.005,.005), 397)), 2)
  przedz90t <- round(srednia+c(-1, 1)*sd/sqrt(398)*qt(.95, 397), 2)
  przedz95t <- round(srednia+c(-1, 1)*sd/sqrt(398)*qt(.975, 397), 2)
  przedz99t <- round(srednia+c(-1, 1)*sd/sqrt(398)*qt(.995, 397), 2)
  
  rezultat <- c(przedz90norm,przedz95norm, przedz99norm,  przedz90war,przedz95war,
                przedz99war, przedz90t, przedz95t, przedz99t)
  
  return(rezultat)
}
# Przypisanie odpowiednim zmiennym rezultatów wykonania funkcji
accp <- przedzialy(Autko$acceleration)
mpgp <- przedzialy(Autko$mpg)
disp <- przedzialy(Autko$displacement)
horsep <- przedzialy(Autko$horsepower)
kgp <- przedzialy(Autko$weight)
# Umieszczenie wcześniej utworzonych zmiennych w tabeli
przedzialy <- data.frame(mpgp, accp , disp, horsep, kgp)
# Odpowiednie nazwanie tabeli z wyznaczonymi przedziałami
row.names(przedzialy) <- c("początek przedziału dla ufności 90% dla wartości oczekiwanej"  , "koniec  przedziału dla ufności 90% dla wartości oczekiwanej",
                           "początek przedziału dla ufności 95% dla wartości oczekiwanej"  , "koniec  przedziału dla ufności 95% dla wartości oczekiwanej",
                           "początek przedziału dla ufności 99% dla wartości oczekiwanej"  , "koniec  przedziału dla ufności 99% dla wartości oczekiwanej",
                           "początek przedziału dla ufności 90% dla wariancji"  , "koniec  przedziału dla ufności 90% dla wariancji",
                           "początek przedziału dla ufności 95% dla wariancji"  , "koniec  przedziału dla ufności 95% dla wariancji",
                           "początek przedziału dla ufności 99% dla wariancji"  , "koniec  przedziału dla ufności 99% dla wariancji",
                           "początek przedziału dla ufności 90% dla rozkładu t"  , "koniec  przedziału dla ufności 90% dla rozkładu t",
                           "początek przedziału dla ufności 95% dla rozkładu t"  , "koniec  przedziału dla ufności 95% dla rozkładu t",
                           "początek przedziału dla ufności 99% dla rozkładu t"  , "koniec  przedziału dla ufności 99% dla rozkładu t")
# Otrzymane przedziały
kable(przedzialy, caption = "Wyznaczone przedziały ufności")

# NORMALNOŚĆ
normalnosc_rozkladu <- function(x)
{
  sh_test <- shapiro.test(x)
  sh_result <- sh_test$p.value
  return(sh_result)
}

accn <- normalnosc_rozkladu(Autko$acceleration)
mpgn <- normalnosc_rozkladu(Autko$mpg)
disn <- normalnosc_rozkladu(Autko$displacement)
horsen <- normalnosc_rozkladu(Autko$horsepower)
kgn <- normalnosc_rozkladu(Autko$weight)

normalnosc <- data.frame(accn, mpgn, disn, horsen, kgn)
row.names(normalnosc) <- c("P-value")
colnames(normalnosc) <- c("Acceleration", "MPG", "Displacement", "Horsepower", "Weight")

# HIPOTEZY
dwie_srednie <- function(x, y)
{
  ds.test <- t.test(x, y, paired = F)
  ds.p <- ds.test$p.value
  ds.przedzial95 <- round(ds.test$conf.int, 2)
  ds_list <- list("p_value" = ds.p, "przedzial95" = ds.przedzial95)
  return(ds_list)
}
# przyspieszenie vs. waga 3000
accel_3000 <- dwie_srednie(Autko$acceleration[Autko$weight > 3000], Autko$acceleration[Autko$weight <= 3000])
accel_3000$p_value
accel_3000$przedzial95

# moc vs. pojemność 250
horsepower_disp250 <- dwie_srednie(Autko$horsepower[Autko$displacement > 250], Autko$horsepower[Autko$displacement <= 250])
horsepower_disp250$p_value
horsepower_disp250$przedzial95

# spalanie vs cylindry
mpg_8cyl_6cyl <- dwie_srednie(Autko$mpg[Autko$cylinders == 8], Autko$mpg[Autko$cylinders == 6])
mpg_8cyl_6cyl$p_value
mpg_8cyl_6cyl$przedzial95

# przyspieszenie am vs jap
accel_am_jap <- dwie_srednie(Autko$acceleration[Autko$origin == 1], Autko$acceleration[Autko$origin == 3])
accel_am_jap$p_value
accel_am_jap$przedzial95

# przyspieszenie am vs eu
accel_am_eu <- dwie_srednie(Autko$acceleration[Autko$origin == 1], Autko$acceleration[Autko$origin == 2])
accel_am_eu$p_value
accel_am_eu$przedzial95

# kraj pochodzenia vs spalanie ANOVA
accelerationOriModel.0 <- lm(acceleration~1, data = Autko)
accelerationOriModel.1 <- lm(acceleration~origin, data = Autko)
anova(accelerationOriModel.0, accelerationOriModel.1)

# cylindry vs spalanie ANOVA
mpgModel.0 <- lm(mpg~1, data = Autko)
mpgModel.1 <- lm(mpg~cylinders, data = Autko)
anova(mpgModel.0, mpgModel.1)

# cylindry vs przyspieszenie ANOVA
accelerationCylModel.0 <- lm(acceleration~1, data = Autko)
accelerationCylModel.1 <- lm(acceleration~cylinders, data = Autko)
anova(accelerationCylModel.0, accelerationCylModel.1)



## Cumulative distribution function plot
## Wykres funkcji rozk?adu skumulowanego

#Procent skumulowany - to statystyczna miara, okre?laj?ca jaki odsetek "os?b",
#w tym przypadku samochod?w, uzyska? pewien zakres wynik?w.
#Jak sama nazwa wskazuje jest to procent z?o?ony z dodawania procent?w dla pojedy?czych kategorii - nast?puje kumulacja.

#Dzi?ki temu w ?atwy spos?b (bez dodawania) okre?li? odsetek(procent/prawdopodobie?stwo) samochod?w przyjmuj?cych pewien zakres, <br/>licz?c od pocz?tku do danej warto?ci.

#Aby uzyska? taki wykres nale?y: uzyska? dane i obliczy? kluczowe statystyki podsomowuj?ce,
#wyodr?bni? wektor danych "mpg" dla Auto-Mpg,

mpg = Autko$mpg

#obliczy? liczb? nie brakuj?cych warto?ci w "mpg"
n = sum(!is.na(mpg));
#uzyska? empiryczne warto?ci CDF
mpg.ecdf = ecdf(mpg)

#Teraz mo?emy wykre?li? empiryczn? funkcj? rozk??du skumulowanego (za pomoc? ecdf() i plot()):
plot(mpg.ecdf, xlab = 'MPG - Miles Per Gallon', ylab = 'Prawdopodobienstwo', main = 'Empiryczny rozk?ad skumulowany\nMPG samochod?w')

#Zatem samochod?W spalaj?cych 20 galon?w na mil? i mniej jest oko?o 38%.
#Samochod?w spalaj?cych 30 galon?w na mil? jest 80%.

#Dla ostatniej mo?liwej kategorii/opcji procent zawsze b?dzie wynosi? 100%,
#gdy? "wyczerpuje" on wszystkie pozosta?e kategorie/opcje.

## QQ plots
## Wykresy QQ
  
#Wykres QQ (kwantylowo-kwantylowy) ukazuje nam korelacj? pomi?dzy dan? pr?bk?,
#a rozk?adem normalnym.<br/>Rysowana jest r?wnie? 45 stopniowa linia odniesienia.
#Wykresy QQ to narz?dzie graficzne pomagaj?ce nam oceni?, czy zbi?r danych pochodzi z jakiego? teoretycznego rozk?adu (normalny, wyk?adniczy).
#Jest to kontrola wizualna, a nie hermetyczny dow?d, wi?c jest ona subiektywna.


#Aby uzyska? taki wykres nale?y: utworzy? normalny wykres zmiennej -> qqnorm(),
#doda? lini? odniesiena -> qqline(),


#Teraz mo?emy utworzy? wykres QQ:

##### - dla MPG,

qqnorm(Autko$mpg, pch=1, frame=FALSE, main="QQ plot - MPG")
qqline(Autko$mpg, col='red', lwd=2)

##### - dla Displacement,

qqnorm(Autko$displacement, pch=1, frame=FALSE, main="QQ plot - Displacement")
qqline(Autko$displacement, col='red', lwd=2)

##### - dla Horsepower,

qqnorm(Autko$horsepower, pch=1, frame=FALSE, main="QQ plot - Horsepower")
qqline(Autko$horsepower, col='red', lwd=2)

##### - dla Weight,

qqnorm(Autko$weight, pch=1, frame=FALSE, main="QQ plot - Weight")
qqline(Autko$weight, col='red', lwd=2)

##### - dla Acceleration.

qqnorm(Autko$acceleration, pch=1, frame=FALSE, main="QQ plot - Acceleration")
qqline(Autko$acceleration, col='red', lwd=2)


#Je?li wszystkie punkty opadaj? w przybli?eniu wzd?u? linii odniesienia
#to mo?emy za?o?y? normalno??.
#Jesli natomiast punkty tworz? krzyw? zamiast lini? prost? to w tym momencie mamy do czynienia z wypaczeniem danych pr?bki.


## Scatterplot matrix (by class)
## Wykres macierzy rozrzutu (wed?ug klasy)

#Macierz rozrzutu umo?liwia nam zwizualizowanie korelacji ma?ych zestaw?w danych.
#Wykres macierzy rozrzutu pokazuje nam wszystkie pary wykres?w rozrzutu zmiennych w jednym widoku w formacie macierzy.


#Aby uzyska? wykres macierzy rozrzutu nale?y:
# utworzy? podstawowy wykres za pomoc? -> pairs(),
pairs(Autko[c(1,3,4,5,6)], pch = 19)
#mo?na usun?? doln? cz??? wykresu,
pairs(Autko[c(1,3,4,5,6)], pch = 19, lower.panel=NULL)
#mo?na pokolorowa? punkty poprzez poszczeg?ln? klas? (np. origin) w celu lepszego zobrazowania korelacji,

my_cols <- c("#FF0000", "#00FF00", "#0000FF")  
pairs(Autko[c(1,3,4,5,6)], pch = 19,  cex = 0.5,
      col = my_cols[Autko$origin],
      lower.panel=NULL)


#///////////////////////////////////////////////////////////////////////////////////////////////////////



#////////////////////////////////////////////////////////////////////////////////////////////////

#Losowanie ziarna

set.seed(100)
#wybieranie nowego testu i trainingu dla wi?kszej liczebno?ci danych
indexes <- sample(nrow(Autko), (0.7*nrow(Autko)), replace = FALSE)
trainData <- Autko[indexes, ]
testData <- Autko[-indexes, ]


#regresja liniowa mpg i weight
ggplot(data= Autko,aes(weight,mpg)) + geom_point()+ geom_smooth(method=lm) 
model <- lm(weight~mpg, data= Autko)
summary(model)


#regresja liniowa  mpg i displacement
ggplot(Autko,aes(displacement,mpg)) +geom_point()+geom_smooth(method=lm) 
model <- lm(mpg~displacement, data= Autko)
summary(model)


#regresja liniowa  weight i horsepower
ggplot(Autko,aes(weight, horsepower)) + geom_point() +geom_smooth(method = lm)
model <- lm(weight~horsepower, data= Autko)
summary(model)



#Budowanie nowych danych do stworzenia korelacji, wybranie tylko kolumn numerycznych
newdata <- cor(Autko[ , c('mpg','weight', 'displacement', 'horsepower', 'acceleration')], use='complete')
corrplot(newdata, method = "number")

# Wykres korelacji
ggcorrplot(newdata, hc.order = TRUE, 
           type = "upper", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Korelacja", 
           ggtheme=theme_bw)

#Wniosek: mpg z wszystkimi innymi kolumnami koreluje na minusie, posiada te? wysok? korelacj?. 
#Najwi?ksza korelacj? ma displacement i weight r?wn? 0.93.

#Tworzenie regresji liniowej dla ca?ego zbioru z wieloma predykatorami
model <- lm(mpg~weight+horsepower+origin+model_year+displacement+acceleration,data = Autko)
summary(model)

#Jak wida? acceleration i horsepower s? statystycznie nieistotne
#Wykresy dla modelu
plot(model)

#Obliczenie b??du RMS
predictions <- predict(model, newdata = testData)
sqrt(mean((predictions - testData$mpg)^2))

#Wykonujemy regresje liniow? dla trainingowego zbioru
regresja <- lm( mpg ~ cylinders + displacement + horsepower + weight  + acceleration + origin, data = trainData)
summary(regresja)
plot(regresja)

#displacement i acceleration s? nieistotne 
#pon?wny wi?c regresje
regresja2<- lm(formula = mpg ~ cylinders + horsepower + weight, data = trainData)

summary(regresja2)
plot(regresja2)
#cylinder 6 jest statystycznie nieistotne, ale nale?y zostawi? t? zmienn? z powodu na inne cylindry

predykcja <- predict(regresja2, newdata = testData)

#tworzymy ramk? danych dla zobaczenia warto?ci

wynik <- data.frame(model_year = testData$model_year,  prediction = predykcja,  actual = testData$mpg)
roznicaproc <- abs(wynik$prediction - wynik$actual) / 
  wynik$actual * 100
wynik$roznicaproc <- roznicaproc
remove(roznicaproc)
paste("Procent roznicy:", round(mean(wynik$roznicaproc)))

wynik$prediction <- round(wynik$prediction, 2)
wynik$roznicaproc <- round(wynik$roznicaproc, 2)
print(wynik)

#14 procent r?znicy to nie jest dobry wynik lepszym rozwi?zaniem bedzie drzewo decyzyjne

regresTREE <- rpart(formula = mpg ~ ., data = testData)
dpred <- predict(regresTREE , data = testData)

plot(regresTREE, uniform=TRUE, main="Drzewo decyzyjne")
text(regresTREE, use.n=TRUE, all=TRUE)

wynik2 <- data.frame(model_year = testData$model_year, 
                     prediction = dpred, 
                     actual = testData$mpg)
roznicaproc2 <- abs(wynik2$prediction - wynik2$actual) / 
  wynik2$actual * 100
wynik2$roznicaproc2 <- roznicaproc2
remove(roznicaproc2)
paste("Procent roznicy:", round(mean(wynik2$roznicaproc2)))
#aktualnie b??d jest r?wny tylko 6 %


#ponowne losowanie ziarna
set.seed(100)
#wLosowanie zbioru treningowego i testowego 
indexes <- sample(nrow(Autko), (0.9*nrow(Autko)), replace = FALSE)
trainData <- Autko[indexes, ]
testData <- Autko[-indexes, ]

theme_set(theme_bw())


#dist() u?ywamy aby obliczy? dystans pomi?dzy pr?bk?
#hclust() prezentuje hierachiczne klastry
# plot() funkcja kt?ra obrazuje dane za pomoc? drzewa


#Dendrogram

Autko2<- testData[,-c(2,7,8,9,10)]
mean_data <- apply(Autko2,2,mean)
std<- apply(Autko2, 2,sd)
#mo?na to robi? manualnie  (x - mean(x)) / sd(x) ale lepsze jest scaling
Autko2<-scale(Autko2, mean_data, std)

distance <- dist(Autko2)
distance
hc<-hclust(distance)
plot(hc, labels = testData$origin)
#zak?adamy 3 klastry, poniewa? s? trzy warto?ci
groups <- cutree(hc, k=3)
#tworzenie border?w
rect.hclust(hc, k=3, border="red")


#Metoda k-?rednich jest metod? nale?ac? do grupy algorytm?w analizy skupie? tj. 
#analizy polegaj?cej na szukaniu i wyodr?bnianiu grup obiekt?w podobnych (skupie?). 
#k - means

ggplot(testData, aes(x= horsepower, y= displacement, color = origin)) + geom_point()

testData<-testData %>% select(mpg,displacement, horsepower, weight, acceleration, model_year, origin) 
#wiemy, ?e maj? by? 3 klastry 
k.cluster <- kmeans(testData[,c(1:6)],3, nstart = 20)
print(k.cluster$centers)

table(testData$origin, k.cluster$cluster)
# jak wida? pierwszy klaster jest dobrze dopasowany, w drugim ju? jest noise chocia? jest zdecydowanie lepszy od trzeciego
library(cluster) 
clusplot(testData, k.cluster$cluster, color=TRUE, shade=TRUE, labels=0,lines=0)
#pokrycie wynosi 81,13% nie jest to te? najlepsza warto?? 


#////////////////////////////////////////////////////////////////////////////////////////////////////

#Wykresy dla lepszej analizy zbioru

#wyliczenie najpopularniejszych marek 
namesOccurence <- Autko %>% group_by(brand) %>% tally() %>% rename(Number_of_Occurences = n)

#Wykres najpopularniejszych samochod?w (top 20)
namesOccurence  %>% top_n(20, Number_of_Occurences) %>% arrange(desc(Number_of_Occurences)) %>% ggplot(aes(x=brand, y=Number_of_Occurences)) +
  geom_bar(stat='identity') +
  coord_flip() +  
  ggtitle("Najpopularniejsze samochody")+
  xlab("Marki samochodow")+
  ylab("Ilosc") + 
  theme_test() +
  theme(plot.title = element_text(size = 15,  face= 'bold', margin = ))+
  theme(legend.title =element_text(size = 40, face= 'bold'), legend.position = "bottom")+
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) 
# Najpopularniejszymi markami jest Ford i Chevrolet 

#Przedstawienie na wykresie ko?owym
#wyliczenie procent?w
namesOccurence$procent <- round(namesOccurence$Number_of_Occurences / sum(namesOccurence$Number_of_Occurences), digits = 2)

#wykres ko?owy
pie <- namesOccurence %>% filter(procent>0) %>%
  ggplot(aes(x = "", y=procent ,fill = factor(brand))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="marki", 
       x=NULL, 
       y=NULL, 
       title="Wykres kolowy dla najczesciej wystepowanych marek samochodowych")

pie + coord_polar(theta = "y", start=0) +  geom_text(aes(x = 1.3, label = procent), position = position_stack(vjust = 0.5), size=2) 


#Wykres MPG dla ka?dej marki 
Autko %>% group_by(brand) %>% 
  summarise(sredniam = mean(mpg, na.rm = TRUE))  %>%
  ggplot(aes(x=brand, y=sredniam))+geom_bar(stat='identity')  + coord_flip()+
  xlab("Marki samochodow") +ylab("Srednia mpg")+
  ggtitle("Srednia MPG")+
  theme(plot.title = element_text(size = 15,  face= 'bold', margin = ))+
  theme(legend.title =element_text(size = 40, face= 'bold'), legend.position = "bottom")+
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold"))

#wyliczenie ?redniej z ilo?ci
df<-Autko%>% group_by(model_year) %>%summarise(ilosc = n()) %>% mutate(srednia = mean(ilosc))


#Wykres ilo?ci aut w danym roku, przerywan? lini? zaznaczono ?redni? ilo?? aut
Autko%>% group_by(model_year) %>%summarise(ilosc = n()) %>% ggplot(aes(x=model_year,y= ilosc ), lty=5)+
  geom_line(group=1)+
  geom_point(size=2)+
  theme_bw()+
  theme(legend.position = "none")+  xlab("Rok") +ylab("Ilosc")+
  geom_hline(yintercept = df$srednia, linetype="dotted", color="red", size=2)+
  annotate(geom="text", x= 10,y=sredniak-0.5,
           label="srednia ilo?? aut",color= "black", size=4)+
  ggtitle("Ilosc aut w danym roku")+
  geom_text(label="") +
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +
  theme(plot.title = element_text(size = 15,  face= 'bold' ))

#Wykres ?rednich horsepower i displacement w danym roku
Autko %>% group_by(model_year)%>% summarise(srednia = mean(horsepower), srednia2 = mean(displacement)) %>%
  ggplot(aes(x=model_year))+
  geom_line(aes(x = model_year, y=srednia, group = 1),lty=2, size=0.8)+
  geom_line(aes(x= model_year, y=srednia2, group = 1),size=1.2)+
  scale_color_gradient(low = 'blue', high = 'red')+
  theme_bw()+
  annotate(geom="text", x=6,y=230,
           label="displacement",color= "black", size=4 )+
  annotate(geom="text", x=3,y=150,
           label="horsepower",color= "black", size=4 ) +
  ggtitle("srednia dla horsepower i displacement w danym roku")+
  theme(legend.position = "none")+
  labs(x="Rok",y="") +
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +
  theme(plot.title = element_text(size = 15,  face= 'bold' ))


#DODANIE NOWEJ KOLUMNY MPGOPT - OKRESLAJACEJ OCHYLENIE OD SREDNIEJ MPG
Autko$mpgopt<- round((Autko$mpg - mean(Autko$mpg))/sd(Autko$mpg), 2)
Autko$typ <- ifelse(Autko$mpgopt < 0, "pod", "nad")
#wykres dla powy?ej i poni?ej ?redniej mpg
Autko %>%group_by(brand) %>% ggplot(aes(x=brand, y=mpgopt, label=mpgopt)) + 
  geom_bar(stat='identity', aes(fill=typ), width=.5)  +
  scale_fill_manual(name="Wed?ug mpg", 
                    labels = c("Powyzej sredniej", "Ponizej sredniej"), 
                    values = c("nad"="#00ba38", "pod"="#f8766d")) + 
  labs( title= "Srednie mpg dla danych marek") + 
  coord_flip() + 
  labs(x="Marka",y="srednia mpg")








