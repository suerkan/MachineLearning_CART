library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)

#Verinin import edilmesi
library(readxl)
Odev <- read_excel("......")
View(Odev)
summary(Odev)

#Verilen kategorik olarak tanımlanması
Odev$maas=as.factor(Odev$maas)
Odev$vadeli=as.factor(Odev$vadeli)
Odev$yatirim=as.factor(Odev$yatirim)
Odev$kart=as.factor(Odev$kart)
Odev$kredi=as.factor(Odev$kredi)
Odev$bes=as.factor(Odev$bes)
Odev$sigorta=as.factor(Odev$sigorta)
Odev$sube=as.factor(Odev$sube)
Odev$atm=as.factor(Odev$atm)
Odev$intmob=as.factor(Odev$intmob)
Odev$cagmerk=as.factor(Odev$cagmerk)
Odev$yil=as.factor(Odev$yil)
Odev$yas=as.factor(Odev$yas)
Odev$cins=as.factor(Odev$cins)
Odev$egitim=as.factor(Odev$egitim)
summary(Odev)

#Verinin Train ve Test setlerine ayrılması
set.seed(121)
split <- sample.split(Odev$nps_kat, SplitRatio = 0.8)
Test <- subset(Odev, split == FALSE)
Train <- subset(Odev, split == TRUE)

#CART modeli oluşturma
Odevcart = rpart(nps_kat ~ maas + vadeli + yatirim + kart + kredi + bes + 
                   sigorta + sube + atm + intmob + cagmerk + yil + yas + cins +
                   egitim, data = Train, method = "class", minbucket = 25)
summary(Odevcart)

#modelin görselleştirilmesi
prp(Odevcart)
rpart.plot(Odevcart)

#Test seti üzerinde tahmin yapma
Cartpre <- predict(Odevcart, Test, type = "class")

#Confusion Matrix
t <- table(Test$nps_kat, Cartpre)
t

#Accuracy
acc <- sum(diag(t))/sum(t)
acc
