## R script

#1.
x = seq(1, 9, by=2)
x
fivenum(x)
summary(x)

y = seq(1, 11, by=2)
y
fivenum(y)
summary(y)

#2.
boxplot(len ~ dose, data = ToothGrowth, xlab = "Amount of Dose", ylab = "Length of Odontoblasts",
        main = "ToothGrowth", notch=T, col="papayawhip")

boxplot(len ~ dose, data = ToothGrowth, boxwex = 0.25, at = 1:3 - 0.2,
        col = "papayawhip", main = "Tooth Growth",subset = supp == "VC", 
        xlab = "Amount of Dose", ylab = "Length of Odontoblasts",
        xlim = c(0.5, 3.5), ylim = c(0, 35), notch=T, xaxt = "n")

boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset = supp == "OJ", col = "peachpuff2", notch=T, xaxt = "n")

axis(side = 1, labels = c("0.5", "1", "2"), at = 1:3)

legend(2.5, 7, c("Ascorbic acid", "Orange juice"), fill = c("papayawhip", "peachpuff2"))

#3.
#3-1.
fivenum(airquality$Ozone)
boxplot(airquality$Ozone, main="Boxplot of Ozone", col="papayawhip")

#3-2.
airquality[airquality$Wind >= 7,"Wind_7"] = "Over than 7"
airquality[airquality$Wind < 7,"Wind_7"] = "Less than 7"

boxplot(Ozone ~ Wind_7, data=airquality, xlab="Wind", main="Boxplot of Ozone by Wind", col="papayawhip", notch=T)

source("http://mgimond.github.io/ES218/es218.R")
lsum(airquality[airquality$Wind < 7,]$Ozone)
lsum(airquality[airquality$Wind >= 7,]$Ozone)

par(mfrow=c(1,2))
hist(airquality[airquality$Wind < 7,]$Ozone, main="Hist of Ozone(Wind less than 7mph)", xlab="Ozone", col="papayawhip", xlim=c(0,200), ylim=c(0,25), breaks=10)
hist(airquality[airquality$Wind >= 7,]$Ozone, main="Hist of Ozone(Wind over than 7mph)", xlab="Ozone", col="papayawhip", xlim=c(0,200), breaks=10)

#3-3.
airquality[airquality$Temp >= 80,"Temp_80"] = "Over than 80"
airquality[airquality$Temp < 80,"Temp_80"] = "Less than 80"

par(mfrow=c(1,1))
boxplot(Ozone ~ Temp_80, data=airquality, xlab="Temperature", main="Boxplot of Ozone by Temperature", col="papayawhip", notch=T)

lsum(airquality[airquality$Temp < 80,]$Ozone)
lsum(airquality[airquality$Temp >= 80,]$Ozone)

par(mfrow=c(1,2))
hist(airquality[airquality$Temp < 80,]$Ozone, main="Hist of Ozone(Temp less than 80)", xlab="Ozone", col="papayawhip", xlim=c(0,200), breaks=10, ylim=c(0,25))
hist(airquality[airquality$Temp >= 80,]$Ozone, main="Hist of Ozone(Temp over than 80)", xlab="Ozone", col="papayawhip", xlim=c(0,200), breaks=10, ylim=c(0,25))

#4.
old = read.csv("광역시-구 인구(2000).csv")
new = read.csv("광역시-구 인구(2019).csv")

par(mfrow=c(1,1))
boxplot(인구~지역명, data=old, boxwex = 0.25, at = 1:7 - 0.2, 
          col="papayawhip", xlim=c(0.5,7.5), ylim=c(0,700000), xaxt = "n")

boxplot(인구~지역명, data=new, boxwex = 0.25, at = 1:7 + 0.2, 
          col="peachpuff2", add=TRUE, xaxt = "n")

title("광역시의 구별 인구 비교")

axis(side = 1, labels = c("광주", "대구", "대전", "부산", "서울", "울산", "인천"), 
     at = 1:7)

legend(0.5, 685000, c("2000", "2019"), fill = c("papayawhip", "peachpuff2"))
