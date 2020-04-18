## R script

#2.
Nile
summary(Nile)
stem(Nile)
stem(Nile[Nile != 456])

#3.
head(warpbreaks)
A = warpbreaks[warpbreaks$wool=="A",]
B = warpbreaks[warpbreaks$wool=="B",]
summary(A$breaks)
summary(B$breaks)
length(A$breaks)==length(B$breaks)
stem(A$breaks)
stem(B$breaks)
stem(A$breaks, scale=2)
stem(B$breaks)
stem(A$breaks)
stem(B$breaks, scale=0.5)

#4.

#4.1.
killed = Seatbelts[,1] * 1000/Seatbelts[,2] * 10000/Seatbelts[,5]
plot(Seatbelts[,2], Seatbelts[,5], main="scatterplot of drivers and kms", xlab="drivers", ylab="kms")
killed

#4.2.
summary(killed)
stem(killed)

#4.3.
Seatbelts[,8]
sum(Seatbelts[,8]==0)
sum(Seatbelts[,8]==1)
length(Seatbelts[,8])
a = killed[1:169]
b = killed[170:192]
summary(a)
summary(b)
stem(a, scale=1)
stem(b, scale=2)

#5.
stem(killed)
hist(killed, nclass=14, main="Hist of Killed", xlab="Killed", xlim=c(20,100))