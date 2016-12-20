

# A FUNCTION THAT PRODUCES AN ALLEE
# EFFECT MODIFYING GAMETE FERTILIZATAION
# SUCCESS


B0=0.49
B1=0.72
D<- seq(0:1000)

y<- B1*log(D)+B0
y<- exp(y)

plot(D,y,ylab="Fertilization success",
	xlab="Density (fish/rkm)")
