#Generiramo izide neodvisnih slučjanih spremenljivk X in Y, ki sta porazdeljeni enakomerno na 
#intervalu [0,1]
X <- runif(1000, min=0, max=1)
Y <- runif(1000, min=0, max=1)

#La Relance
R1 <- c()
R2 <- c()
B <- 10 #predpostavimo, da je B=1
V1 <- -(B^2)/(B+2)^2
c1 <- B/(B+2)

for (x in X[]){
  for (y in Y[]){
    if (x > c1^2){
      if (y > c1){
        if (x > y){
          R1 = (B+1)
          R2 = -(B+1)}
        else
          R1 = -(B+1)
          R2 = (B+1)}
      else
        R1 = 1 
        R2 = -1
    }else{
      R1 = -1
      R2 = 1 
}}}



  

#Von Neumman
V2 = B/((B+1)(B+4))
a = B/((B+1)(B+4))
b = (B^2+4*B+2)/((B+1)(B+4))
c = (B(B+3))/((B+1)(B+4))

