#Generiramo izide neodvisnih slučjanih spremenljivk X in Y, ki sta porazdeljeni enakomerno na 
#intervalu [0,1]
X <- runif(100, min=0, max=1)
Y <- runif(100, min=0, max=1)

#La Relance
R1 <- c() #dobiček/izguba posamezne igre 1.igralca
R2 <- c() #dobiček/izguba posamezne igre 2.igralca
Rprvega <- c(0) #Spreminjaje dobička skozi igre 1.igralca
Rdrugega <- c(0) #Spreminjaje dobička skozi igre 2.igralca
B <- 5 #predpostavimo, da je B=1
V1 <- -(B^2)/(B+2)^2 #vrednost igre za 1.igralca 
c1 <- B/(B+2) #mejna vrednost za stavo 
Z1 <- 0 #število zmag 1.igralca
Z2 <- 0 #število zmag 2.igralca

for (i in 1:length(X)){
  if (X[i] > c1^2){
    if (Y[i] > c1){
      if (X[i] > Y[i]){
        R1[i] = (B+1)
        Rprvega[i+1] = Rprvega[i]+(B+1)
        R2[i] = -(B+1)
        Rdrugega[i+1] = Rdrugega[i]-(B+1)
        Z1 = Z1+1}
      else{
        R1[i] = -(B+1)
        Rprvega[i+1] = Rprvega[i]-(B+1)
        R2[i] = (B+1)
        Rdrugega[i+1] = Rdrugega[i]+(B+1)
        Z2 = Z2+1}}
    else{
      R1[i] = 1 
      Rprvega[i+1] = Rprvega[i]+1
      R2[i] = -1
      Rdrugega[i+1] = Rdrugega[i]-1
      Z1 = Z1+1}
  }else{
    R1[i] = -1
    Rprvega[i+1] = Rprvega[i]-1
    R2[i] = 1 
    Rdrugega[i+1] = Rdrugega[i]+1
    Z2 = Z2+1
  }}

plot(Rprvega,type='l')
plot(Rdrugega,type='l')

Dobicek1 <- sum(R1)
Dobicek2 <- sum(R2)

#Von Neumman
M1 <- c() #dobiček/izguba posamezne igre 1.igralca
M2 <- c() #dobiček/izguba posamezne igre 2.igralca
Mprvega <- c(0) #Spreminjaje dobička skozi igre 1.igralca
Mdrugega <- c(0) #Spreminjaje dobička skozi igre 2.igralca
V2 <- B/((B+1)*(B+4))
a <- B/((B+1)*(B+4))
b <- (B^2+4*B+2)/((B+1)*(B+4))
c <- (B*(B+3))/((B+1)*(B+4))
W1 <- 0 #število zmag 1.igralca
W2 <- 0 #število zmag 2.igralca

for (j in 1:length(X)){
  if (X[j] > b | X[j] < a){
    if (Y[j] > c){
      if (X[j] > Y[j]){
        M1[j] = (B+1)
        M2[j] = -(B+1)
        Mprvega[j+1] = Mprvega[j]+(B+1)
        Mdrugega[j+1] = Mdrugega[j]-(B+1)
        W1 = W1+1}
      else{
        M1[j] = -(B+1)
        M2[j] = (B+1)
        Mprvega[j+1] = Mprvega[j]-(B+1)
        Mdrugega[j+1] = Mdrugega[j]+(B+1)
        W2 = W2+1}}
    else{
      M1[j] = 1 
      M2[j] = -1
      Mprvega[j+1] = Mprvega[j]+1
      Mdrugega[j+1] = Mdrugega[j]-1
      W1 = W1+1}
  }else{
    if (X[j] > Y[j]){
      M1[j] = 1
      M2[j] = -1
      Mprvega[j+1] = Mprvega[j]+1
      Mdrugega[j+1] = Mdrugega[j]-1
      W1 = W1+1}
    else{
      M1[j] = -1
      M2[j] = 1
      Mprvega[j+1] = Mprvega[j]-1
      Mdrugega[j+1] = Mdrugega[j]+1
      W2 = W2+1} 
  }}

plot(Mprvega,type='l')
plot(Mdrugega,type='l')

DobicekPrvega <- sum(M1)
DobicekDrugega <- sum(M2)

#Primerjava modelov
t <- c()
t1 <- c(0)

for (k in 1:length(R1)){
  if (R1[k] < M1[k]) {
    t[k] = 1
    t1[k+1]=t1[k]+1
  }else {
    t[k] = 0
    t1[k+1]=t1[k]
  }
}

T=sum(t)
plot(t1,type='l')
