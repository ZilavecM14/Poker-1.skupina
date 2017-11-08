#Generiramo izide neodvisnih slučjanih spremenljivk X in Y, ki sta porazdeljeni enakomerno na 
#intervalu [0,1]
X <- runif(1000, min=0, max=1)
Y <- runif(1000, min=0, max=1)

#La Relance
R1 <- c() #dobiček/izguba posamezne igre 1.igralca
R2 <- c() #dobiček/izguba posamezne igre 2.igralca
B <- 10 #predpostavimo, da je B=1
V1 <- -(B^2)/(B+2)^2 #vrednost igre za 1.igralca 
c1 <- B/(B+2) #mejna vrednost za stavo 
Z1 <- 0 #število zmag 1.igralca
Z2 <- 0 #število zmag 2.igralca

for (i in 1:length(X)){
  if (X[i] > c1^2){
    if (Y[i] > c1){
      if (X[i] > Y[i]){
        R1[i] = (B+1)
        R2[i] = -(B+1)
        Z1 = Z1+1}
      else{
        R1[i] = -(B+1)
        R2[i] = (B+1)
        Z2 = Z2+1}}
    else{
      R1[i] = 1 
      R2[i] = -1
      Z1 = Z1+1}
  }else{
    R1[i] = -1
    R2[i] = 1 
    Z2 = Z2+1
    }}


#Von Neumman
M1 <- c()
M2 <- c()
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
        W1 = W1+1}
      else{
        M1[j] = -(B+1)
        M2[j] = (B+1)
        W2 = W2+1}}
    else{
      M1[j] = 1 
      M2[j] = -1
      W1 = W1+1}
  }else{
    if (X[j] > Y[j]){
      M1[j] = 1
      M2[j] = -1
      W1 = W1+1}
    else{
      M1[j] = -1
      M2[j] = 1
      W2 = W2+1} 
    }}
