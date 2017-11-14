#Von Neumann

# optimalna strategija za prvega igralca pri Von Neumannu
VonNeumann.opt1 <- function(X, a, b) {
  return(X > b | X < a)
}

# optimalna strategija za drugega igralca pri La Relance
VonNeumann.opt2 <- function(Y, c) {
  return(Y > c)
}

VonNeumann <- function(st1, st2, B, n = 1000) {
  X <- runif(n, min=0, max=1)
  Y <- runif(n, min=0, max=1)
  a <- B/((B+1)*(B+4))
  b <- (B^2+4*B+2)/((B+1)*(B+4))
  c <- (B*(B+3))/((B+1)*(B+4))
  M1 <- c() #dobiček/izguba posamezne igre 1.igralca
  M2 <- c() #dobiček/izguba posamezne igre 2.igralca
  Mprvega <- c(0) #Spreminjaje dobička skozi igre 1.igralca
  Mdrugega <- c(0) #Spreminjaje dobička skozi igre 2.igralca
  W1 <- 0 #število zmag 1.igralca
  W2 <- 0 #število zmag 2.igralca
  for (j in 1:n){
    if (st1(X[j], a,b)) { #prvi igralec je stavil
      if (st2(Y[j], c)) {
        # drugi igralec je izenačil
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
          W2 = W2+1}
      }else {
        # drugi igralec je odstopil
        M1[j] = 1 
        M2[j] = -1
        Mprvega[j+1] = Mprvega[j]+1
        Mdrugega[j+1] = Mdrugega[j]-1
        W1 = W1+1
    }}else {
        # prvi igralec primerja in zmaga
        if (X[j] > Y[j]){
          M1[j] = 1
          M2[j] = -1
          Mprvega[j+1] = Mprvega[j]+1
          Mdrugega[j+1] = Mdrugega[j]-1
          W1 = W1+1
        # prvi igralec primerja in izgubi
          }else{
          M1[j] = -1
          M2[j] = 1
          Mprvega[j+1] = Mprvega[j]-1
          Mdrugega[j+1] = Mdrugega[j]+1
          W2 = W2+1} 
      }
  }
  # končni izračuni, risanje grafov
  plot(Mprvega,type='l',col='blue',main='Von Neumnann dobiček/izguba',xlab = 'Število iger', ylab = 'Dobiček/izguba 1.igralca')
  return(c(W1,sum(M1)))
}