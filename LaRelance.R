#La relance

# optimalna strategija za prvega igralca pri La Relance
LaRelance.opt1 <- function(X, c1) {
  return(X > c1^2)
}

# optimalna strategija za drugega igralca pri La Relance
LaRelance.opt2 <- function(Y, c1) {
  return(Y > c1)
}

# naključna izbira vrednosti pri kateri bo stavil 1.igralec
Karkoli1 <- function(X, random1) {
  return(X > random1)
}

# naključna izbira vrednosti pri kateri bo stavil 2.igralec
Karkoli2 <- function(Y, random2) {
  return(Y > random2)
}

LaRelance <- function(st1, st2, B, n = 100) {
  X <- runif(n, min=0, max=1)
  Y <- runif(n, min=0, max=1)
  c1 <- B/(B+2) #mejna vrednost za stavo 
  random1 <- runif(n, min=0, max=1)
  random2 <- runif(n, min=0, max=1)
  R1 <- c() #dobiček/izguba posamezne igre 1.igralca
  R2 <- c() #dobiček/izguba posamezne igre 2.igralca
  Rprvega <- c(0) #Spreminjaje dobička skozi igre 1.igralca
  Rdrugega <- c(0) #Spreminjaje dobička skozi igre 2.igralca
  Z1 <- 0 #število zmag 1.igralca
  Z2 <- 0 #število zmag 2.igralca
  for (i in 1:n){
    if (st1(X[i], c1)) { #prvi igralec je stavil
      if (st2(Y[i], c1)) {
        # drugi igralec je izenačil
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
          Z2 = Z2+1}
      } else {
        # drugi igralec je odstopil
        R1[i] = 1 
        Rprvega[i+1] = Rprvega[i]+1
        R2[i] = -1
        Rdrugega[i+1] = Rdrugega[i]-1
        Z1 = Z1+1
    }} else {
      # prvi igralec je odstopil
      R1[i] = -1
      Rprvega[i+1] = Rprvega[i]-1
      R2[i] = 1 
      Rdrugega[i+1] = Rdrugega[i]+1
      Z2 = Z2+1
    }
  }
  # končni izračuni, risanje grafov
  plot(Rprvega,type='l',col='blue',main='La relance dobiček/izguba',xlab = 'Število iger', ylab = 'Dobiček/izguba 1.igralca')
  return(c(Z1,sum(R1)))
}

V1 <- -(B^2)/(B+2)^2 #vrednost igre za 1.igralca 

sum(R1)
sum(R2)
