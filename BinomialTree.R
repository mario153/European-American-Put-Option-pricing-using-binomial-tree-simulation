# STAT G6505 Stochastic Methods in Finance 
# Mario Zhu
# Refer to formulas in exercise 3.8, Shreve vol II. Let the interest rate r=2%, the volatility =20%, the current spot is S_0=10. 
# Use a binomial tree with n=100 steps to 

# (a) compute the price of an European put (K,T)=(10,1)

# (b) do the same thing for an American put (K,T)=(10,1)

# (c, d)  Use your code to plot the relationship between the underlying stock price and the option price
# i.e., (S, V)-profile, at time t=0.25, 0.5, 0.75 in different colors.

## All codes below are written in R. 

r <- 0.02
sigma <- 0.2
S0 <- 10
K <- 10
n <- 100

u <- exp(sigma/sqrt(n)) #up factor
d <- exp(-sigma/sqrt(n)) # down factor

rp <- r/n #interest rate per period
disc <- 1/(1+rp)#discount factor per period
# Risk-neutral Probabilities:
p <- (rp+1-d)/(u-d)
q <- (u-rp-1)/(u-d)

### Part a, European Put Option pricing
S <- matrix(0,nrow = n+1,ncol=n+1)
S[1,1] = S0

for (i in 1:(n+1)){
  for(j in 2:(n+1)){
    S[i,j] = S[i,j-1]*u
  }
}

for(j in 2:(n+1)){
  for(i in 2:(n+1)){
  S[i,j] = S[i-1,j-1]*d
  }
}
##S is generated as an upper triangle matrix on the binomial tree

zeroM <- matrix(0,nrow = n+1,ncol=n+1) 

K <- matrix(10,nrow=n+1,ncol=n+1) #strike matrix on put option with K=10

temp_e <- K-S 

ge <- ifelse(temp_e>=0,ifelse(temp_e==10,0,temp_e),0)# intrinsic value of European put, recall the intrinsic value for European put=(K-S)+

Ve <- matrix(nrow=n+1,ncol=n+1)

Ve[,n+1] = ge[,n+1]

for(j in n:1){
  for(i in 1:n){
  Ve[i,j]=disc*(p*Ve[i,j+1]+(q*Ve[i+1,j+1]))
  }
}
# Ve[1,1] is the European Put price (K,T) = (10,1)
# Ve[1,1] = 0.6916215

################

### Part b, American Put Option pricing
Va <- matrix(nrow=n+1,ncol=n+1)# intrinsic value of American put, recall the intrinsic value for American put=max{(K-S)+,0}

Va[,n+1] = ifelse(ge[,n+1]>=0,ge[,n+1],0)

for(j in n:1){
  for(i in 1:n){
    Va[i,j]=ifelse(disc*(p*Va[i,j+1]+(q*Va[i+1,j+1]))>=ge[i,j],disc*(p*Va[i,j+1]+(q*Va[i+1,j+1])),ge[i,j])
  }
}
# Va[1,1] is the American Put price (K,T) = (10,1)
# Va[1,1] = 0.7099546

################

### Part c:[European Option] plot the relationship between the underlying stock price and the option price
### i.e., (S, V)-profile, at time t=0.25, 0.5, 0.75 in different colors


#EuropeanPutPrice_t1 is the function that calculates the European Put price with given underlying stock price when t=0.25
EuropeanPutPrice_t1 <- function(S0){
  r <- 0.02
  sigma <- 0.2
  K <- 10
  n <- 25
  u <- exp(sigma/sqrt(n)) #up factor
  d <- exp(-sigma/sqrt(n)) # down factor
  rp <- r/n #interest rate per period
  disc <- 1/(1+rp)#discount factor per period
  # Risk-neutral Probabilities:
  p <- (rp+1-d)/(u-d)
  q <- (u-rp-1)/(u-d)
  
  S <- matrix(0,nrow = n+1,ncol=n+1)
  S[1,1] = S0
  for (i in 1:(n+1)){
    for(j in 2:(n+1)){
      S[i,j] = S[i,j-1]*u
    }
  }

  for(j in 2:(n+1)){
    for(i in 2:(n+1)){
      S[i,j] = S[i-1,j-1]*d
    }
  }
  ##S is generated as an upper triangle matrix on the binomial tree
  zeroM <- matrix(0,nrow = n+1,ncol=n+1) 
  K <- matrix(10,nrow=n+1,ncol=n+1) #strike matrix on put option with K=10
  temp_e <- K-S 

  ge <- ifelse(temp_e>=0,ifelse(temp_e==10,0,temp_e),0)# intrinsic value of European put, recall the intrinsic value for European put=(K-S)+

  Ve <- matrix(nrow=n+1,ncol=n+1)

  Ve[,n+1] = ge[,n+1]

  for(j in n:1){
    for(i in 1:n){
      Ve[i,j]=disc*(p*Ve[i,j+1]+(q*Ve[i+1,j+1]))
    }
  }
  return (Ve[1,1])
}

### Same function When t = 0.5
EuropeanPutPrice_t2 <- function(S0){
  r <- 0.02
  sigma <- 0.2
  K <- 10
  n <- 50
  u <- exp(sigma/sqrt(n)) #up factor
  d <- exp(-sigma/sqrt(n)) # down factor
  rp <- r/n #interest rate per period
  disc <- 1/(1+rp)#discount factor per period
  # Risk-neutral Probabilities:
  p <- (rp+1-d)/(u-d)
  q <- (u-rp-1)/(u-d)
  
  S <- matrix(0,nrow = n+1,ncol=n+1)
  S[1,1] = S0
  for (i in 1:(n+1)){
    for(j in 2:(n+1)){
      S[i,j] = S[i,j-1]*u
    }
  }
  
  for(j in 2:(n+1)){
    for(i in 2:(n+1)){
      S[i,j] = S[i-1,j-1]*d
    }
  }
  ##S is generated as an upper triangle matrix on the binomial tree
  zeroM <- matrix(0,nrow = n+1,ncol=n+1) 
  K <- matrix(10,nrow=n+1,ncol=n+1) #strike matrix on put option with K=10
  temp_e <- K-S 
  
  ge <- ifelse(temp_e>=0,ifelse(temp_e==10,0,temp_e),0)# intrinsic value of European put, recall the intrinsic value for European put=(K-S)+
  
  Ve <- matrix(nrow=n+1,ncol=n+1)
  
  Ve[,n+1] = ge[,n+1]
  
  for(j in n:1){
    for(i in 1:n){
      Ve[i,j]=disc*(p*Ve[i,j+1]+(q*Ve[i+1,j+1]))
    }
  }
  return (Ve[1,1])
}

### Same function when t = 0.75
EuropeanPutPrice_t3 <- function(S0){
  r <- 0.02
  sigma <- 0.2
  K <- 10
  n <- 75
  u <- exp(sigma/sqrt(n)) #up factor
  d <- exp(-sigma/sqrt(n)) # down factor
  rp <- r/n #interest rate per period
  disc <- 1/(1+rp)#discount factor per period
  # Risk-neutral Probabilities:
  p <- (rp+1-d)/(u-d)
  q <- (u-rp-1)/(u-d)
  
  S <- matrix(0,nrow = n+1,ncol=n+1)
  S[1,1] = S0
  for (i in 1:(n+1)){
    for(j in 2:(n+1)){
      S[i,j] = S[i,j-1]*u
    }
  }
  
  for(j in 2:(n+1)){
    for(i in 2:(n+1)){
      S[i,j] = S[i-1,j-1]*d
    }
  }
  ##S is generated as an upper triangle matrix on the binomial tree
  zeroM <- matrix(0,nrow = n+1,ncol=n+1) 
  K <- matrix(10,nrow=n+1,ncol=n+1) #strike matrix on put option with K=10
  temp_e <- K-S 
  
  ge <- ifelse(temp_e>=0,ifelse(temp_e==10,0,temp_e),0)# intrinsic value of European put, recall the intrinsic value for European put=(K-S)+
  
  Ve <- matrix(nrow=n+1,ncol=n+1)
  
  Ve[,n+1] = ge[,n+1]
  
  for(j in n:1){
    for(i in 1:n){
      Ve[i,j]=disc*(p*Ve[i,j+1]+(q*Ve[i+1,j+1]))
    }
  }
  return (Ve[1,1])
}

x=seq(9.5,10.5,length=100)
y1=seq(length=100)
y2=seq(length=100)
y3=seq(length=100)
for (i in 1:100){
  y1[i]=EuropeanPutPrice_t1(x[i])
  y2[i]=EuropeanPutPrice_t2(x[i])
  y3[i]=EuropeanPutPrice_t3(x[i])
}
plot(c(9.5,10.5),c(0.5,0.8),type="n",xlab="stock price",ylab="European option price")
lines(x,y1,col="red")
lines(x,y2,col="blue")
lines(x,y3,col="green")
legend(10.3,0.75, c("t1=0.25","t2=0.5","t3=0.75"),lty=c(1,1,1), lwd=c(2.5,2.5,2.5),col=c("red","blue","green"))


### Part d:[American Option] plot the relationship between the underlying stock price and the option price
### i.e., (S, V)-profile, at time t=0.25, 0.5, 0.75 in different colors

#AmericanPutPrice_t1 is the function that calculates the American Put price with given underlying stock price when t=0.25
AmericanPutPrice_t1 <- function(S0){
  r <- 0.02
  sigma <- 0.2
  K <- 10
  n <- 25
  u <- exp(sigma/sqrt(n)) #up factor
  d <- exp(-sigma/sqrt(n)) # down factor
  rp <- r/n #interest rate per period
  disc <- 1/(1+rp)#discount factor per period
  # Risk-neutral Probabilities:
  p <- (rp+1-d)/(u-d)
  q <- (u-rp-1)/(u-d)
  
  S <- matrix(0,nrow = n+1,ncol=n+1)
  S[1,1] = S0
  for (i in 1:(n+1)){
    for(j in 2:(n+1)){
      S[i,j] = S[i,j-1]*u
    }
  }
  
  for(j in 2:(n+1)){
    for(i in 2:(n+1)){
      S[i,j] = S[i-1,j-1]*d
    }
  }
  ##S is generated as an upper triangle matrix on the binomial tree
  zeroM <- matrix(0,nrow = n+1,ncol=n+1) 
  K <- matrix(10,nrow=n+1,ncol=n+1) #strike matrix on put option with K=10
  temp_e <- K-S 
  
  ge <- ifelse(temp_e>=0,ifelse(temp_e==10,0,temp_e),0)# intrinsic value of European put, recall the intrinsic value for European put=(K-S)+
  
  Va <- matrix(nrow=n+1,ncol=n+1)# intrinsic value of American put, recall the intrinsic value for American put=max{(K-S)+,0}
  
  Va[,n+1] = ifelse(ge[,n+1]>=0,ge[,n+1],0)
  
  for(j in n:1){
    for(i in 1:n){
      Va[i,j]=ifelse(disc*(p*Va[i,j+1]+(q*Va[i+1,j+1]))>=ge[i,j],disc*(p*Va[i,j+1]+(q*Va[i+1,j+1])),ge[i,j])
    }
  }
  return (Va[1,1])
}

#AmericanPutPrice_t2 is the function that calculates the American Put price with given underlying stock price when t=0.5
AmericanPutPrice_t2 <- function(S0){
  r <- 0.02
  sigma <- 0.2
  K <- 10
  n <- 50
  u <- exp(sigma/sqrt(n)) #up factor
  d <- exp(-sigma/sqrt(n)) # down factor
  rp <- r/n #interest rate per period
  disc <- 1/(1+rp)#discount factor per period
  # Risk-neutral Probabilities:
  p <- (rp+1-d)/(u-d)
  q <- (u-rp-1)/(u-d)
  
  S <- matrix(0,nrow = n+1,ncol=n+1)
  S[1,1] = S0
  for (i in 1:(n+1)){
    for(j in 2:(n+1)){
      S[i,j] = S[i,j-1]*u
    }
  }
  
  for(j in 2:(n+1)){
    for(i in 2:(n+1)){
      S[i,j] = S[i-1,j-1]*d
    }
  }
  ##S is generated as an upper triangle matrix on the binomial tree
  zeroM <- matrix(0,nrow = n+1,ncol=n+1) 
  K <- matrix(10,nrow=n+1,ncol=n+1) #strike matrix on put option with K=10
  temp_e <- K-S 
  
  ge <- ifelse(temp_e>=0,ifelse(temp_e==10,0,temp_e),0)# intrinsic value of European put, recall the intrinsic value for European put=(K-S)+
  
  Va <- matrix(nrow=n+1,ncol=n+1)# intrinsic value of American put, recall the intrinsic value for American put=max{(K-S)+,0}
  
  Va[,n+1] = ifelse(ge[,n+1]>=0,ge[,n+1],0)
  
  for(j in n:1){
    for(i in 1:n){
      Va[i,j]=ifelse(disc*(p*Va[i,j+1]+(q*Va[i+1,j+1]))>=ge[i,j],disc*(p*Va[i,j+1]+(q*Va[i+1,j+1])),ge[i,j])
    }
  }
  return (Va[1,1])
}

#AmericanPutPrice_t3 is the function that calculates the American Put price with given underlying stock price when t=0.75
AmericanPutPrice_t3 <- function(S0){
  r <- 0.02
  sigma <- 0.2
  K <- 10
  n <- 75
  u <- exp(sigma/sqrt(n)) #up factor
  d <- exp(-sigma/sqrt(n)) # down factor
  rp <- r/n #interest rate per period
  disc <- 1/(1+rp)#discount factor per period
  # Risk-neutral Probabilities:
  p <- (rp+1-d)/(u-d)
  q <- (u-rp-1)/(u-d)
  
  S <- matrix(0,nrow = n+1,ncol=n+1)
  S[1,1] = S0
  for (i in 1:(n+1)){
    for(j in 2:(n+1)){
      S[i,j] = S[i,j-1]*u
    }
  }
  
  for(j in 2:(n+1)){
    for(i in 2:(n+1)){
      S[i,j] = S[i-1,j-1]*d
    }
  }
  ##S is generated as an upper triangle matrix on the binomial tree
  zeroM <- matrix(0,nrow = n+1,ncol=n+1) 
  K <- matrix(10,nrow=n+1,ncol=n+1) #strike matrix on put option with K=10
  temp_e <- K-S 
  
  ge <- ifelse(temp_e>=0,ifelse(temp_e==10,0,temp_e),0)# intrinsic value of European put, recall the intrinsic value for European put=(K-S)+
  
  Va <- matrix(nrow=n+1,ncol=n+1)# intrinsic value of American put, recall the intrinsic value for American put=max{(K-S)+,0}
  
  Va[,n+1] = ifelse(ge[,n+1]>=0,ge[,n+1],0)
  
  for(j in n:1){
    for(i in 1:n){
      Va[i,j]=ifelse(disc*(p*Va[i,j+1]+(q*Va[i+1,j+1]))>=ge[i,j],disc*(p*Va[i,j+1]+(q*Va[i+1,j+1])),ge[i,j])
    }
  }
  return (Va[1,1])
}

a=seq(9.5,10.5,length=100)
b1=seq(length=100)
b2=seq(length=100)
b3=seq(length=100)
for (i in 1:100){
  b1[i]=AmericanPutPrice_t1(a[i])
  b2[i]=AmericanPutPrice_t2(a[i])
  b3[i]=AmericanPutPrice_t3(a[i])
}
plot(c(9.5,10.5),c(0.5,0.8),type="n",xlab="stock price",ylab="American option price")
lines(a,b1,col="red")
lines(a,b2,col="blue")
lines(a,b3,col="green")
legend(10.3,0.75, c("t1=0.25","t2=0.5","t3=0.75"),lty=c(1,1,1), lwd=c(2.5,2.5,2.5),col=c("red","blue","green"))

