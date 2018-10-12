percept <- function(data = data,eta = eta ){
  x <- data[,-dim(data)[2]]
  y <- data[,dim(data)[2]]
  w <- c(0,0)
  b <- 0
  len <- length(y)
  i <- 1  
  while(i <= len){
    if(y[i] * (x[i,] %*% w + b) <= 0){
    ## update w and b
      w <- w + eta * y[i] * x[i,]
      b <- b + eta * y[i]
      i <- 1 ##important, for traversing every point
    }
    else{
      i <- i + 1 
    }
  }
  return(list(w=w,b=b))
}

data <- matrix(c(3,3,1,4,3,1,1,1,-1),nr=3,byrow=T)
percept(data = data,eta = 1)

