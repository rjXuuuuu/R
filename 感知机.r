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
--------------------- 
作者：littlely_ll 
来源：CSDN 
原文：https://blog.csdn.net/littlely_ll/article/details/53674517?utm_source=copy 
版权声明：本文为博主原创文章，转载请附上博文链接！
