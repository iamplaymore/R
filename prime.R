# 本函数用筛法计算素数。

# x是取值范围，用c(min, max)表示，若仅有一个值表示最大值。

# 最后返回的是指定范围内的素数向量。 

# Created on 2014.5.30
# Modified on 2014.5.30

prime <- function(x){
  stopifnot(is.numeric(x))
  
  # 检查x的合法性
  if (length(x) == 1){
    x[2] <- x
    x[1] <- 2
  }
  
  res <- c(x[1]:x[2])
  flag <- rep(TRUE, x[2]-x[1]+1)
  
  for (i in 2:sqrt(x[2])){
    flag[which((res %% i) == 0 & res > i)] <- FALSE
  }
  
  return(res[flag])
}

Demo <- function(){
  p <- prime(1000)
  p <- prime(c(800,1000))
}