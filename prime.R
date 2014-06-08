# ��������ɸ������������

# x��ȡֵ��Χ����c(min, max)��ʾ��������һ��ֵ��ʾ���ֵ��

# ��󷵻ص���ָ����Χ�ڵ����������� 

# Created on 2014.5.30
# Modified on 2014.5.30

prime <- function(x){
  stopifnot(is.numeric(x))
  
  # ���x�ĺϷ���
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