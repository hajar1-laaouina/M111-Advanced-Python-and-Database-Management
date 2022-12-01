X <- c(-10,-9.5,-9,-8.5,-8,-7.5,-7,-6.5,-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,0,1,2,3,4,5,6,7,8,9,10)
f<-exp(X)/1+exp(X)
f

COL1<-sample(f,size=5, replace= FALSE , prob =   NULL)
COL2<-sample(f,size=5, replace= FALSE , prob =   NULL)
COL3<-sample(f,size=5, replace= FALSE , prob = NULL)
COL4<-sample(f,size=5, replace= FALSE , prob = NULL)
COL5<-sample(f,size=5, replace= FALSE , prob = NULL)

M <- matrix(c(COL1,COL2,COL3,COL4,COL5), nrow=5)
M 
det(M)
tr(M)

inv0 <- solve(M)

inv0
colSums(inv0)
rowSums(inv0)
# qst2
require(stats)

col_prime1 <- normalize.vector(M[,1])
col_prime2 <- normalize.vector(M[,2])
col_prime3 <- normalize.vector(M[,3])
col_prime4 <- normalize.vector(M[,4])
col_prime5 <- normalize.vector(M[,5])
M_prime <- matrix(c(col_prime1,col_prime2,col_prime3,col_prime4,col_prime5),nrow=5)
M_prime
#or using scale function
c1 <- colSums(M)
scale(M, center = FALSE, scale = c1)
# qst3
M2<-t(M)
Q=M%*%M2
Q
deter=det(Q)
# diagonal matrix
Dq=diag(diag(Q))
Dq
Dq%*%Dq
Dq%*%Dq%*%Dq

# qst4
ev <- eigen(Q)
ev
(values <- ev$values)
(vectors <- ev$vectors)
diagmatrix<-diag(values,nrow=5,ncol=5)
diagmatrix
invV <- solve(vectors)
invV
vectors%*%diagmatrix%*%invV
# qst5
library(expm) 

X3<-diagmatrix %^% 0.5
N<-vectors%*%X3%*%invV
N

exp(N)

M1=N%*%N
M2=M1%*%N
SOMME=N%^%0+M1*1/2+M2*1/6
SOMME