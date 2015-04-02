require(igraph)

#First we define the objective function. The objective function is defined over six dimensions

objective <- function (q, t11 = 2/3, t12 = 1/3, t21 = 1/3, t22 = 2/3, t31 = 1/3, t32 = 1/3, l1 = 1/3, l2 = 1/2, l3 = 1/3)
{
  l1 * q[,1]^t11 * q[,2]^t12 + l2 * q[,3]^t21 * q[,4]^t22 +l3 * q[,5]^t31 * q[,6]^t32
}

obj <- function (q11, q12, q21, q22, q31, q32, t11 = 2/3, t12 = 1/3, t21 = 1/3, t22 = 2/3, t31 = 1/3, t32 = 1/3, l1 = 1/3, l2 = 1/2, l3 = 1/3)
{
  l1 * q11^t11 * q12^t12 + l2 * q21^t21 * q22^t22 +l3 * q31^t31 * q32^t32
}


#Current function I am using
obj1 <- function (q, t11 = 2/3, t12 = 1/3, t21 = 1/3, t22 = 2/3, t31 = 1/3, t32 = 1/3, l1 = 1/2, l2 = 1/2, l3 = 0)
{
  -(l1 * q[1]^t11 * q[2]^t12 + l2 * q[3]^t21 * q[4]^t22 +l3 * q[5]^t31 * q[6]^t32)
}

#Testing optimization code. Constrain matrix
p1 <- -0.5
p2 <- -0.5

constraint_mat <- rbind(c(p1,p2,p1,p2,p1,p2),diag(6))
constraint_vec <- c(-1,rep(0,6))

#Price data
pdata <- function (x){
  y <- matrix(nrow=x,data=runif(x,min=0,max=1))
  cbind(y,1-y,y,1-y,y,1-y)
}
#Testing optimization code:
constrOptim(rep(.3,6),obj1, ui=constraint_mat, ci=constraint_vec, grad=NULL, method = "Nelder-Mead",outer.iterations=7)

#Making something behave like a choice function
cf <- function(x){
  y <- matrix(nrow=dim(x)[1],ncol=6)
  for (i in 1:dim(x)[1]){
    y[i,] <- constrOptim(rep(.3,6),obj1, ui=rbind(-x[i,],diag(6)), ci=constraint_vec, grad=NULL, method = "Nelder-Mead",outer.iterations=7)$par
  }
  y
}

#Horrid function, will do for now
pairformat <- function (x) {
  rbind(x[,1:2],x[,3:4],x[,5:6])
}

#Sample procedure
#Generate data
n <- 100
price <- pdata(n)
quant <- cf(price)
#format as vectors--not also we are 'forgetting' that we caluated a bunch of these at the same time
sprice <- pairformat(price)
squant <- pairformat(quant)
#Secondary statistics
incomes <- rowSums(sprice*squant)
#Make a matrix with that data, every column is the same(for comparison). Each 'experiment' has three
#pairs of results, so the dimension needs to be 3*n X 3*x
income_mat <- matrix(nrow=3*n,ncol=3*n,data=incomes)
#Will be in the form (element ij is bundle i at price j)
pq <- squant %*% t(sprice)
#comparison matrix. Because we elected to have income be formatted in columns, we have to read the
#dominant bundles as being indexed by the columns. So a 'TRUE' at ij means bundle j is revealed preferred
#to bundle i.
compare_mat <- income_mat < pq
#Next, we need to start coercing more data
#Where there is an arrow from i to j, and j to i, there is an 'adjacency', an edge. Thus,
#we produce adjacency_mat from compare_mat * t(compare_mat) which is elementwise multiplication of
#compare_mat with its transverse

adjacency_mat <- compare_mat * t(compare_mat)

#Total demand/price
q1 <- quant[,1] + quant[,3] + quant[,5]

q2 <- quant[,2] + quant[,4] + quant[,6]

d1 <- cbind(price[,1], q1)
d2 <- cbind(price[,2], q2)

write(adjacency_mat, file="sim4dat.txt",ncolumns = 300)