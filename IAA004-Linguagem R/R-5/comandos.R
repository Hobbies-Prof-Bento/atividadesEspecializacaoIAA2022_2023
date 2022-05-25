a <- function (n){
    return(n^2)
}

print(a(2))

print(a(8))

b <- function(m1,m2){
    return(m1 %*% m2)
}

matrix1 <-matrix(c(1:5),nrow = 1)
print(matrix1)

matrix2 <- matrix(c(5:1),ncol = 1)
print(matrix2)

print(b(matrix1,matrix2))

mydf2 <- data.frame(nome=c("João", "Maria", "José", "Ana"), idade=sample(15:30,4,replace=TRUE))
print(mydf2)

c <- function(df){
    return (mean(df$idade))
}

print(c(mydf2))

d <- function(df){
    lista <- list(nome = df[order(max(df$idade)),][[1]], idade = df[order(max(df$idade)),][[2]])
    return(lista)
}

print(d(mydf2))