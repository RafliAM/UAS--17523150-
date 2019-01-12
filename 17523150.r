#Rafli Abdul Malik (17523150)


#No. 11
trap <- function(f, a, b){
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')}
  h <- b - a
  fx <- (h / 2) * (f(a) + f(b))
  return(fx)
}
f <- function(x) {
  return(x^2-6)
}
trap(f, 0,1)

#-------------------------------------------------------------------------------------------------------------

#No. 12
trap <- function(f, a, b){
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')}
  h <- b - a
  fx <- (h / 2) * (f(a) + f(b))
  return(fx)
}
f <- function(x) {
  return(x^3+4*x^2-10)
}
trap(f, 1,2)

#-------------------------------------------------------------------------------------------------------------

#No. 13 dan 14
h <- 0.1
x <- seq(0,1, by=h)
f <- function(x) {
  return(x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:10],f)
fn <- f(x[length(x)])


trap <- function (f0,f1,fn,h){
  L <- h*(f0+2*sum(fi)+fn)/2  # <-----jawabannya
  return(L)
}
trap(f0,fi,fn,h)

#-------------------------------------------------------------------------------------------------------------

#no. 15
h <- 0.2
x <- seq(0,1, by=h)
f <- function(x) {
  return(x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:4],f) #<------jawabannya
fn <- f(x[length(x)])

trap <- function (f0,f1,fn,h){
  L <- h*(f0+2*sum(fi)+fn)/2
  return(L)
}
trap(f0,fi,fn,h)