library(tidyverse)

f <- function(x){
  
  return((x^2)*(exp(-3*x)))
  
}

#L?mites de integraci?n

a <- 2
b <- 3

#N?mero de trapecios en los que se divide el ?rea

n <- 28

#h es la anchura de los trapecios en los que se divide el ?rea

h <- (b - a)/n

#F?rmula para el m?todo del trapecio compuesto

s <- (f(a) + f(b))/2

    #Este bucle es el sumatorio de la f?rmula
for (i in 1:(n - 1)) {
  
  s <- s + f(a + i * h)
  
}

integral <- s * h
print(integral)

#Error

 ##Funcion para la segunda derivada
DD<-function(expr,name,order=1){
  if(order<1) stop("Order must be >=1")
  if(order==1)D(expr,name)
  else DD(D(expr,name),name,order-1)
}
 expe <- expression((x^2)*(exp(-3*x)))
 DD(expe,"x",2)
 
 ##Segunda derivada de nuestra funci?n evaluada en 2
 dff <- 2 * (exp(-3 * 2)) - 2 * 2 * (exp(-3 * 2) * 3) - (2 * 2 * (exp(-3 * 2) * 3) - (2^2) * (exp(-3 * 2) * 3 * 3))
 
 expres <- expression(log((((b - a)^3)/((12*x)^2))* dff))
 D(expres, "x")
 
 mx <- matrix(nrow = n, ncol = 2)
 for (z in 1:n) {
   
   e <- abs((((b - a)^3)/((12*z)^2))* dff)
   
   mx[z,1] <- z
   mx[z,2] <- e
 }
 mx

 mm <- matrix(nrow = n, ncol = 2)
 for (z in 1:n) {
   
   e <- abs((((b - a)^3)/((12*z)^2))* dff)
   
   mm[z,1] <- log(z, exp(1))
   mm[z,2] <- log(e, exp(1))
 }
mm



graph_logn <- ggplot(data.frame(x = c(0,5)) , mapping = aes(x = x)) + 
  stat_function(fun = function(x){log(x,exp(1))}, color = "blue", lwd = 2) +
  geom_segment(aes(x = 0, y = 0, xend = 5, yend = 0)) +
  geom_segment(aes(x = 0, y = -3, xend = 0, yend = 3))

graph_logn

graph_logerror <- ggplot(data.frame(x = c(0,5)) , mapping = aes(x = x)) + 
  stat_function(fun = function(x){log((((b - a)^3)/((12*x)^2))* dff,exp(1))}, color = "blue", lwd = 2) +
  geom_segment(aes(x = 0, y = 0, xend = 5, yend = 0)) +
  geom_segment(aes(x = 0, y = -12, xend = 0, yend = 3))

graph_logerror

graph_error <- ggplot(data.frame(x = c(0,1)) , mapping = aes(x = x)) + 
  stat_function(fun = function(x){(((b - a)^3)/((12*x)^2))* dff}, color = "blue", lwd = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0)) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 3))

graph_error




graph_integral <- ggplot(data.frame(x = c(a,b)) , mapping = aes(x = x)) + 
  stat_function(fun = f, color = "red", lwd = 2) +
  geom_segment(aes(x = b, y = f(b), xend = b, yend = 0)) +
  geom_segment(aes(x = a, y = f(a), xend = a, yend = 0)) +
  geom_segment(aes(x =a, y = 0, xend = b, yend = 0))

h2 <- 0
h3 <- h
for (j in 1:(n)) {
  
  graph_integral <- graph_integral +
    geom_segment(aes_(x = a + h2, y = f(a + h2), xend = a + h3 , yend = f( a + h3))) +
    geom_segment(aes_(x = a + h2, y = f(a + h2), xend = a + h2 , yend = 0)) + 
    geom_polygon(data = data.frame(cbind(c(a + h2, a + h3, a + h3, a + h2), c(f(a + h2), f(a + h3), 0, 0))), aes(x=X1, y=X2), fill = 'blue', alpha = 0.2)
    
  h2 <- h2 + (b - a)/n
  h3 <- h3 + (b - a)/n
  
}
graph_integral





m <- matrix(nrow = n, ncol = 2)

for (z in 1:n) {
  
  h <- (b - a)/z
  
  s <- (f(a) + f(b))/2
  
  for (i in 1:(z - 1)) {
    
    s <- s + f(a + i * h)
    
  }
  
  integral <- s * h
  m[z,1] <- z
  m[z,2] <- integral
  
}
m



graph_integrales <- ggplot(data.frame(x = c(0,b + 2)) , mapping = aes(x = x)) + 
  stat_function(fun = f, color = "red", lwd = 2)

graph_integrales




