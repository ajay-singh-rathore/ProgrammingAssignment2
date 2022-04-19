library(MASS)
mkCahMat <- function(a = matrix()) {
  inv<-NULL            #initializing inverse as NULL
  seter<-function(b){
    a<<-b
    inv<<-NULL
  }
  geter<-function()a             #function to get matrix a
  seterinv<-function(inverse)inv<<-inverse
  geterinv<-function(){ 
    inver<-ginv(a)
    inver%*%a           #function to obtain inverse of the matrix
  }
  list(seter = seter, geter = geter, 
       seterinv = seterinv, 
       geterinv = geterinv)
}


## Write a short comment describing this function
##This is used to get the cache data

cahSol <- function(a, ...) ##gets cache data      
{
  inv<-a$geterinv()                  
  if(!is.null(inv)){                 #checking whether inverse is NUll 
    message("getting cached data!")
    return(inv)                       #returns inverse value
  }
  data<-a$geter()
  inv<-solve(data,...)              #calculates inverse value
  a$seterinv(inv)
  inv   ## Return a matrix that is the inverse of 'a'
}

