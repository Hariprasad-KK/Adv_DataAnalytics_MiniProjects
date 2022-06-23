#NAME  :  Hariprasad KK
#REG NO:  19BCE7079
#LAB  : L45+L46  (Lab_Assignment_6)

#Recommendation Systems


# collecting  the dataset 

x=read.csv("Ratings.csv")
x
full=data.frame(x) 
sample_data=subset(full,select=2:7)
sample_data

# creating the matrix 

R=data.matrix(sample_data)
X=matrix(0,8,2)
Y=matrix(0,2,7)
X
Y
for (i in 1:nrow(X)){
  for(j in 1:ncol(X)){
    X[i,j]=as.numeric(runif(1,min=0,max=1))
  }
}
for (i in 1:nrow(Y)){
  for(j in 1:ncol(Y)){
    Y[i,j]=as.numeric(runif(1,min=0,max=1))
  }
}
X=data.matrix(X)
Y=data.matrix(Y)
X
Y
matMul=function(mat1,mat2){
  mat2=t(mat2)
  sum=0
  for(i in 1:length(mat1)){
    sum=sum+mat1[i]*mat2[i]
  }
  return(sum)
}
alpha=0.0064
for (count in 1:5){
  cat("Epoch ")
  print(count)
  for(i in 1:nrow(R)){
    for(j in 1:ncol(R)){
      E=(matMul(X[i,],Y[,j]))^2
      cat("Error : ")
      print(E)
      for(k in 1:2){
        X[i,k]=X[i,k]+2*alpha*(matMul(X[i,],Y[,j]))*Y[k,j]
        Y[k,j]=Y[k,j]+2*alpha*(matMul(X[i,],Y[,j]))*X[i,k]
      }
    }
  }
}

# printing all the required values after training : 

print("Matrix after training : ")
print(R)
print("Latent features matrix X after training ")
print(X)
print("Latent features matrix Y after training")
print(Y)