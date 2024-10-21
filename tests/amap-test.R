
library(amap)

 set.seed(1234)

data(USArrests)

  METHODS <- c("euclidean", "maximum", "manhattan", "canberra", 
               "binary","pearson","correlation","spearman","kendall",
               "abspearson","abscorrelation")
  METHODSLINKS <- c("ward", "single", "complete", "average", "mcquitty", 
                    "median", "centroid","centroid2","ward.D2")



for (mymethod in METHODS) {		    
    d = Dist(USArrests, method = mymethod)
  
    k  = Kmeans(USArrests, centers = 4, method = mymethod)
    print(k)
    for (mylink in METHODSLINKS)
    {
	cat(mylink)
	cat(mymethod)
	hc <- hcluster(USArrests,link = mylink, method =  mymethod, nbproc=4)
	print(hc)
    }
}

  COMMONDIST <- c("euclidean", "maximum", "manhattan", "canberra", 
               "binary")
  COMMONLINKS <- c( "single", "complete", "average", "mcquitty", 
                    "median", "centroid","ward.D2")

for (mymethod in COMMONDIST) {		    
   d = dist(USArrests,method = mymethod)
   d2 = Dist(USArrests,method = mymethod)
   cat("test",mymethod)
   stopifnot(floor(d * 1000) == floor(d2*1000))
}
d = dist(USArrests)
for(mylink in COMMONLINKS){
  cat("test",mylink)
  h = hclust(d, method = mylink)
  hc = hcluster(USArrests,link = mylink)
  stopifnot(h$order == hc$order)
  stopifnot(floor(h$height * 1000) == floor(hc$height*1000))
}

hc <- hcluster(USArrests, nbproc=1)
print(hc)
    





KERNELS = c("gaussien", "quartic", "triweight", "epanechikov" , 
"cosinus", "uniform")

for(myKernel in KERNELS) {
  myacp = acprob(USArrests, kernel = myKernel);
  print(myacp)
} 



d <-2 * matrix(c(9,  8,  5,  7,  7,  2
     ,  8,  9,  2,  5,  1,  7
     ,  5,  2,  9,  8,  7,  1
     ,  7,  5,  8,  9,  3,  2
     ,  7,  1,  7,  3,  9,  6
     ,  2,  7,  1,  2,  6,  9),ncol=6,byrow=TRUE) - 9

pop(d)





