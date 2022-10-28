
library(amap)

 set.seed(1234)

data(USArrests)

  METHODS <- c("euclidean", "maximum", "manhattan", "canberra", 
               "binary","pearson","correlation","spearman","kendall",
               "abspearson","abscorrelation")
  METHODSLINKS <- c("ward", "single", "complete", "average", "mcquitty", 
                    "median", "centroid","centroid2")



for (mymethod in METHODS) {		    
    d = Dist(USArrests, method = mymethod)
    print(d)
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





