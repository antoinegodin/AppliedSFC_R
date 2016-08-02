Applied SFC modelling with R
========================================================
author: Antoine Godin
date: 13 May 2016

Outline
========================================================
TODO

Installing the dependent libraries
========================================================
You need to install all the required libraries This is for traditional libraries

```r
install.packages("expm")
install.packages("igraph")
```

For non-convetional libraries, such as the one need to visualise DAGs, you need to do the following

```r
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
```

Installing the packages
========================================================
Finally you can then download the PKSFC package from github and install it locally

```r
install.packages("path/PKSFC_1.3.tar.gz", repos = NULL, type="source")
```



Loading the package
========================================================
Now we're ready to load the package:

```r
library(PKSFC)
```


The Gauss Seidel Algorithm
========================================================

- Principle:
Solving $Ax=b,\, A\in\mathbb{R}^{n\times n},\, b\in \mathbb{R}^n$ via an iterative algorithm, where each iteration can be represented by $L x^{k+1} = b-Ux^{k},\, A=L+U$.

- Pseudo-code:
  1. Select initial values $x^0$
  2. While $k<maxIter$ \& $\delta < tolValue$
    a. For each $i=1,...,n$: $$x_i^{k+1}=\frac{1}{a_{ii}}\left( b_i-\sum^{i-1}_{j=1}a_{ij} x_j^{k+1}-\sum_{j=i+1}^n a_{ij}x_j^k\right)$$
    b. Compute $\delta$: $$\delta = \frac{x^{k+1}-x^k}{x^k}$$

Slide With Code
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-7](Lecture1-figure/unnamed-chunk-7-1.png)
