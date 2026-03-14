# Class07
Matthew Chan (A18130675)

## Background

Today we wuill begin our exploration of some important machine learning
methods, namely **clustering** and **dimensionality reduction**

Let’s make up some input data for clustering where we know what the
natural “clusters” are.

The function `rnorm()` can be useful here. It returns a bell curve out
of a sample n, with mean and sd as things you can change.

``` r
hist( rnorm(5000, mean=10) )
```

![](Class07_files/figure-commonmark/unnamed-chunk-1-1.png)

> Q. Generate 30 random numbers centered at 3 and another 30 centered at
> -3

``` r
tmp <- c( ( rnorm(30, mean=3) ), rnorm(30, mean=-3) ) #combines both rnorm distributions

x <- cbind(x=tmp, y=rev(tmp)) #compiles to column

plot(x)
```

![](Class07_files/figure-commonmark/unnamed-chunk-2-1.png)

\## K-means clustering

The main function in “base R” for K-means clustering is called
surprisingly `kmeans()`

``` r
km <- kmeans(x, centers= 2)
km
```

    K-means clustering with 2 clusters of sizes 30, 30

    Cluster means:
              x         y
    1 -3.403350  2.982657
    2  2.982657 -3.403350

    Clustering vector:
     [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1
    [39] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

    Within cluster sum of squares by cluster:
    [1] 49.70079 49.70079
     (between_SS / total_SS =  92.5 %)

    Available components:

    [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    [6] "betweenss"    "size"         "iter"         "ifault"      

> Q. What component of the results object details the cluster sizes

``` r
km$size
```

    [1] 30 30

> Q. What component of the results object details the cluster centers?

``` r
km$centers
```

              x         y
    1 -3.403350  2.982657
    2  2.982657 -3.403350

> What component of the results object details the cluster membership
> vector( ie. our main result of which points lie in which cluster) ?

``` r
km$cluster
```

     [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1
    [39] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

> Q. Plot our clustering results with points colored by cluster and also
> add the cluster centers as new points colored blue?

``` r
plot( x, col=km$cluster) #plots the color, uses the membership to color
points(km$centers, col="blue", pch=15) #adds points to an existing plot
```

![](Class07_files/figure-commonmark/unnamed-chunk-7-1.png)

> Q. Run `kmeans()` again and this time produce 4 clusters (and call
> your result object `k4`) and make a results figure like above?

``` r
k4 <- kmeans(x, centers = 4)
plot(x, col=k4$cluster)
points(k4$centers, col="red", pch=15)
```

![](Class07_files/figure-commonmark/unnamed-chunk-8-1.png)

The metric

``` r
km$tot.withinss
```

    [1] 99.40157

``` r
k4$tot.withinss
```

    [1] 72.41113

> Q. Let’s try different number of K (centers) from 1 to 30 and see what
> the best result is

``` r
ans <- NULL 
for(i in 1:30) {
  ans <- c(ans, kmeans(x, centers =i)$tot.withinss)
}

ans
```

     [1] 1322.834066   99.401573   83.593585   66.616705   53.550111   61.212157
     [7]   43.731882   33.364869   28.034466   23.539991   24.082416   18.124508
    [13]   16.158393   13.280198   14.508029   11.604285   10.647794   11.341317
    [19]    9.812401    8.557461    6.811067    5.746805    5.751585    4.700402
    [25]    4.872063    4.066460    4.204138    3.900007    3.980525    3.378759

``` r
plot(ans, typ="o") #gives a scree plot. it gives you the most with the least number of centers. you want the elbow part of the graph. The problem with k is that it is self fulfilling
```

![](Class07_files/figure-commonmark/unnamed-chunk-11-1.png)

**Key-Point:** K-means will impose a clustering structure on your data
even if it is not there - it will always give you the answer you asked
for even if that answer is silly.

## Hierarchieal Clustering

The main function for Hierarchial Clustering is called `hclust()`.
Unlike `kmeans()` (which does all the work for you) you cant just pass
`hclust()` our raw input data. It needs a “distance matrix” like the one
returned from the `dist()` function.

``` r
d <- dist(x)
hc <- hclust(d)
plot(hc)
```

![](Class07_files/figure-commonmark/unnamed-chunk-12-1.png)

To extract our cluster membership vector from a `hclust()` result object
we have to “cut” our tree at a given height to yield separate
“groups/branches”

``` r
plot(hc)
abline(h=8, col="red", lty=2)
```

![](Class07_files/figure-commonmark/unnamed-chunk-13-1.png)

To do this we use the `cutree` function on our `hclust()` object:

``` r
grps <- cutree(hc, h=8)
grps
```

     [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2
    [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
table(grps, km$cluster)
```

        
    grps  1  2
       1  0 30
       2 30  0

## PCA of UK food data

Import the dataset of food consumption in the UK

``` r
url <- "https://tinyurl.com/UK-foods"
x <- read.csv(url)
x
```

                         X England Wales Scotland N.Ireland
    1               Cheese     105   103      103        66
    2        Carcass_meat      245   227      242       267
    3          Other_meat      685   803      750       586
    4                 Fish     147   160      122        93
    5       Fats_and_oils      193   235      184       209
    6               Sugars     156   175      147       139
    7      Fresh_potatoes      720   874      566      1033
    8           Fresh_Veg      253   265      171       143
    9           Other_Veg      488   570      418       355
    10 Processed_potatoes      198   203      220       187
    11      Processed_Veg      360   365      337       334
    12        Fresh_fruit     1102  1137      957       674
    13            Cereals     1472  1582     1462      1494
    14           Beverages      57    73       53        47
    15        Soft_drinks     1374  1256     1572      1506
    16   Alcoholic_drinks      375   475      458       135
    17      Confectionery       54    64       62        41

> Q1. How many rows and columns are in your new data frame named x? What
> R functions could you use to answer this questions?

``` r
dim(x)
```

    [1] 17  5

One solution to set the row names is to do it by hand

``` r
rownames(x) <- x[,1]
x
```

                                          X England Wales Scotland N.Ireland
    Cheese                           Cheese     105   103      103        66
    Carcass_meat              Carcass_meat      245   227      242       267
    Other_meat                  Other_meat      685   803      750       586
    Fish                               Fish     147   160      122        93
    Fats_and_oils            Fats_and_oils      193   235      184       209
    Sugars                           Sugars     156   175      147       139
    Fresh_potatoes          Fresh_potatoes      720   874      566      1033
    Fresh_Veg                    Fresh_Veg      253   265      171       143
    Other_Veg                    Other_Veg      488   570      418       355
    Processed_potatoes  Processed_potatoes      198   203      220       187
    Processed_Veg            Processed_Veg      360   365      337       334
    Fresh_fruit                Fresh_fruit     1102  1137      957       674
    Cereals                        Cereals     1472  1582     1462      1494
    Beverages                     Beverages      57    73       53        47
    Soft_drinks                Soft_drinks     1374  1256     1572      1506
    Alcoholic_drinks      Alcoholic_drinks      375   475      458       135
    Confectionery            Confectionery       54    64       62        41

To remove the first column I can use the minus index trick.

``` r
x <- x[,-1]
```

A better way to do this is to set the row names to the first column with
`read.csv()`

``` r
x <- read.csv(url, row.names = 1)
```

> Q2. Which approach to solving the ‘row-names problem’ mentioned above
> do you prefer and why? Is one approach more robust than another under
> certain circumstances?

## Spotting major differences and trends

Is difficult even in this wee 17D dataset

``` r
barplot(as.matrix(x), beside=F, col=rainbow(nrow(x)))
```

![](Class07_files/figure-commonmark/unnamed-chunk-21-1.png)

> Q5: Generating all pairwise plots may help somewhat. Can you make
> sense of the following code and resulting figure? What does it mean if
> a given point lies on the diagonal for a given plot?

if a given plot is off of the diagonal then there will be a difference.
for ireland there are many points that are off the diagonal than the
other countries

``` r
pairs(x, col=rainbow(nrow(x)), pch=16)
```

![](Class07_files/figure-commonmark/unnamed-chunk-22-1.png)

``` r
library(pheatmap)
pheatmap(x)
```

![](Class07_files/figure-commonmark/unnamed-chunk-23-1.png)

## PCA to the rescue

The main PCA function in “base R” is called `prcomp()` This function
wants the transpose of our food data as input (i.e. the foods as
columsna dn the countries as rows).

``` r
pca <- prcomp(t(x))
```

``` r
summary(pca)
```

    Importance of components:
                                PC1      PC2      PC3     PC4
    Standard deviation     324.1502 212.7478 73.87622 2.7e-14
    Proportion of Variance   0.6744   0.2905  0.03503 0.0e+00
    Cumulative Proportion    0.6744   0.9650  1.00000 1.0e+00

``` r
attributes(pca)
```

    $names
    [1] "sdev"     "rotation" "center"   "scale"    "x"       

    $class
    [1] "prcomp"

To make one of our main PCA result figures we turn to `pca$x` the scores
along our new PCs . This is called “PC plot” or “score plot” or
“Ordination plot”

``` r
my_cols <- c("orange", "red", "blue", "darkgreen")
```

``` r
library(ggplot2)

ggplot(pca$x) + 
  aes(PC1, PC2) +
  geom_point(col=my_cols)
```

![](Class07_files/figure-commonmark/unnamed-chunk-28-1.png)

The second major result figure is called a “loadings plot” or “variable
contributions plot” or “weight plot”

``` r
ggplot(pca$rotation) +
  aes(PC1, rownames(pca$rotation)) +
  geom_col()
```

![](Class07_files/figure-commonmark/unnamed-chunk-29-1.png)
