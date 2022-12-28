Airline Market Segmentation
================
2022-12-26

### About the Study and the Data Set

Market segmentation is a way to divide a larger target market of
customers into smaller groups that are more similar to each other. One
common method for doing this is clustering, which is a technique that
can automatically identify groups of similar customers based on a set of
data. In this case, an airline is using clustering to find groups of
customers within their frequent flyer program who are similar to each
other. The airline hopes to use this information to design different
marketing strategies for each customer segment, such as offering
different types of mileage promotions.  
The AirlinesCluster.csv file contains data on 3,999 members of an
airline’s frequent flyer program. This data was taken from the textbook
“Data Mining for Business Intelligence” by Galit Shmueli, Nitin R.
Patel, and Peter C. Bruce

The dataset contained in the AirlinesCluster.csv file includes seven
variables that describe the characteristics of the frequent flyer
program members. These variables are:

- Balance: the number of miles that are eligible for award travel.

- QualMiles: the number of miles that qualify for TopFlight status.

- BonusMiles: the number of miles earned from non-flight bonus
  transactions in the past 12 months.

- BonusTrans: the number of non-flight bonus transactions in the past 12
  months.

- FlightMiles: the number of flight miles traveled in the past 12
  months.

- FlightTrans: the number of flight transactions in the past 12 months.

- DaysSinceEnroll: the number of days since the customer was enrolled in
  the frequent flyer program.

### EDA

``` r
airlines = read.csv("AirlinesCluster.csv")
str(airlines)
```

    ## 'data.frame':    3999 obs. of  7 variables:
    ##  $ Balance        : int  28143 19244 41354 14776 97752 16420 84914 20856 443003 104860 ...
    ##  $ QualMiles      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ BonusMiles     : int  174 215 4123 500 43300 0 27482 5250 1753 28426 ...
    ##  $ BonusTrans     : int  1 2 4 1 26 0 25 4 43 28 ...
    ##  $ FlightMiles    : int  0 0 0 0 2077 0 0 250 3850 1150 ...
    ##  $ FlightTrans    : int  0 0 0 0 4 0 0 1 12 3 ...
    ##  $ DaysSinceEnroll: int  7000 6968 7034 6952 6935 6942 6994 6938 6948 6931 ...

``` r
summary(airlines)
```

    ##     Balance          QualMiles         BonusMiles       BonusTrans    FlightMiles     
    ##  Min.   :      0   Min.   :    0.0   Min.   :     0   Min.   : 0.0   Min.   :    0.0  
    ##  1st Qu.:  18528   1st Qu.:    0.0   1st Qu.:  1250   1st Qu.: 3.0   1st Qu.:    0.0  
    ##  Median :  43097   Median :    0.0   Median :  7171   Median :12.0   Median :    0.0  
    ##  Mean   :  73601   Mean   :  144.1   Mean   : 17145   Mean   :11.6   Mean   :  460.1  
    ##  3rd Qu.:  92404   3rd Qu.:    0.0   3rd Qu.: 23801   3rd Qu.:17.0   3rd Qu.:  311.0  
    ##  Max.   :1704838   Max.   :11148.0   Max.   :263685   Max.   :86.0   Max.   :30817.0  
    ##   FlightTrans     DaysSinceEnroll
    ##  Min.   : 0.000   Min.   :   2   
    ##  1st Qu.: 0.000   1st Qu.:2330   
    ##  Median : 0.000   Median :4096   
    ##  Mean   : 1.374   Mean   :4119   
    ##  3rd Qu.: 1.000   3rd Qu.:5790   
    ##  Max.   :53.000   Max.   :8296

It appears that our variables have a big diversity in their mean values.
For instance, BounsMiles and Balance have the highest mean of values
between all the variables. While, BonusTrans and FlightTrans have the
lowest. Moreover, the different is so big that normalizing the data
seems to be a sound thing to begin with, because if we don’t the
clustering will be dominated and largly affected by the variables with
the higher values.

``` r
#Normalizing the data
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
```

``` r
summary(airlinesNorm)
```

    ##     Balance          QualMiles         BonusMiles        BonusTrans        FlightMiles     
    ##  Min.   :-0.7303   Min.   :-0.1863   Min.   :-0.7099   Min.   :-1.20805   Min.   :-0.3286  
    ##  1st Qu.:-0.5465   1st Qu.:-0.1863   1st Qu.:-0.6581   1st Qu.:-0.89568   1st Qu.:-0.3286  
    ##  Median :-0.3027   Median :-0.1863   Median :-0.4130   Median : 0.04145   Median :-0.3286  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000  
    ##  3rd Qu.: 0.1866   3rd Qu.:-0.1863   3rd Qu.: 0.2756   3rd Qu.: 0.56208   3rd Qu.:-0.1065  
    ##  Max.   :16.1868   Max.   :14.2231   Max.   :10.2083   Max.   : 7.74673   Max.   :21.6803  
    ##   FlightTrans       DaysSinceEnroll   
    ##  Min.   :-0.36212   Min.   :-1.99336  
    ##  1st Qu.:-0.36212   1st Qu.:-0.86607  
    ##  Median :-0.36212   Median :-0.01092  
    ##  Mean   : 0.00000   Mean   : 0.00000  
    ##  3rd Qu.:-0.09849   3rd Qu.: 0.80960  
    ##  Max.   :13.61035   Max.   : 2.02284

``` r
sd(airlinesNorm$Balance)
```

    ## [1] 1

We can see that in the new data set we created, airlinesNorm, the mean
of all the variables is 0 and the satndard diviation is 1. It is worth
nothing that, the variable with the maximum value is FlightMiles and the
variable with the minimum is DaysSinceEnroll.

### Hierarchical Clustering

``` r
distances = dist(airlinesNorm, method = "euclidean")
hierClust = hclust(distances, method = "ward.D")
```

``` r
plot(hierClust)
```

![](Airline%20Market%20Segmentation_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

It appears that 5 clusters will be a good choice for this problem.

``` r
plot(hierClust)
rect.hclust(hierClust, k = 5, border = "red")
```

![](Airline%20Market%20Segmentation_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
clusterGroups = cutree(hierClust, k = 5)
table(clusterGroups)
```

    ## clusterGroups
    ##    1    2    3    4    5 
    ##  776  519  494  868 1342

Let’s explore our cluster groups by finding the centroid of each
cluster.

``` r
library(knitr)
x = lapply(split(airlines, clusterGroups), colMeans)
kable(x)
```

<table class="kable_wrapper">
<tbody>
<tr>
<td>

|                 |            x |
|:----------------|-------------:|
| Balance         | 5.786690e+04 |
| QualMiles       | 6.443299e-01 |
| BonusMiles      | 1.036012e+04 |
| BonusTrans      | 1.082345e+01 |
| FlightMiles     | 8.318428e+01 |
| FlightTrans     | 3.028351e-01 |
| DaysSinceEnroll | 6.235365e+03 |

</td>
<td>

|                 |            x |
|:----------------|-------------:|
| Balance         | 1.106693e+05 |
| QualMiles       | 1.065983e+03 |
| BonusMiles      | 2.288176e+04 |
| BonusTrans      | 1.822929e+01 |
| FlightMiles     | 2.613418e+03 |
| FlightTrans     | 7.402698e+00 |
| DaysSinceEnroll | 4.402414e+03 |

</td>
<td>

|                 |            x |
|:----------------|-------------:|
| Balance         | 1.981916e+05 |
| QualMiles       | 3.034615e+01 |
| BonusMiles      | 5.579586e+04 |
| BonusTrans      | 1.966397e+01 |
| FlightMiles     | 3.276761e+02 |
| FlightTrans     | 1.068826e+00 |
| DaysSinceEnroll | 5.615709e+03 |

</td>
<td>

|                 |            x |
|:----------------|-------------:|
| Balance         | 52335.913595 |
| QualMiles       |     4.847926 |
| BonusMiles      | 20788.766129 |
| BonusTrans      |    17.087558 |
| FlightMiles     |   111.573733 |
| FlightTrans     |     0.344470 |
| DaysSinceEnroll |  2840.822581 |

</td>
<td>

|                 |            x |
|:----------------|-------------:|
| Balance         | 3.625591e+04 |
| QualMiles       | 2.511177e+00 |
| BonusMiles      | 2.264788e+03 |
| BonusTrans      | 2.973174e+00 |
| FlightMiles     | 1.193219e+02 |
| FlightTrans     | 4.388972e-01 |
| DaysSinceEnroll | 3.060081e+03 |

</td>
</tr>
</tbody>
</table>

- We can observe that for Cluster1, the average of the DaysSinceEnroll
  values is the highest across all the clusters. However, the QualMiles
  variable is the lowest across them all as well. Consequently, we can
  safely assume that the customers in Cluster1 are loyal bu infrequent
  customers.

- Cluster2 has the largest average values in all three of QualMiles,
  FlightMiles, and FlightTrans. which indicates that the customers of
  this cluster have accumulated the largest amount of miles and these
  are mostly accumulated through flight transactions.

- On the other hand, Cluster3 has the highest mean values of Balance,
  BonusMiles and BonusTrans, indicating that the customers of this
  cluster have accumulated large amount of miles, only this time through
  non-flight transactions.

- Cluster4 has no significant value compared to the other clusters.
  However, the customers seems to relatively new customers and they’re
  accumulating miles through non-flight transactions.

- Finally, Cluster5 has relatively new customers who don’t use the
  airlines very often.

### K-means Clustering

``` r
set.seed(123)
kmcut = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
```

``` r
kable(table(kmcut$cluster))
```

| Var1 | Freq |
|:-----|-----:|
| 1    |   57 |
| 2    | 1650 |
| 3    |  143 |
| 4    |  776 |
| 5    | 1373 |

``` r
x = lapply(split(airlines, kmcut$cluster), colMeans)
kable(x)
```

<table class="kable_wrapper">
<tbody>
<tr>
<td>

|                 |            x |
|:----------------|-------------:|
| Balance         | 1.140122e+05 |
| QualMiles       | 5.543333e+03 |
| BonusMiles      | 1.919668e+04 |
| BonusTrans      | 1.229825e+01 |
| FlightMiles     | 9.397719e+02 |
| FlightTrans     | 2.824561e+00 |
| DaysSinceEnroll | 3.872175e+03 |

</td>
<td>

|                 |            x |
|:----------------|-------------:|
| Balance         | 3.815031e+04 |
| QualMiles       | 3.438424e+01 |
| BonusMiles      | 6.745658e+03 |
| BonusTrans      | 7.638182e+00 |
| FlightMiles     | 1.796448e+02 |
| FlightTrans     | 5.551515e-01 |
| DaysSinceEnroll | 2.283476e+03 |

</td>
<td>

|                 |            x |
|:----------------|-------------:|
| Balance         | 191736.33566 |
| QualMiles       |    471.56643 |
| BonusMiles      |  33093.33566 |
| BonusTrans      |     28.35664 |
| FlightMiles     |   5763.13287 |
| FlightTrans     |     16.76923 |
| DaysSinceEnroll |   4666.41259 |

</td>
<td>

|                 |            x |
|:----------------|-------------:|
| Balance         | 1.528793e+05 |
| QualMiles       | 7.798711e+01 |
| BonusMiles      | 5.100809e+04 |
| BonusTrans      | 2.131572e+01 |
| FlightMiles     | 4.799072e+02 |
| FlightTrans     | 1.457474e+00 |
| DaysSinceEnroll | 4.915534e+03 |

</td>
<td>

|                 |            x |
|:----------------|-------------:|
| Balance         | 5.741614e+04 |
| QualMiles       | 5.510415e+01 |
| BonusMiles      | 8.756787e+03 |
| BonusTrans      | 9.101238e+00 |
| FlightMiles     | 2.135805e+02 |
| FlightTrans     | 6.460306e-01 |
| DaysSinceEnroll | 5.826598e+03 |

</td>
</tr>
</tbody>
</table>

``` r
kable(table(kmcut$cluster, clusterGroups))
```

|   1 |   2 |   3 |   4 |   5 |
|----:|----:|----:|----:|----:|
|   0 |  57 |   0 |   0 |   0 |
|  30 |  99 |   1 | 552 | 968 |
|   0 | 137 |   6 |   0 |   0 |
|  23 | 114 | 444 | 194 |   1 |
| 723 | 112 |  43 | 122 | 373 |

As we can see above, the cluster groups are not particularly in order.
In fact, in clustering order doesn’t have any meaning. For example, the
k-means cluster1 is most similar to cluster1 of the previous model and
kmeans cluster2 is most similar to the 5th one in the previous model,
and so on.
