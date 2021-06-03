"final_dataset_for_ben_post_2017.csv"->fn
read.csv(fn)->x
sum(x$state=="CALIFORNIA",na.rm=TRUE)/nrow(x)



## > L
## $`1`
## $`1`$`2018`
## [1] 1.5e-01 6.5e-03 2.8e+01 1.3e+00 1.1e-01 1.9e-02 3.3e+03
## $`1`$`2019`
## [1] 1.4e-01 5.1e-03 2.5e+01 1.3e+00 6.8e-02 2.2e-02 4.8e+03
## $`2`
## $`2`$`2018`
## [1] 1.2e-01 5.5e-03 2.4e+01 1.0e+00 5.1e-02 1.5e-02 4.6e+03
## $`2`$`2019`
## [1]  1.2e-01  5.1e-03  2.2e+01  1.3e+00 -9.5e-03  2.3e-02  4.4e+03
## $`3`
## $`3`$`2018`
## [1] 1.0e-01 6.6e-03 1.9e+01 1.2e+00 5.5e-02 2.0e-02 3.4e+03
## $`3`$`2019`
## [1]  9.2e-02  4.8e-03  1.7e+01  1.2e+00 -1.1e-02  2.0e-02  4.7e+03
## $`4`
## $`4`$`2018`
## [1] 7.3e-02 7.9e-03 1.4e+01 1.3e+00 2.7e-02 1.9e-02 2.9e+03
## $`4`$`2019`
## [1] 7.4e-02 5.1e-03 1.3e+01 1.7e+00 3.4e-02 2.6e-02 4.3e+03
s1<-c(0.11,0.0511,0.0548,0.0683,-0.0095,-0.0114) ##slopes for spring 2019 grade 1:3, 2020 grade 1:3

##see F_fall.R
## > tab
##    [,1]  [,2]  [,3]   [,4]
## 1 0.180 0.011 0.181 0.0053
## 2 0.175 0.011 0.159 0.0057
## 3 0.128 0.012 0.115 0.0066
## 4 0.097 0.015 0.082 0.0079
s2<-c(0.175,0.128,0.097,0.159,0.115,0.082) ##slopes for fall 2019 2:4, 2020 grade 2:4

n<-s1*90+s2*90
(n[1]-n[4])/n[1]
(n[2]-n[5])/n[2]
(n[3]-n[6])/n[3]


## The accumulated learning loss for second and third graders would leave them 7.3 and
## 7.7 WPM behind their expected level respectively (representing 26 percent and 33 percent of the
## expected yearly gains).
##see e_parametric
