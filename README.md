optimhelp
=========

``` r
library(optimhelp)
cl <- log_par("CL", 1.2)
vc <- log_par("VC", 22.3)
p <- new_pars(cl,vc)
```

``` r
p
```

    .    name value
    . CL   CL   1.2
    . VC   VC  22.3

``` r
start.values <- trans(p)
start.values
```

    .        CL        VC 
    . 0.1823216 3.1045867

``` r
untrans(p,start.values)
```

    .   CL   VC 
    .  1.2 22.3
