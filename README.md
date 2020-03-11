# Reservoir Operations
Repo for managing reservoir operations with CONUS 2.0
Contains historical information as well as current information

## Installing
The priority flow functions are provided as an R package. To install it you will need to do the following:
1. To use R packages from GitHub you need to have the devtools package installed:
```
install.packages('devtools’)
library(devtools)
```

2. Next you can install the Reservoir Operations package:
```
install_github("hydroframe/Reservoir_Operations", subdir="ResOps")
library('ResOps’)
```
