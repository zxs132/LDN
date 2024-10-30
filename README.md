# R package: LDN

Title: Longitudinal dependency network modeling and visualization

Version: 1.0

Date: 2024-10-27

Author: Juan Kim

Maintainer: Juan Kim <zxs132@yuhs.ac>

Description: This sofware package provides facilities for LDN for modeling and visualizing predictive relationships between variables in longitudinal data

NeedsCompilation: No

Depends: R(>= 3.4.1)

Imports: reshape, dplyr, ranger, igraph, visNetwork, ztable, grDevices

License: GPL-2

URL: https://github.com/zxs132/LDN

## Installation

```
devtools::install_github("zxs132/LDN")
```

## Example

```
library(LDN)
test_data <- read.csv("test_data.csv")
test1 <- data_reshape(test_data)
test2 <- compute_test(test1) # Takes time...
visualize(test2)
```

