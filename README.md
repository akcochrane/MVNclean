<!-- README.md is generated from README.Rmd. Please edit that file -->

# MVNclean

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview of MVNclean

Outliers are a pervasive problem in many forms of data analysis, and
these problematic observations can be even more insidious as the number
of variables increases. Outliers may be univariate, multivariate, or
both [Leys et al., 2018](doi.org/10.1016/j.jesp.2017.09.011).

If you use the functions in published work, please this package using
the [Zenodo DOI](INSERT_LINK). Much appreciated!

## Function introductions

### YeoJohn: Yeo-Johnson Transformation

In cases wherein variables have univariate skew, monotonic
transformations such as the Yeo-Johnson transformation, which uses a
single parameter `lambda`, can be applied. The function `YeoJohn` finds
the optimal `lambda` for minimizing univariate skew on a trimmed vector
(e.g., after removing the highest and lowest 10% of values), and applies
that `lambda` to the entire untrimmed vector.

### RUW: Robust Univariate Winsorization

In cases wherein variables have univariate outliers, winsorization
replaces outlying values with values associated with somewhere
putatively “in-distribution.” The function `RUW` applies this univariate
winsorization to a vector using robust estimates of center (median) and
dispersion (asymmetric median absolute deviation),

### RMW: Robust Multivariate Winsorization

In cases where a dataset’s outliers are multivariate rather than
univariate, these outliers can be difficult to detect. The function
`RMW` identifies multivariate outliers using robust Mahalanobis distance
and then moves these outliers toward the multivariate centroid

### MVNclean: Multivariate Normal Cleaning

The function `MVNclean` applies the pipeline of `YeoJohn`, `RUW`, and
`RMW` to a dataset.

## Installing the package

The R package `devtools` includes a very easy way to install packages
from Github.

    devtools::install_github('akcochrane/MVNclean')
