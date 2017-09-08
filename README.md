
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.1.1-6666ff.svg)](https://cran.r-project.org/) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/kotzeb0912)](https://cran.r-project.org/package=kotzeb0912) [![packageversion](https://img.shields.io/badge/Package%20version-1.0-orange.svg?style=flat-square)](commits/master)

[![Last-changedate](https://img.shields.io/badge/last%20change-2017--05--09-yellowgreen.svg)](/commits/master)

# spAReco
## An R package in support of publication, "Spatial Autoregressive Models for Statistical Inference from Ecological Data." 

#### Jay M. Ver Hoef<sup>a</sup>, Erin E. Peterson<sup>b</sup>, Mevin B. Hooten<sup>c</sup>, Ephraim M. Hanks<sup>d</sup>, and Marie-Josee Fortin<sup>e</sup>

#### <sup>a</sup>NOAA Fisheries (NMFS) Alaska Fisheries Science Center, Marine Mammal Laboratory
#### <sup>b</sup>ARC Centre for Excellence in Mathematical and Statistical Frontiers (ACEMS) and the Institute for Future Environments, Queensland University of Technology
#### <sup>c</sup>U.S. Geological Survey, Colorado Cooperative Fish and Wildlife Research Unit, Department of Fish, Wildlife, and Conservation Biology, Department of Statistics, Colorado State University
#### <sup>d</sup>Department of Statistics, The Pennsylvania State University
#### <sup>e</sup>Department of Ecology and Evolutionary Biology, University of Toronto

As a scientific work, and in keeping with common scientific practicies, I kindly request that you cite my research project and applicable publications if you use my work(s) or data in your publications or presentations. Additionally, I strongly encourage and welcome collaboration to promote use of these data in the proper context and scope.  The publication is currently submitted:

#### Ver Hoef, J. M., Peterson, E. E., Hooten, M. B., Hanks, E. M., and Fortin, M-J. Spatial Autoregressive Models for Statistical Inference from Ecological Data. Submitted to *Ecological Monographs*.


Abstract
-----------------

 Ecological data often exhibit spatial pattern, which can be modeled as autocorrelation. Conditional autoregressive (CAR) and simultaneous autoregressive (SAR) models are network-based models (also known as graphical models) specifically designed to model spatially autocorrelated data based on neighborhood relationships. We identify and discuss six different types of practical ecological inference using CAR and SAR models, including: 1) model selection, 2) spatial regression, 3) estimation of autocorrelation, 4) estimation of other connectivity parameters, 5) spatial prediction, and 6) spatial smoothing.  We compare CAR and SAR models, showing their development and connection to partial correlations.  Special cases, such as the intrinsic autoregressive model (IAR), are described.  CAR and SAR models depend on weight matrices, whose practical development uses neighborhood definition and row-standardization. Weight matrices can also include ecological covariates and connectivity structures, which we emphasize, but have been rarely used. Trends in harbor seals (\emph{Phoca vitulina}) in southeastern Alaska from 463 polygons, some with missing data, are used to illustrate the six inference types. We develop a variety of weight matrices and CAR and SAR spatial regression models are fit using maximum likelihood and Bayesian methods. Profile likelihood graphs illustrate inference for covariance parameters. The same data set is used for both prediction and smoothing, and the relative merits of each are discussed.  We show the nonstationary variances and correlations of a CAR model and demonstrate the effect of row-standardization. We include several take-home messages for CAR and SAR models, including 1) choosing between CAR and IAR models, 2) modeling ecological effects in the covariance matrix, 3) the appeal of spatial smoothing, and 4) how to handle isolated neighbors. We highlight several reasons why ecologists will want to make use of autoregressive models, both directly and in hierarchical models, and not only in explicit spatial settings, but also for more general connectivity models.

Installation
------------

Installation of this R data package is done through the `devtools::install_github()` function or by downloading the [source package from the latest release](https://github.com/jayverhoef/spAReco).

```
library("devtools")
install_github("jayverhoef/spAReco")
```

Run R Scripts
-------------

The knitr document used to create the manuscript can be found here on your computer system:

```
system.file("doc/spARecoEM2.Rnw", package = "spAReco")
```

which contains all of the R code embedded in the Latex manuscript.  Stripping out the R code with the "purl" command yields a pure R script, which can be found here:

```
system.file("doc/spAReco.R", package = "spAReco")
```

To run the whole script from within R use:

```
source(system.file("doc/spAReco.R", package = "spAReco"))
```

A pure Latex document can be found here:

```
system.file("doc/spAReco.tex", package = "spAReco")
```

-------------
##### Disclaimer

<sub>This repository is a scientific product and is not official communication of the Alaska Fisheries Science Center, the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All AFSC Marine Mammal Laboratory (AFSC-MML) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. AFSC-MML has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.</sub>
# spAReco
