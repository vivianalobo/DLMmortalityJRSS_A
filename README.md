# DLMmortalityJRSS_A
Bayesian Multivariate Approach to Subnational mortality graduation with Age-Varying Smoothness

##### Authors: Luiz F V Figueiredo, Viviana G R Lobo, Mariane B Alves and Thais C O Fonseca

This work presents a Bayesian dynamical model for the joint mortality graduation of a group of populations. An essential issue in subnational mortality probabilistic modelling is the lack or sparseness of information for some subpopulations considered in the analysis. For many countries, mortality data is severely limited, and approaches based on a single population model could result in a high level of uncertainty associated with the adjusted mortality tables.  Here, we recognize the interdependence within a group of mortality data and pursue the approach of pooling information across several curves that ideally share common characteristics, such as the influence of epidemics and large economic changes.  Our proposal considers multivariate Bayesian dynamical models with common parameters allowing for borrowing information across mortality tables. This will also allow for testing the hypothesis of convergence of several populations.  Instead of considering the usual temporal evolution of dynamical linear models (DLMs), we define the stochastic process as a function of age, inducing correlations that ensure the smoothness of mortality curves.   Furthermore, we employ discount factors, which are typical in DLMs, to regulate the smoothness of the fit, with varying discounting across the age domain. This ensures less smoothness at younger ages, while allowing for greater stability and slower changes in the mortality curve at adult ages.  This setup implies a trade-off between stability and adaptability of mortality graduation.  In particular, the discount parameter can be interpreted as a controller for the responsiveness of the fit for older age,  relative to new data. The estimation approach is fully Bayesian, accommodating all uncertainties in the modelling and prediction. To illustrate the effectiveness of our proposed model, we analyse the mortality dataset for males and females from England and Wales between 2010 and 2012, obtained from the Office for National Statistics database, across several scenarios. Moreover, in the context of simulated missing data, our approach demonstrated good properties and flexibility in the pooling of information from other tables where data are available or patterns are clearer.



### Prerequisites
The methodology utilises the dynamic linear mortality model, as implemented in the **BayesMortalityPlus** package, which we have developed.

``` r
require("BayesMortalityPlus")
```


