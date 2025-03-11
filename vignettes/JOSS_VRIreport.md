---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'ViroReportR: An R package for automated short-term forecasting and report generation of viral respiratory infections'
tags:
  - R
  - forecasting
  - viral respiratory infections
  - covid
  - rsv
  - influenza
  - reproductive number
authors:
  - name: Nirupama Tamvada
    affiliation: "1" # (Multiple affiliations must be quoted)
  - name: Rebeca Cardim Falcao
    orcid: 0000-0001-8793-9675
    affiliation: "1, 2"
  - name: Ceasar Wong
    affiliation: 1
  - name: Mike Irvine
    orcid: 0000-0003-4785-8998
    affiliation: "1, 2"
affiliations:
 - name: BC Centre for Disease Control
   index: 1
 - name: School of Population and Public Health, University of British Columbia
   index: 2
citation_author: Tamvada et. al.
date: 13 March 2025
year: 2025
bibliography: paper.bib
output: 
  rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

Viral respiratory infections have established community transmission happening year-round or in seasonal patterns. Forecasting tools are essential for efficient surveillance and effective public health policy development. This package was built to support BC public health decision makers and surveillance teams by providing them with the latest forecast of the number of viral respiratory infections for common virus, such as Sars-CoV-2, Influenza A and Respiratory Syncytial Virus. Given how widespread is these virus, and the Covid-19 pandemic, we decided to upload this package to CRAN and release the methodology publicaly. This R package provides an automated short-term forecasting and report generation of viral respiratory infections. Using Cori et al.'s Rt estimation methods [@cori2013new], it provides real-time projections across different time horizons and disease types.

# Methodology

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{ if } x < 0\cr
1\textrm{ else}
\end{array}\right.$$

# Example Usage

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](https://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.


# Rendered R Figures

Figures can be plotted like so:


``` r
plot(1:10)
```

![](JOSS_VRIreport_files/figure-latex/unnamed-chunk-1-1.pdf)<!-- --> 

# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.

# References
