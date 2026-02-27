---
title: 'ViroReportR: An R package for automated short-term forecasting and report generation of viral respiratory infections'
tags:
  - R
  - viral respiratory infections
  - short-term forecasting
  - epidemiology
  - automated reporting
authors:
  - name: Rebeca Cardim Falcao
    orcid: 0000-0000-0000-0000
    corresponding: true
    affiliation: "1, 2" # (Multiple affiliations must be quoted)
  - name: Nelson Tang
    affiliation: 1
  - name: Nirupama Tamvada
    affiliation: 1
  - name: Cesar Wong
    affiliation: 1
  - name: Michael A. Irvine
    orcid: 0000-0000-0000-0000
    affiliation: "1, 3"
affiliations:
 - name: BC Centre for Disease Control, Canada
   index: 1
 - name: University of British Columbia, Canada
   index: 2
 - name: Simon Fraser University, Canada
   index: 3
date: 13 August 2017
bibliography: paper.bib

---

**authors list and order not finalized**

*Your paper must include the following required sections:*
**Summary**: A description of the high-level functionality and purpose of the software for a diverse, non-specialist audience.

**Statement of need**: A section that clearly illustrates the research purpose of the software and places it in the context of related work. This should clearly state what problems the software is designed to solve, who the target audience is, and its relation to other work.

**State of the field**: A description of how this software compares to other commonly-used packages in the research area. If related tools exist, provide a clear “build vs. contribute” justification explaining your unique scholarly contribution and why existing alternatives are insufficient.

**Software design**: An explanation of the trade-offs you weighed, the design/architecture you chose, and why it matters for your research application. This should demonstrate meaningful design thinking beyond a superficial code structure description.

**Research impact statement**: Evidence of realized impact (publications, external use, integrations) or credible near-term significance (benchmarks, reproducible materials, community-readiness signals). The evidence should be compelling and specific, not aspirational.

**AI usage disclosure**: Transparent disclosure of any use of generative AI in the software creation, documentation, or paper authoring. If no AI tools were used, state this explicitly. If AI tools were used, describe how they were used and how the quality and correctness of AI-generated content was verified.*

# Summary

Viral respiratory infections have established community transmission happening yearround
or in seasonal patterns. Forecasting tools are essential for efficient surveillance
and effective public health policy development. This package was built to support BC
public health decision makers and surveillance teams by providing them with the latest
forecast of the number of viral respiratory infections for common virus, such as Sars-
CoV-2, Influenza A and Respiratory Syncytial Virus. Given how widespread is these
virus, and the Covid-19 pandemic, we decided to upload this package to CRAN and
release the methodology publicaly. This R package provides an automated short-term
forecasting and report generation of viral respiratory infections. Using Cori et al.’s Rt
estimation methods [@coriNewFrameworkSoftware2013] , it provides real-time
projections across different time horizons and disease types.

# Statement of need
Respiratory viruses are continuously contributing for outbreaks and epidemics, requiring a constant need for forecast of future cases. During and before respiratory seasons, these forecasts can support better public health response and planning. Communicating forecasts and model results is always challenging, and transparency is essential for build trust and capacity. ViroReportR tackles this issue by providing an automated forecast reporting tool for respiratory viruses, including Influenza A, SARS-CoV2 and Respiratory Syncytial. The final deliverable provides forecasts of future cases with the inclusion of model uncertainity and validation metrics. Therefore, ViroReportR provides a timely, transparent and reliably tool to support public heatlh respondes and epidemiologists with planning and capacity around respiratory viral infections.

# State of the field                                                                                                                  
...

# Software design

We want to provide a flexible and easy to use report generation tool, maintaining the overall structure of the report fixed. ViroReportR package works around a main function `generate_forecast_report()` that calls all other functions for data processing, fitting, forecasting, validation and render a final markdown file (`inst/vriforecasting_report.Rmd`).
The data input is fairly simple and consists of the daily counts of a respiratory virues. Given that jurisdictions might be insterested in different respiratory viruses, we allow users to add or remove viruses. All of this can be done by the data input. The required format of the data is 3 columns with `date`, `disease_type` and `confirm` (confirmed cases). If the user has added any additional viruses other than Influenza A, SARS-CoV2 and RSV, they will also need to add serial interval distribution parameters for those viruses. The overall text and explanations on the report are not dependent on results. Adding new virus provides more tabs, plots and summary tables on the final html report.  In addition, some respiratory viruses follow a clear respiratory season, whereas others may not. Thus, we include an option for users to provide seasonal period for each virus if needed. Currently, we provide estimations using the `EpiEstim` packages [@coriNewFrameworkSoftware2013], but we design `ViroReportR` to allow for inclusion of other model techniques in the future. 


# Research impact statement
Currently, ViroReportR is being deployed in pipeline for internal use within BCCDC. 

# AI usage disclosure
...

# Acknowledgements
The authors ...

# References