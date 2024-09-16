# COVID-19 Vaccine Data Description

## Background

The datasets in this folder were sourced from the study "Comparative Effectiveness of mRNA-1273 and BNT162b2 COVID-19 Vaccines Among Older Adults: A Systematic Review and Meta-Analysis Using the GRADE Framework". 

The preprint is available on the MedRxiv server and can be accessed [here](https://medrxiv.org/cgi/content/short/2023.11.21.23298832v1).

## Overview

This collection contains COVID-19 outcome data for individuals vaccinated with Moderna or Pfizer vaccines. The datasets provide relative comparison statistics between the two vaccines across various outcomes and are anonymised with unique index IDs.

## Outcome Measures

The data includes five outcome measures, reflecting increasing severity with smaller sample sizes for more severe outcomes:

1. **Infection**
2. **Symptomatic Infection**
3. **Severe Infection**
4. **Hospitalisation**
5. **Death**

## File Descriptions

- **`infection_data.csv`**: Data on COVID-19 infections.
- **`severe_infection_data.csv`**: Data on severe COVID-19 infections.
- **`symptomatic_infection_data.csv`**: Data on symptomatic COVID-19 infections.
- **`hospitalisation_data.csv`**: Data on hospitalisations due to COVID-19.
- **`death_data.csv`**: Data on deaths due to COVID-19.
- **`covid_outcome_r.csv`**: Observed COVID-19 outcomes for both vaccines, used in multi-state models.
- **`covid_total_n.csv`**: Total number of vaccinated individuals for each study, used in multi-state models.

## Column Descriptions

- **`study_id/author_id`**: Unique identifier for each study.
- **`age`**: Age group of the study population.
- **`interv_id1`**: Unique ID for the reference vaccine group.
- **`interv_id2`**: Unique ID for the comparison vaccine group.
- **`N1`**, **`N2`**: Sample sizes for the reference and comparison vaccine groups, respectively..
- **`n1`**, **`n2`**: Number of cases in each vaccine group.
