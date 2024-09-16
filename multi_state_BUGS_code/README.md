# Multi-State BUGS Code

## Overview

This folder contains BUGS code files used for Bayesian multi-state modelling in the study. These models facilitate the analysis and synthesis of mutiple outcomes comparing the effectiveness of two COVID-19 vaccines. The code includes both the base-case and node-splitting models used in the analyses.

## File Descriptions

- **`base_case_model.txt`**: Contains the BUGS code for the base-case multi-state model. This serves as the main analytical framework for synthesizing multiple outcomes in the study.
- **`node_splitting_model.txt`**: Contains the BUGS code for the node-splitting model, used to assess the consistency between different sources of evidence regarding treatment effects.

## Usage

To run these models, a BUGS-compatible software package like OpenBUGS or WinBUGS is required. The code files are designed to be executed using the R packages `R2OpenBUGS` or `R2WinBUGS`.

Ensure that the necessary data files and R scripts are set up, as outlined in the `/R_running_scripts/` folder, to run these models effectively.
