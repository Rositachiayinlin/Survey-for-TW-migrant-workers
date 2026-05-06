================================================================================
Survey for Taiwanese Migrant Workers in Vietnam
================================================================================

Project Overview
--------------------------------------------------------------------------------
This repository contains the data and analysis code for the study:

"Taiwanese migrant workers in Vietnam: working conditions and health outcomes 
compared with non-migrant workers in Taiwan"

The study examines working conditions and health outcomes among Taiwanese 
migrant workers in Vietnam (n=434) compared with non-migrant workers in 
Taiwan (n=868), using a mixed-methods cross-sectional design.


Folders
--------------------------------------------------------------------------------

- script/
  R scripts for statistical analysis:
  
  1.0.Table 1.R                    - Descriptive statistics of migrant workers
  1.1 Internal consistency.R       - Cronbach's alpha for fatigue & distress scales
  2.0.Table 2.R                    - Comparison of migrant vs non-migrant workers
  2.1.Appendix p2 PSM result.R     - Propensity score matching diagnostics
  2.2.Appendix p3 Table 2 before PSM.R  - Pre-matching comparison
  3.0.Table 3.R                    - Adjusted ORs for health outcomes
  3.1.Appendix p4 Table 3 non-match.R   - Sensitivity analysis (unmatched)
  3.2.Appendix p5 Fatigue sensitivity.R - Quantile regression for fatigue
  3.3.Appendix p6 distress score sensitivity.R - Quantile regression for distress
  4.0.Table 4(VIF).R               - Logistic regression & multicollinearity check
  4.1.Model comparison and test for non-linearity.R - Non-linearity assessment
  4.2.Appendix p7.R                - Continuous distress score analysis
  4.3.Appendix p8.R                - Alternative abuse definitions
  4.4.Appendix p9.R                - Family status sensitivity analysis
  4.5.Appendix p10.R               - Sex-stratified analysis

- result/
  Output tables and figures generated from the analysis


Requirements
--------------------------------------------------------------------------------
- R version 4.4.3 or higher
- Required packages: tidyverse, MatchIt, cobalt, tableone, psych, quantreg


Contact
--------------------------------------------------------------------------------
For questions or data access requests, please contact the corresponding author.