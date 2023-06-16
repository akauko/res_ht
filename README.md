# res_ht
Code for the project: 
[Ebinger, J.E. et al., Apparent Treatment Resistant Hypertension Associated Lifetime Cardiovascular Risk in a Longitudinal 
National Registry. Eur. J. Prev. Cardiol. (2023) accepted.](https://doi.org/10.1093/eurjpc/zwad066)


* Data: FinnGen https://www.finngen.fi/en
* Description of variables: https://risteys.finregistry.fi/
* Description of registries: https://finngen.gitbook.io/finngen-analyst-handbook/finngen-data-specifics/finnish-health-registers-and-medical-coding
* Data access: https://site.fingenious.fi/en/

```
res_ht
├── README.md                    # Overview
├── res_ht5_ses.rmd              # R markdown for the main analysis
├── res_ht5_diursublc.rmd        # Sensitivity analysis with changed medication class definition
├── ATC_codes_r8.csv             # Medication class definitions for the main analysis
├── ATC_codes_r8_diursubcl.csv   # Medication class definitions for the sensitivity analysis
├── scripts
  ├── gen_res_ht_pre55.R           # Calculates a moving sum of drug purchases, required by the main script
  ├── gen_res_ht_pre55_diursubcl.R # Calculates a moving sum of drug purchases, required by the sensitivity analysis
  ├── functions.R                  # Minor R functions for the analysis
  ├── select columns.pl            # Perl script to select columns from a tsv file by column names
  ├── fg_pheno_cols.txt            # List of variables that are extracted from the FinnGen phenotype file

```
