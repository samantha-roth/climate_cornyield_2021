# climate_cornyield_2021
Updated work on climate and corn yields

Data Preparation:
File Directory: /gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/data_prep
1. Run create_df_metdata.R. Uses the T_distribution function written by Haochen Ye. 
2. Run step_model_setup.R (format data for training a model with a step function of heat), and run pwl_model_setup.R (format data for training a model with a piecewise linear function of heat).
4. Run create_timeloc_df.R (matrix of times and locations)
5. Run normalize_step3data.R and normalize_pwl_data.R
6. Run reorder_by_year.R

Models

Model for 1 year of data:
File directory: /gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/PICAR/run
1. Run step_yr1_generateSamples.R and pw_yr1_generateSamples.R
2. Run step_yr1_prelimPICAR.R and pw_yr1_prelimPICAR.R
File directory: /gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/bayesian_spacetime/linearspatialmodel_PICAR
3. Run step_yr1_lspmodel.R and pw_yr1_lspmodel.R

Model for multiple years of data
File directory: /gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/PICAR/run
1. Run step_multyr_generateSamples.R and pw_multyr_generateSamples.R
File directory: /gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/bayesian_spacetime/linearspatialmodel_PICAR
2. Run step_multyr_prelimPICAR.R and pw_multyr_prelimPICAR.R
3. Run step_multyr_lspmodel.R and pw_multyr_lspmodel.R
