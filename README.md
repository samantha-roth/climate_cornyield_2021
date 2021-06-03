# climate_cornyield_2021
Updated work on climate and corn yields

File Directory: /gpfs/group/kzk10/default/private/svr5482/Climate_CornYield-me/data_prep
1. First, run create_df_metdata.R. Uses the T_distribution function written by Haochen Ye. 
2. Run step_model_setup.R (format data for training a model with a step function of heat)
3. Run pwl_model_setup.R (format data for training a model with a piecewise linear function of heat)
4. Run normalize_step3data.R
5. Run reorder_by_year.R
