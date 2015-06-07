minerva_relig_paper
===================

The process to recreate is:

1. Run ```casos_get_twitter_term_nets.py``` on the data
2. Run ```casos_get_news_term_nets.py``` on the data
3. Run  ```R/transform_raw_news_data.R``` which will save out ```result_data/full_news_data.rdata``` and ```results_data/news_data.rdata```
4. Run ```R/transform_raw_twitter_data.R```, which will save out  ```result_data/full_twitter_data.rdata``` and ```results_data/twitter_data.rdata```
5. Run ```R/get_simplified_nu_value.R```, which will save out ```result_data/final_data.rdata```. 

6. Run ```R/get_mu_v_estimate.R```, which will save out ```result_data/final_data_w_mu_v.rdata```.

7. Run ```R/get_final_eta_estimates.R``` which will save out a bunch of .rdata files, one for each of the samples we need.

8. All plots and tables from the paper are generated from ```R/final_analysis.R```. 