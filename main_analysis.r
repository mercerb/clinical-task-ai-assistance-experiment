"
Entry point to the analysis.
This is the only file that needs to be run.
It loads the packages and data, runs the analysis,
and runs the validation for that analysis.
"

# a priori power analysis first
source("power_analysis/power_graph_vary_n.r")

# load files and functions
source("load_data.r")
source("utils.r")
source("outcome_analysis_summary.r")
source("outcome_analysis_consistency.r")
source("outcome_analysis.r")
source("time_t_test.r")
source("bootstrap_analysis.r")
source("time_as_complexity.r")

# make image directories
for(image_dir in c("images_normal", "images_test_good", "images_test_bad", "images_test_random")) {
    dir.create(image_dir, showWarnings = FALSE)
}

# analyze actual data
ala_exp_data_normal <- load_all_data()
run_outcome_summary(ala_exp_data_normal, "images_normal")
run_outcome_consistency(ala_exp_data_normal, "images_normal")
run_outcome_analysis(ala_exp_data_normal, "images_normal")
run_time_analysis(ala_exp_data_normal, "images_normal")
run_bootstrap_analysis(ala_exp_data_normal)
run_time_complexity_analysis(ala_exp_data_normal)

# validate analysis with known "good" dataset
ala_exp_data_test_good <- load_all_data(data_source = "test_good")
run_outcome_summary(ala_exp_data_test_good, "images_test_good")
run_outcome_consistency(ala_exp_data_test_good, "images_test_good")
run_outcome_analysis(ala_exp_data_test_good, "images_test_good")
run_time_analysis(ala_exp_data_test_good, "images_test_good")

# validate analysis with known "bad" dataset
ala_exp_data_test_bad <- load_all_data(data_source = "test_bad")
run_outcome_summary(ala_exp_data_test_bad, "images_test_bad")
run_outcome_consistency(ala_exp_data_test_bad, "images_test_bad")
run_outcome_analysis(ala_exp_data_test_bad, "images_test_bad")
run_time_analysis(ala_exp_data_test_bad, "images_test_bad")

# validate analysis with known "random" dataset
ala_exp_data_test_random <- load_all_data(data_source = "test_random")
run_outcome_summary(ala_exp_data_test_random, "images_test_random")
run_outcome_consistency(ala_exp_data_test_random, "images_test_random")
run_outcome_analysis(ala_exp_data_test_random, "images_test_random")
run_time_analysis(ala_exp_data_test_random, "images_test_random")
