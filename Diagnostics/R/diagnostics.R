# Pnuemonia [T,F] -> X_Ray, Temperature
# Temperature [Normal]

# Visited_TB_Spot [T,F] -> Tuberculosis
# Tuberculosis [T,F] -> X_Ray
#
# Smokes [T,F] -> Lung_Cancer, Bronchitis
# Lung_Cancer [T,F] -> X_Ray, Dyspnea
# Bronchitis [T,F] -> Dyspnea
#
# X_Ray : [T,F]
# Dyspnea (Shortness of Breath): [T,F]


#' runDiagnostics
#'
#' The run function for the diagnostics project. You need to pass the two functions indicated in the parameters.
#'
#' Evaluation will be performed on new, randomly generated cases. Like here, your will get a MAE between your
#' estimates of your model and the true probabilities from the generative model (the model used to generate
#' the data).
#'
#' We will perform 1000 runs using the par function, which performs Metropolis within Gibbs MCMC sampling
#' on a network trained on the historical data using count parameters that start at one for the
#' categorical/conditional-categorical distributions, and a maximum likelihood approach for the normal
#' distribution. To pass your performance (MAE) need to be better than or equal to the worst performing of
#' these 1000 runs.
#'
#' All groups will be tested on the sampling that begin after the random seed is again set to a specific
#' value.
#'
#' Your code will also need to complete within 60 seconds. The par function using 1000 samples are completes
#' in about 22 seconds. If code runs slowly on the evaluation machine, you will code will need to complete
#' quicker than the time taken by the slowest of the 1000 runs plus 25%.
#'
#' If you want to load the historical cases (training data) or test cases (test data) use data(hist) or data(cases). See information
#' about this data in the help documentation (?hist or ?Diagnostics::hist)
#'
#' @param learn Your function which will create the network. It should accept as a single
#' argument the historical cases as returned by the Get_Historical_Data function.
#' @param diagnose Your function which will use the network created by your learn function
#' to estimate probabilities of unknown variables in a set of cases.
#' This function should take two arguments: (1) your network, as returned by your
#' learn function; and (2) The cases, as returned by the Get_Cases function.
#' @param verbose Controls the amount of output printed to console. 0 prints nothing. 1 prints your
#' time taken. 2 prints the time take plus the mean absolute error (MAE) of your estimates compared with the
#' true probabilities of the generative model, as well as the MAE of analytics solutions given the historic
#' data (with Bayesian expectation and maximum likelihood calculations) and the MAE of one precalculated run of
#' the par function using 1000 samples (as it will in the evaluation) and
#' one precalculated run of the par model using 5000 samples (to give you an indication of how much
#' improvement additional samples will make).
#' @return The output from your diagnose function.
#' @export
runDiagnostics = function (learn,diagnose,verbose=0) {
  startTime=Sys.time()
  network=learn(hist)
  estimates=diagnose(network,cases)
  ground_truth_probabilities=Get_Cases_Analytic_Generative_Model_Probabilities()
  mae=compareEstimates(estimates,ground_truth_probabilities)
  endTime=Sys.time()
  if (verbose==2) {
    cat("\nYour mean absolute error (MAE):",mae)
    mae_hist_bayes=compareEstimates(Get_Cases_Analytic_Empirical_Probabilities_With_Bayesian(),ground_truth_probabilities)
    cat("\nThe MAE of an analytic solution using Bayesian expectation:",mae_hist_bayes)
    mae_hist_ml=compareEstimates(Get_Cases_Analytic_Empirical_Probabilities_Without_Bayesian(),ground_truth_probabilities)
    cat("\nThe MAE of an analytic solution using maximum likelihood:",mae_hist_ml)
    mae_mike_1000=compareEstimates(Get_Cases_Mikes_Model_1000(),ground_truth_probabilities)
    cat("\nThe MAE of (one run of) the par model:",mae_mike_1000)
    mae_mike_5000=compareEstimates(Get_Cases_Mikes_Model_5000(),ground_truth_probabilities)
    cat("\nThe MAE of (one run of) the par model using 5000 MCMC samples instead of 1000:",mae_mike_5000)
  }
  if (verbose>0)
    cat("\nTime taken:",as.numeric(endTime)-as.numeric(startTime),"seconds.")
  return(mae)
}
#' @keywords internal
compareEstimates=function(e1,e2){
  mean(abs(e1-e2))
}
#' @keywords internal
Get_Cases_Analytic_Generative_Model_Probabilities=function(){
  out=matrix(c(
    0.000, 0.043, 0.924, 0.139,
    1.000, 0.014, 0.013, 0.028,
    0.000, 0.749, 0.419, 0.164,
    0.000, 0.146, 0.038, 0.184,
    0.001, 0.314, 0.278, 0.025,
    0.000, 0.020, 0.979, 0.469,
    1.000, 0.004, 0.002, 0.028,
    0.000, 0.003, 0.001, 0.028,
    0.000, 0.003, 0.036, 0.184,
    1.000, 0.453, 0.573, 0.565),nrow=10,byrow=T)
  colnames(out)=c("Pn","TB","LC","Br")
  rownames(out)=1:10
  return(out)
}
#' @keywords internal
Get_Cases_Analytic_Empirical_Probabilities_With_Bayesian=function(){
  out=matrix(c(
    0.000, 0.053, 0.899, 0.160,
    1.000, 0.012, 0.009, 0.026,
    0.000, 0.791, 0.350, 0.171,
    0.000, 0.120, 0.043, 0.177,
    0.002, 0.322, 0.240, 0.025,
    0.000, 0.020, 0.979, 0.368,
    1.000, 0.004, 0.002, 0.026,
    0.000, 0.002, 0.001, 0.026,
    0.000, 0.002, 0.033, 0.177,
    1.000, 0.414, 0.544, 0.520),nrow=10,byrow=T)
  colnames(out)=c("Pn","TB","LC","Br")
  rownames(out)=1:10
  return(out)
}
#' @keywords internal
Get_Cases_Analytic_Empirical_Probabilities_Without_Bayesian=function(){
  out=matrix(c(
    0.000, 0.053, 0.899, 0.159,
    1.000, 0.012, 0.009, 0.026,
    0.000, 0.793, 0.353, 0.171,
    0.000, 0.115, 0.039, 0.177,
    0.002, 0.323, 0.239, 0.025,
    0.000, 0.020, 0.979, 0.368,
    1.000, 0.002, 0.001, 0.026,
    0.000, 0.002, 0.001, 0.026,
    0.000, 0.002, 0.033, 0.177,
    1.000, 0.444, 0.565, 0.513),nrow=10,byrow=T)
  colnames(out)=c("Pn","TB","LC","Br")
  rownames(out)=1:10
  return(out)
}
#' @keywords internal
Get_Cases_Mikes_Model_1000=function(){
  out=matrix(c(
    0, 0.054, 0.902, 0.169,
    1, 0.017, 0.008, 0.032,
    0, 0.791, 0.346, 0.176,
    0, 0.115, 0.042, 0.180,
    0, 0.319, 0.246, 0.027,
    0, 0.017, 0.984, 0.365,
    1, 0.005, 0.006, 0.025,
    0, 0.004, 0.000, 0.030,
    0, 0.003, 0.038, 0.164,
    1, 0.427, 0.540, 0.513),nrow=10,byrow=T)
  colnames(out)=c("Pn","TB","LC","Br")
  rownames(out)=1:10
  return(out)
}
#' @keywords internal
Get_Cases_Mikes_Model_5000=function(){
  out=matrix(c(
    0.000, 0.051, 0.900, 0.165,
    1.000, 0.012, 0.007, 0.024,
    0.000, 0.794, 0.350, 0.166,
    0.000, 0.116, 0.047, 0.183,
    0.002, 0.328, 0.236, 0.027,
    0.000, 0.023, 0.976, 0.364,
    1.000, 0.009, 0.005, 0.021,
    0.000, 0.003, 0.001, 0.027,
    0.000, 0.002, 0.031, 0.183,
    1.000, 0.418, 0.538, 0.519),nrow=10,byrow=T)
  colnames(out)=c("Pn","TB","LC","Br")
  rownames(out)=1:10
  return(out)
}
