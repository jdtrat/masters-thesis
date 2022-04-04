
functions {

  // define a function getEV that takes the magnitude of the option's
  // first outcome and probability of the first outcome (mag_a and prob_a),
  // the magnitude of the second outcome and probability of the second outcome (mag_b and prob_b),
  // and returns the expected utility of the option

  real getEV(real mag_a, real prob_a, real mag_b, real prob_b) {
    real eu;
    eu = (mag_a * prob_a) + (mag_b * prob_b);
    return eu;
  }

}

data {
  int<lower=0> max_prospects; // define number of prospects
  int<lower=0> num_subjects; // define number of "subjects"
  int<lower=0, upper=max_prospects> prospects_per_subj[num_subjects]; // define the number of prospects per subject

  // define outcomes
  real x1[num_subjects, max_prospects];
  real x2[num_subjects, max_prospects];
  real x3[num_subjects, max_prospects];
  real x4[num_subjects, max_prospects];

  // define probabilities
  real<lower=0, upper=1> p1[num_subjects, max_prospects];
  real<lower=0, upper=1> not_p1[num_subjects, max_prospects];
  real<lower=0, upper=1> p2[num_subjects, max_prospects];
  real<lower=0, upper=1> not_p2[num_subjects, max_prospects];

  // add chose_option1
  int<lower=-1, upper=1> chose_option1[num_subjects, max_prospects];

}

parameters {
  // Hyper(group)-parameters
  // _pr indicates prior
  vector[1] mu_pr;
  vector[1] sigma_pr;

  // Subject-level raw parameters
  vector[num_subjects] rho_pr;
}

transformed parameters {

 vector<lower=0,upper=2>[num_subjects] rho;

  // take a draw from standard normal and shift
  // means to be within the distribution
  for (i in 1:num_subjects) {
    rho[i] = Phi_approx(mu_pr[1] + sigma_pr[1] * rho_pr[i]) * 2;
  }
}

model {

  // Hyperparameters
  mu_pr[1]     ~ normal(0, 1);
  sigma_pr[1]  ~ normal(0, 0.2);

  // Individual parameter
  rho_pr ~ normal(0, 1.0);

  for (i in 1:(num_subjects)) {

       for (j in 1:(prospects_per_subj[i])) {

        real UE1;
        real UE2;
        real pChoose1;

        // determine expected utilities with the risk aversion (rho) parameter
        // for all prospects
         UE1 = pow(getEV(x1[i,j], p1[i,j], x2[i,j], not_p1[i,j]), rho[i]);
         UE2 = pow(getEV(x4[i,j], p2[i,j], x3[i,j], not_p2[i,j]), rho[i]);

         // Fit to the observed choice behavior
         chose_option1[i,j] ~ bernoulli_logit((UE1 - UE2));

       }

     }

}

generated quantities {

  // population level
  real mu_rho;

  // log likelihood
  real log_lik[num_subjects];

  // posterior predictive checks
  real pred_choice[num_subjects, max_prospects];

  // set all posterior predictions
  // to -1 to avoid NULL values
  for (i in 1:num_subjects) {
    for (j in 1:max_prospects) {
      pred_choice[i,j] = -1;
    }
  }

  mu_rho = Phi_approx(mu_pr[1]) * 2;

  { // local section to save time and space

    for (i in 1:num_subjects) {
      log_lik[i] = 0;
      for (j in 1:(prospects_per_subj[i])) {

       real UE1;
       real UE2;
       real pChoose1;

       // determine expected utilities with the risk aversion (rho) parameter
       // for all prospects
        UE1 = pow(getEV(x1[i,j], p1[i,j], x2[i,j], not_p1[i,j]), rho[i]);
        UE2 = pow(getEV(x4[i,j], p2[i,j], x3[i,j], not_p2[i,j]), rho[i]);

        log_lik[i] += bernoulli_logit_lpmf(chose_option1[i,j] | (UE1 - UE2));

        // generate posterior prediction for current trial
        pred_choice[i,j] = bernoulli_logit_rng((UE1 - UE2));

      }

    }

  }

}
