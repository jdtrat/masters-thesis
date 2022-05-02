
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

  // define a function getCU that takes the magnitude of the specific option's
  // first outcome and probability of the first outcome (mag_a and prob_a),
  // the magnitude of the second outcome and probability of the second outcome (mag_b and prob_b),
  // the current gamma value for which to calculate the counterfactual utility, and the expected utility
  // of the other option, and returns the expected utility of the option
  real getCU(real mag_a, real prob_a, real mag_b, real prob_b, real gamma, real ue_other) {
    real cu;
    cu = (prob_a * (mag_a - (gamma * (mag_b + ue_other)))) + (prob_b * (mag_b - (gamma * (mag_a + ue_other))));
    return cu;
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

  real tau[num_subjects]; // define tau for each subject.

  // add chose_option1
  int<lower=-1, upper=1> chose_option1[num_subjects, max_prospects];

}

parameters {
  // Hyper(group)-parameters
  // _pr indicates prior
  vector[1] mu_pr;
  vector[1] sigma_pr;

  // Subject-level raw parameters
  vector[num_subjects] gamma_pr;
}

transformed parameters {

 vector<lower=0,upper=1>[num_subjects] gamma;

  // take a draw from standard normal and shift
  // means to be within the distribution
  for (i in 1:num_subjects) {
    gamma[i] = Phi_approx(mu_pr[1] + sigma_pr[1] * gamma_pr[i]);
  }
}

model {

  // Hyperparameters
  mu_pr     ~ normal(0, 1);
  sigma_pr  ~ normal(0, 0.2);

  // Individual parameter
  gamma_pr ~ normal(0, 1.0);

  for (i in 1:(num_subjects)) {

       for (j in 1:(prospects_per_subj[i])) {

        // UE changes for each prospect -- only dependent on magnitude/probability
        // UC changes for each prospect & gamma value -- dependent on subject and prospect
        real UE1;
        real UE2;
        real UC1;
        real UC2;
        real pChoose1;

        // determine expected and counterfactual utilities
        // for all prospects
         UE1 = getEV(x1[i,j], p1[i,j], x2[i,j], not_p1[i,j]);
         UE2 = getEV(x4[i,j], p2[i,j], x3[i,j], not_p2[i,j]);

         UC1 = getCU(x1[i,j], p1[i,j], x2[i,j], not_p1[i,j], gamma[i], UE2);

         UC2 = getCU(x4[i,j], p2[i,j], x3[i,j], not_p2[i,j], gamma[i], UE1);

         // Fit to the observed choice behavior
         chose_option1[i,j] ~ bernoulli_logit(tau[i] * (UC1 - UC2));

       }

     }

}

generated quantities {

  // population level
  real mu_gamma;

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

  mu_gamma = Phi_approx(mu_pr[1]);

  { // local section to save time and space

    for (i in 1:num_subjects) {
      log_lik[i] = 0;
      for (j in 1:(prospects_per_subj[i])) {

        real UE1;
        real UE2;
        real UC1;
        real UC2;
        real pChoose1;

        // determine expected and counterfactual utilities
        // for all prospects
        UE1 = getEV(x1[i,j], p1[i,j], x2[i,j], not_p1[i,j]);
        UE2 = getEV(x4[i,j], p2[i,j], x3[i,j], not_p2[i,j]);

        UC1 = getCU(x1[i,j], p1[i,j], x2[i,j], not_p1[i,j], gamma[i], UE2);

        UC2 = getCU(x4[i,j], p2[i,j], x3[i,j], not_p2[i,j], gamma[i], UE1);

        log_lik[i] += bernoulli_logit_lpmf(chose_option1[i,j] | (UC1 - UC2));

        // generate posterior prediction for current trial
        pred_choice[i,j] = bernoulli_logit_rng(tau[i] * (UC1 - UC2));

      }

    }

  }

}
