
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

  // add chose_option1
  int<lower=-1, upper=1> chose_option1[num_subjects, max_prospects];

}

parameters {
  // Hyper(group)-parameters
  // _pr indicates prior
  vector[3] mu_pr;
  vector[3] sigma_pr;

  // Subject-level raw parameters
  vector[num_subjects] rho_pr;
  vector[num_subjects] tau_pr;
  vector[num_subjects] gamma_pr;
}

transformed parameters {

 vector<lower=0,upper=2>[num_subjects] rho;
 vector<lower=0,upper=30>[num_subjects] tau;
 vector<lower=0,upper=1>[num_subjects] gamma;

  // take a draw from standard normal and shift
  // means to be within the distribution
  for (i in 1:num_subjects) {
    rho[i] = Phi_approx(mu_pr[1] + sigma_pr[1] * rho_pr[i]) * 2;
    tau[i] = Phi_approx(mu_pr[2] + sigma_pr[2] * tau_pr[i]) * 30;
    gamma[i] = Phi_approx(mu_pr[3] + sigma_pr[3] * gamma_pr[i]);
  }
}

model {

  // Hyperparameters
  mu_pr     ~ normal(0, 1);
  sigma_pr  ~ normal(0, 0.2);

  // Individual parameter
  rho_pr ~ normal(0, 1.0);
  tau_pr ~ normal(0, 1.0);
  gamma_pr ~ normal(0, 1.0);

  for (i in 1:(num_subjects)) {

       for (j in 1:(prospects_per_subj[i])) {

        // UE changes for each prospect -- only dependent on magnitude/probability
        // UC changes for each prospect & gamma value -- dependent on subject and prospect
        real UE1;
        real UE2;
        real UC1;
        real UC2;

        // determine expected utilities with the risk aversion (rho) parameter
        // for all prospects
         UE1 = ( p1[i,j] * pow(x1[i,j], rho[i]) ) + ( not_p1[i,j] * pow(x2[i,j], rho[i]) );
         UE2 = ( p2[i,j] * pow(x4[i,j], rho[i]) ) + ( not_p2[i,j] * pow(x3[i,j], rho[i]) );

         UC1 = getCU(pow(x1[i,j], rho[i]), p1[i,j], pow(x2[i,j], rho[i]), not_p1[i,j], gamma[i], UE2);
         UC2 = getCU(pow(x4[i,j], rho[i]), p2[i,j], pow(x3[i,j], rho[i]), not_p2[i,j], gamma[i], UE1);


         // Fit to the observed choice behavior
         chose_option1[i,j] ~ bernoulli_logit(tau[i] * (UC1 - UC2));

       }

     }

}

generated quantities {

  // population level
  real mu_rho;
  real mu_tau;
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

  mu_rho = Phi_approx(mu_pr[1]) * 2;
  mu_tau = Phi_approx(mu_pr[2]) * 30;
  mu_gamma = Phi_approx(mu_pr[3]);

  { // local section to save time and space

    for (i in 1:num_subjects) {
      log_lik[i] = 0;
      for (j in 1:(prospects_per_subj[i])) {

        real UE1;
        real UE2;
        real UC1;
        real UC2;

        // determine expected utilities with the risk aversion (rho) parameter
       // for all prospects
        UE1 = ( p1[i,j] * pow(x1[i,j], rho[i]) ) + ( not_p1[i,j] * pow(x2[i,j], rho[i]) );
        UE2 = ( p2[i,j] * pow(x4[i,j], rho[i]) ) + ( not_p2[i,j] * pow(x3[i,j], rho[i]) );

        UC1 = getCU(pow(x1[i,j], rho[i]), p1[i,j], pow(x2[i,j], rho[i]), not_p1[i,j], gamma[i], UE2);

        UC2 = getCU(pow(x4[i,j], rho[i]), p2[i,j], pow(x3[i,j], rho[i]), not_p2[i,j], gamma[i], UE1);

        log_lik[i] += bernoulli_logit_lpmf(chose_option1[i,j] | tau[i] * (UE1 - UE2));

        // generate posterior prediction for current trial
        pred_choice[i,j] = bernoulli_logit_rng(tau[i] * (UE1 - UE2));

      }

    }

  }

}
