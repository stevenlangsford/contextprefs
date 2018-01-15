data{
  int<lower=0> N;//total observations
  int K; //options per trial. K=3 is hardwired into the model for now. Oh well.  
  //This is the AGENT-ONLY version: it gets given observations, and tries to infer the value of each option from those.
  vector[K] calc_observations[N];
  int ord_observations_prob[N,3]; //[N, #pairs formable from K options]
  int ord_observations_payoff[N,3];
}

parameters{
  //this model is aware of its own calc_sd etc decision process params, only uncertain about the option attribute values.
  vector<lower=0,upper=1>[K] prob[N];
  vector[K] payoff[N];

  vector[K] beta_prob; //coeffs for multinomial regression behind ord observations.
  vector[K] beta_payoff; //one for each attribute because priors here set scale-specific tolerance levels.
}

transformed parameters{
  //There are more params than these in the full model: assuming alpha=1 and and U() is identity (for now)
  real calc_sd = 3;

  //These ord-obs parameters are now wrapped into the priors on beta in the multinomial regression (which gives low tolerances if the prior is wide, ie large coeffs expected, and high tolerance if the prior is narrow/small coeffs expected.)
  //  real p_err = .01; 
  /* real tolerance_prob = .011;  */
  /* real tolerance_payoff = 1.1; */
}

model{
  for(k in 1:K){
    beta_prob[k]~normal(0,1); //Encodes tolerance. Maybe try to normalize to get around scale differences for different attributes?
    beta_payoff[k]~normal(0,5);
  }
  
  for(i in 1:N){  
    for(j in 1:K){ //for each option j of trial i:
      prob[i,j]~beta(1,1); //priors on prob and payoff
      payoff[i,j]~normal(100,5);
      
      calc_observations[i,j]~normal(prob[i,j]*payoff[i,j],calc_sd);//eq4 without alpha or U().
    }//end for option j

    //ord observation
    ord_observations_prob[i,1] ~ categorical_logit(beta_prob*(prob[i,1]-prob[i,2]));
    ord_observations_prob[i,2] ~ categorical_logit(beta_prob*(prob[i,1]-prob[i,3]));
    ord_observations_prob[i,3] ~ categorical_logit(beta_prob*(prob[i,2]-prob[i,3]));

    ord_observations_payoff[i,1] ~ categorical_logit(beta_payoff*(payoff[i,1]-payoff[i,2]));
    ord_observations_payoff[i,2] ~ categorical_logit(beta_payoff*(payoff[i,1]-payoff[i,3]));
    ord_observations_payoff[i,3] ~ categorical_logit(beta_payoff*(payoff[i,2]-payoff[i,3]));
  }//end for trial i  
}//end model block

generated quantities{
  vector[3] estValue[N];
  for(i in 1:N){
    for(j in 1:3){
    estValue[i,j] = prob[i,j]*payoff[i,j];
    }
  }
}
