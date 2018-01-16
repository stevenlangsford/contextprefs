data{
    //this is a multi-agent version producing estValue{A,B,C} -> choices  
  int<lower=0> N;//total observations
  int<lower=0> hm_ppnts;
  int ppntid[N]; //used as index, must range in 1:hm_ppnts

  real calc_sd[hm_ppnts];
  real betawidth_prob[hm_ppnts];//used in ordobs tolerance priors
  real betawidth_payoff[hm_ppnts];
  
  vector[3] calc_observations[N];
  int ord_observations_prob[N,3]; //[N, #pairs formable from 3 options]
  int ord_observations_payoff[N,3];
}

parameters{
  //this model is aware of its own calc_sd etc decision process params, only uncertain about the option attribute values.
  vector<lower=0,upper=1>[3] prob[N];
  vector[3] payoff[N];

  vector[3] beta_prob[hm_ppnts]; //coeffs for multinomial regression behind ord observations. Per participant because tolerance may vary.
  vector[3] beta_payoff[hm_ppnts]; //one for each attribute because priors here set scale-specific tolerance levels.
}

model{
  for(ppnt in 1:hm_ppnts){
    for(k in 1:3){
      beta_prob[ppnt,k]~normal(0,betawidth_prob[ppnt]); //Encodes tolerance. Maybe try to normalize to get around scale differences for different attributes?
      beta_payoff[ppnt,k]~normal(0,betawidth_payoff[ppnt]);
    }
  }
  
  for(trial in 1:N){
    for(option in 1:3){
      prob[trial,option]~beta(1,1); //priors on prob and payoff
      payoff[trial,option]~normal(100,5);
      
      calc_observations[trial,option]~normal(prob[trial,option]*payoff[trial,option],calc_sd[ppntid[trial]]);//eq4 without alpha or U().
    }//end for each option

    //ord observation
    ord_observations_prob[trial,1] ~ categorical_logit(beta_prob[ppntid[trial]]*(prob[trial,1]-prob[trial,2]));
    ord_observations_prob[trial,2] ~ categorical_logit(beta_prob[ppntid[trial]]*(prob[trial,1]-prob[trial,3]));
    ord_observations_prob[trial,3] ~ categorical_logit(beta_prob[ppntid[trial]]*(prob[trial,2]-prob[trial,3]));

    ord_observations_payoff[trial,1] ~ categorical_logit(beta_payoff[ppntid[trial]]*(payoff[trial,1]-payoff[trial,2]));
    ord_observations_payoff[trial,2] ~ categorical_logit(beta_payoff[ppntid[trial]]*(payoff[trial,1]-payoff[trial,3]));
    ord_observations_payoff[trial,3] ~ categorical_logit(beta_payoff[ppntid[trial]]*(payoff[trial,2]-payoff[trial,3]));
  }//end for trial i  
}//end model block

generated quantities{
  vector[3] estValue[N];
  int choice[N];
  
  for(i in 1:N){
    for(j in 1:3){
    estValue[i,j] = prob[i,j]*payoff[i,j];
    }
    //ignores the possibility of a tie
    if(estValue[i,1]>estValue[i,2]&&estValue[i,1]>estValue[i,3])choice[i]=1;
    if(estValue[i,2]>estValue[i,1]&&estValue[i,2]>estValue[i,3])choice[i]=2;
    if(estValue[i,3]>estValue[i,1]&&estValue[i,3]>estValue[i,2])choice[i]=3;
  }
  
}
