data{
  int hm_trials;
  int hm_ppnts;
  
  real A1[hm_trials];
  real B1[hm_trials];
  real A2[hm_trials];
  real B2[hm_trials];

  int ppntid[hm_trials];
  int choice[hm_trials];
}

parameters{
  real<lower=0,upper=1> k[hm_ppnts];
  
  //derived values, could just-in-time these in the model? 

}

model{
  vector[2] optionvalues[hm_trials];

  for(i in 1:hm_ppnts){
    k[i]~beta(2,2);
  }
  
  for( i in 1:hm_trials){
    optionvalues[i,1] = (1-k[ppntid[i]])*A1[i]+k[ppntid[i]]*B1[i];
    optionvalues[i,2] = (1-k[ppntid[i]])*A2[i]+k[ppntid[i]]*B2[i];
    choice[i]~categorical_logit(optionvalues[i]);
      }
}
