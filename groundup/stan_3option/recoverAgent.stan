data{
  int hm_trials;
  int hm_ppnts;
  
  real A1[hm_trials]; //a real programmer would pass i_options, j_attributes and make this a matrix[i,j].
  real B1[hm_trials];
  real A2[hm_trials];
  real B2[hm_trials];
  real A3[hm_trials];
  real B3[hm_trials];
  
  int ppntid[hm_trials];
  int choice[hm_trials];
}

parameters{
  real<lower=0,upper=1> k[hm_ppnts];
}

model{
  vector[3] optionvalues[hm_trials];

  for(i in 1:hm_ppnts){
    k[i]~beta(2,2);
  }
  
  for( i in 1:hm_trials){
    optionvalues[i,1] = (1-k[ppntid[i]])*A1[i]+k[ppntid[i]]*B1[i];
    optionvalues[i,2] = (1-k[ppntid[i]])*A2[i]+k[ppntid[i]]*B2[i];
    optionvalues[i,3] = (1-k[ppntid[i]])*A3[i]+k[ppntid[i]]*B3[i];
    choice[i]~categorical_logit(optionvalues[i]);
      }
}
