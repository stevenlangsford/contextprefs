data{
  int hm_trials; //hm_trials
  int hm_ppnts;
  int ppntid[hm_trials];

  real trueA1[hm_trials];
  real trueB1[hm_trials];
  real trueA2[hm_trials];
  real trueB2[hm_trials];
  real trueA3[hm_trials];
  real trueB3[hm_trials];
  
  real<lower=0,upper=1> k[hm_ppnts]; //k is known to agent
  real<lower=0> calcsd[hm_ppnts]; //calcsd is known to agent.
}

transformed data{
  //better (obligatory?) to infer these in agent recovery?
  //On the other hand, the extra degree of flexibility might be bad?
  real impression1[hm_trials];
  real impression2[hm_trials];
  real impression3[hm_trials];
  
  for(i in 1:hm_trials){
    impression1[i] = normal_rng((1-k[ppntid[i]])*trueA1[i]+k[ppntid[i]]*trueB1[i],calcsd[ppntid[i]]);
    impression2[i] = normal_rng((1-k[ppntid[i]])*trueA2[i]+k[ppntid[i]]*trueB2[i],calcsd[ppntid[i]]);
    impression3[i] = normal_rng((1-k[ppntid[i]])*trueA3[i]+k[ppntid[i]]*trueB3[i],calcsd[ppntid[i]]); 
  }
}

parameters{
  real A1[hm_trials];
  real B1[hm_trials];
  real A2[hm_trials];
  real B2[hm_trials];
  real A3[hm_trials];
  real B3[hm_trials];
}

model{
  for(i in 1:hm_trials){
    A1[i]~normal(0,1);
    B1[i]~normal(0,1);
    A2[i]~normal(0,1);
    B2[i]~normal(0,1);
    A3[i]~normal(0,1);
    B3[i]~normal(0,1);
    
    impression1[i]~normal(
			 (1-k[ppntid[i]])*A1[i]+k[ppntid[i]]*B1[i],
			 calcsd[ppntid[i]]
			 );
    impression2[i]~normal(
			 (1-k[ppntid[i]])*A2[i]+k[ppntid[i]]*B2[i],
			 calcsd[ppntid[i]]
			 );
    impression3[i]~normal(
			 (1-k[ppntid[i]])*A3[i]+k[ppntid[i]]*B3[i],
			 calcsd[ppntid[i]]
			 );
    
  }
}

generated quantities{
  real estval1[hm_trials];
  real estval2[hm_trials];
  real estval3[hm_trials];
  for(i in 1:hm_trials){
    estval1[i]=(1-k[ppntid[i]])*A1[i]+k[ppntid[i]]*B1[i];
    estval2[i]=(1-k[ppntid[i]])*A2[i]+k[ppntid[i]]*B2[i];
    estval3[i]=(1-k[ppntid[i]])*A3[i]+k[ppntid[i]]*B3[i];
  }
}
