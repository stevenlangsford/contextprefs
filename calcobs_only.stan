functions{
  vector get_ordtheta(real a, real b, real tolerance){
    real p_err; 
    real difference;
    vector[3] theta;
    p_err=.01;
    difference = a-b;
     
    if(fabs(difference)<tolerance){
        theta[1]=p_err/3;
        theta[2]=1-p_err+p_err/3;
        theta[3]=p_err/3;
    }else if(difference < 0){
        theta[1]=1-p_err+p_err/3;
        theta[2]=p_err/3;
        theta[3]=p_err/3;
    }else{
        theta[1]=p_err/3;
        theta[2]=p_err/3;
        theta[3]=1-p_err+p_err/3;
    }
    return theta;
  }
}

data{
  int<lower=0> N;//total observations
  int K; //options per trial. K=3 is hardwired into the model for now. Oh well.  
  //This is the AGENT-ONLY version: it gets given observations, and tries to infer the value of each option from those.
  vector[K] calc_observations[N];
  int ord_observations_prob[N,3]; //[N, #pairs formable from K options]
  int ord_observations_payoff[N,3];
}

parameters{
  //this model is aware of its own calc_sd etc values, just uncertain about the option values.
  vector<lower=0,upper=1>[K] prob[N];
  vector[K] payoff[N];
}

transformed parameters{
  //There are more params than these in the full model: assuming alpha=1 and and U() is identity (for now)
  real calc_sd = 3;
  //  real p_err = .01; //set in ordrelation function.
  real tolerance_prob = .011; //tolerance vals from pg 374 LHS point 3.
  real tolerance_payoff = 1.1;
}

model{  
  for(i in 1:N){  
    for(j in 1:K){ //for each option j of trial i:
      prob[i,j]~beta(1,1); //priors on prob and payoff
      payoff[i,j]~normal(100,5);
      
      calc_observations[i,j]~normal(prob[i,j]*payoff[i,j],calc_sd);//eq4 without alpha or U().
    }//end for option j

    //ord observation
    /* ord_observations_prob[i,1] ~ categorical(get_ordtheta(prob[i,1], prob[i,2], tolerance_prob)); */
    /* ord_observations_prob[i,2] ~ categorical(get_ordtheta(prob[i,1],prob[i,3],tolerance_prob)); */
    /* ord_observations_prob[i,3] ~ categorical(get_ordtheta(prob[i,2],prob[i,3],tolerance_prob)); */

    /* ord_observations_payoff[i,1] ~ categorical(get_ordtheta(payoff[i,1], payoff[i,2], tolerance_payoff)); */
    /* ord_observations_payoff[i,2] ~ categorical(get_ordtheta(payoff[i,1],payoff[i,3],tolerance_payoff)); */
    /* ord_observations_payoff[i,3] ~ categorical(get_ordtheta(payoff[i,2],payoff[i,3],tolerance_payoff)); */
   
  }//end for trial i  
}//end model block
