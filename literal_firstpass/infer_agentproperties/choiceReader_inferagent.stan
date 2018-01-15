functions{
  real choosemax_log(int x, real a, real b, real c){
    //Oh, so discrete. sorry Stan.
    real p_err;//should push this as low as is feasible for fidelity to howes16?
    p_err=.01;
    if(x==1){
      if(a>b&&a>c) return log(1-p_err);
      else return log(p_err);
    }
    if(x==2){
      if(b>a&&b>c) return log(1-p_err);
      else return log(p_err);
    }
    if(x==3){
      if(c>a&&c>b) return log(1-p_err);
      else return log(p_err);
    }
  }//end ordrelation_log
  
  
  vector get_ordtheta(real a, real b, real tolerance, real p_err){
    real difference;
    vector[3] theta;
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
  int<lower=0> hm_ppnts;
  int<lower=1, upper=hm_ppnts> ppntID[N]; //must run from 1 to hm_ppnts, because it's used to index into calcsd etc ppnt parameter arrays.
  int choice[N];
  real true_prob[N,3];
  real true_payoffA[N,3];  
}//Model params: (these are almost the Wedell ones, but normal instead of t(df=100). Note ord_err=0, U is identity. Paper varies calc_sd
//environment params
var env_a = 1;
var env_b = 1; //probs distributed as beta(env_a,env_b)
var env_mu = 19.6;
var env_sigma = 8.08; //payoffs distributed as gaussian(env_mu,env_sigma)
//calc obs params
var alpha = 1; //weights probability (as exponent)
var U = function(x){return x}; //function(x){return Math.log(x)}; //converts payoff value to subjective utility. Log seems attractive... other options?
var calc_sd = 4; //noise level. Sensible range depends on both env and U.
//Ord obs params
var prob_tolerance = .011; //from pg7 LHS summary.
var payoff_tolerance = 1.1; //ordObs doesn't apply U first, as in calcObs, maybe it should?
var ord_err = 0;//.2;
//end params


//environment setup
var getOption = function(){ //sets convention： Option = [prob,payoff]
    return [
	beta({a:env_a,b:env_b}),
	gaussian({mu:env_mu,sigma:env_sigma})
	]
}
var getTrial = function(){
  return repeat(3, getOption);
}

var expectedValue = function(anOption){ //helper, to compare against.
    return anOption[0]*anOption[1];
}



//model
//calculation Observation
var calcObs = function(anOption){
  //params
    //derived:
    var noise = gaussian({mu:0,sigma:calc_sd});
    //observed values
    var prob = anOption[0];
    var payoff = anOption[1];
    //calculation observation M (eq4)
    return Math.pow(prob, alpha)*U(payoff)+noise;
}


//ordinal Observation
var ordObs = function(arr){ 
//setup helper, so you can have a prob comparator and a payoff comparator and a whatever else comparator DRY.
    var getComparator = function (getAttribute,tolerance){
	return function(objA,objB){
	    if(flip(ord_err)) return categorical({vs:["<","=",">"]}); //flat rate of uniform error. If comparisons were perfect, underlying values noisy, err scales with distance, might be fun to look at?
	    var a = getAttribute(objA);
	    var b = getAttribute(objB);
	    if(Math.abs(a-b)<tolerance) return "=";
	    if(a>b) return ">";
	    if(a<b) return "<";
	}
    }
    
    //tweakable, but this setup here is specific to [prob, payoff] gambles.
    var compare_prob = getComparator(function(x){x[0]},prob_tolerance); //getters match convention that options are [prob, payoff]
    var compare_payoff = getComparator(function(x){x[1]},payoff_tolerance);
    var attributes = [compare_prob, compare_payoff]; //List of all the things you want to compare on: may change depending on stim.
    //end setup.
    
    //walk through all pairs. Assumes comparison direction doesn't matter, which is not true in general.
    var getpairs = function(tomatch, remaining,comparefn){
    if(remaining.length==0) return [];
	map(function(x){comparefn(tomatch[0],x)}, remaining).concat(getpairs(tomatch.slice(1,tomatch.length),remaining.slice(1,remaining.length),comparefn))
    }

    //for each comparison listed in 'attributes', make that comparison on all pairs in arr.
    return map(function(cmp_fn){getpairs(arr,arr.slice(1,arr.length),cmp_fn)},attributes);
}
//end model

//Check implications of the model re context-sensitive preference:
var decoyImpact = function(A,B,decoy){ //where A, B, and decoy are Options, ie, [prob,payoff]. Returns [choseA(AB), choseA(ABC)]
console.log(".");//poorman's progress bar
//one noisy impression of the gamble each time this runs. model accesses the whole impression, model_2option just the A,B parts.
    var ord = ordObs([A,B,decoy]);
    var calc = map(calcObs,[A,B,decoy]);
    
//Inference functions (reference the impression)
var model = function(){
    //priors, reflect environment perfectly here
    var p1 = 	beta({a:env_a,b:env_b});
    var p2 =  	beta({a:env_a,b:env_b});
    var p3 =  	beta({a:env_a,b:env_b});
    var v1 =    gaussian({mu:env_mu,sigma:env_sigma});
    var v2 =    gaussian({mu:env_mu,sigma:env_sigma});
    var v3 =    gaussian({mu:env_mu,sigma:env_sigma});

    var option1 = [p1,v1];
    var option2 = [p2,v2];
    var option3 = [p3,v3];

    //calc[i] is the current impression, calcObs(option1) is the agent modeling what would have been seen given its local p1,v1.
    //so this "observe" line says: the process described in calcObs gave a result very near the impression 'calc' when applied to [p1,v1]
    //This constrains p1 and v1
    observe(Gaussian({mu:calcObs(option1),sigma:.5}),calc[0])
    observe(Gaussian({mu:calcObs(option2),sigma:.5}),calc[1])
    observe(Gaussian({mu:calcObs(option3),sigma:.5}),calc[2])

    var ord_infer = function(option1,option2,data,getAttribute,tolerance){
	var a = getAttribute(option1);
	var b = getAttribute(option2);
	var myComparison = flip(ord_err) ? categorical({vs:["<","=",">"]}) : Math.abs(a-b) < tolerance ? "=" : a > b ? ">" : "<"; //nested ? avoids reassignment.
	condition(myComparison==data) //myComparison depends on/constrains estimated attributes, passed inside option1 and option2.
    }

    //ord observations: splitting them like this looks repetitive, but anything that avoids calling each comparison individually is very cruel to the sampler.
    //PROB
    //ab
        ord_infer(option1, option2, ord[0][0],function(x){x[0]},prob_tolerance)
    //ac
        ord_infer(option1, option3, ord[0][1],function(x){x[0]},prob_tolerance)
    //bc
        ord_infer(option2, option3, ord[0][2],function(x){x[0]},prob_tolerance)
    //PAYOFF
    //ab
        ord_infer(option1, option2, ord[1][0],function(x){x[1]},payoff_tolerance)
    //ac
        ord_infer(option1, option3, ord[1][1],function(x){x[1]},payoff_tolerance)
    //bc
        ord_infer(option2, option3, ord[1][2],function(x){x[1]},payoff_tolerance)

    //Inferred value of each option:
    var value1 = p1*v1;
    var value2 = p2*v2;
    var value3 = p3*v3;

    //return your choice:
    if(value1>value2&&value1>value3)return "a";
    if(value2>value1&&value2>value3)return "b";
    if(value3>value1&&value3>value2)return "c";
    return "washout";//should never happen.
 }


var model_2option = function(){ //copy-paste of model[3-option] with one option removed! For checking the impact of decoys.
    //priors, reflect environment perfectly here
    var p1 = 	beta({a:env_a,b:env_b});
    var p2 =  	beta({a:env_a,b:env_b});
    var v1 =    gaussian({mu:env_mu,sigma:env_sigma});
    var v2 =    gaussian({mu:env_mu,sigma:env_sigma});

    var option1 = [p1,v1];
    var option2 = [p2,v2];

    observe(Gaussian({mu:calcObs(option1),sigma:.5}),calc[0])
    observe(Gaussian({mu:calcObs(option2),sigma:.5}),calc[1])

    var ord_infer = function(option1,option2,data,getAttribute,tolerance){
	var a = getAttribute(option1);
	var b = getAttribute(option2);
	var myComparison = flip(ord_err) ? categorical({vs:["<","=",">"]}) : Math.abs(a-b) < tolerance ? "=" : a > b ? ">" : "<"; //nested ? avoids reassignment.
	condition(myComparison==data)
    }
//prob
        ord_infer(option1, option2, ord[0][0],function(x){x[0]},prob_tolerance)
//payoff
        ord_infer(option1, option2, ord[1][0],function(x){x[1]},payoff_tolerance)

    //Inferred value of each option:
    var value1 = p1*v1;
    var value2 = p2*v2;

    //return your choice:
    return value1>value2 ? "a" : "b";
 }

var hm_samples = 500;
var result_2 =  Infer({method: 'MCMC', samples: hm_samples, lag: 10, burn: 100, model: model_2option});
var result_3 = Infer({method: 'MCMC', samples: hm_samples, lag: 10, burn: 100, model: model});
 
var A_2 = reduce(function(a,b){a+b},0,map(function(x){(x["value"]=="a") ? 1 : 0},result_2["samples"]))/hm_samples;
var A_3 = reduce(function(a,b){a+b},0,map(function(x){(x["value"]=="a") ? 1 : 0},result_3["samples"]))/hm_samples;

return [A_2,A_3];
}


//SANDPIT testing area:

//This is an 'R' decoy, the in-text example from Wedell
//Decoy should increase preference for A
var A = [.5,20];
var B = [.4,25];
var decoy = [.5,18];

//Same decoy now an 'F'
//decoy increases preference for B
//var A = [.67, 15];
//var B = [.5, 20];
//var decoy = [.5,18];

//This is a paranoia check, decoy should have no effect.
//var A = [.5,20];
//var B = [.4,25];
//var decoy = [.10,10] 

var results =repeat(50,function(){decoyImpact(A,B,decoy)});

var AB_prefs = map(function(x){x[0]>.5 ? "a" : "b"},results)
var ABC_prefs = map(function(x){x[1]> .5 ? "a" : "b"},results)
console.log("Two-option preferences")
viz.auto(AB_prefs);
console.log("Three-option preferences")
viz.auto(ABC_prefs);
//Model params: (these are almost the Wedell ones, but normal instead of t(df=100). Note ord_err=0, U is identity. Paper varies calc_sd
//environment params
var env_a = 1;
var env_b = 1; //probs distributed as beta(env_a,env_b)
var env_mu = 19.6;
var env_sigma = 8.08; //payoffs distributed as gaussian(env_mu,env_sigma)
//calc obs params
var alpha = 1; //weights probability (as exponent)
var U = function(x){return x}; //function(x){return Math.log(x)}; //converts payoff value to subjective utility. Log seems attractive... other options?
var calc_sd = 4; //noise level. Sensible range depends on both env and U.
//Ord obs params
var prob_tolerance = .011; //from pg7 LHS summary.
var payoff_tolerance = 1.1; //ordObs doesn't apply U first, as in calcObs, maybe it should?
var ord_err = 0;//.2;
//end params


//environment setup
var getOption = function(){ //sets convention： Option = [prob,payoff]
    return [
	beta({a:env_a,b:env_b}),
	gaussian({mu:env_mu,sigma:env_sigma})
	]
}
var getTrial = function(){
  return repeat(3, getOption);
}

var expectedValue = function(anOption){ //helper, to compare against.
    return anOption[0]*anOption[1];
}



//model
//calculation Observation
var calcObs = function(anOption){
  //params
    //derived:
    var noise = gaussian({mu:0,sigma:calc_sd});
    //observed values
    var prob = anOption[0];
    var payoff = anOption[1];
    //calculation observation M (eq4)
    return Math.pow(prob, alpha)*U(payoff)+noise;
}


//ordinal Observation
var ordObs = function(arr){ 
//setup helper, so you can have a prob comparator and a payoff comparator and a whatever else comparator DRY.
    var getComparator = function (getAttribute,tolerance){
	return function(objA,objB){
	    if(flip(ord_err)) return categorical({vs:["<","=",">"]}); //flat rate of uniform error. If comparisons were perfect, underlying values noisy, err scales with distance, might be fun to look at?
	    var a = getAttribute(objA);
	    var b = getAttribute(objB);
	    if(Math.abs(a-b)<tolerance) return "=";
	    if(a>b) return ">";
	    if(a<b) return "<";
	}
    }
    
    //tweakable, but this setup here is specific to [prob, payoff] gambles.
    var compare_prob = getComparator(function(x){x[0]},prob_tolerance); //getters match convention that options are [prob, payoff]
    var compare_payoff = getComparator(function(x){x[1]},payoff_tolerance);
    var attributes = [compare_prob, compare_payoff]; //List of all the things you want to compare on: may change depending on stim.
    //end setup.
    
    //walk through all pairs. Assumes comparison direction doesn't matter, which is not true in general.
    var getpairs = function(tomatch, remaining,comparefn){
    if(remaining.length==0) return [];
	map(function(x){comparefn(tomatch[0],x)}, remaining).concat(getpairs(tomatch.slice(1,tomatch.length),remaining.slice(1,remaining.length),comparefn))
    }

    //for each comparison listed in 'attributes', make that comparison on all pairs in arr.
    return map(function(cmp_fn){getpairs(arr,arr.slice(1,arr.length),cmp_fn)},attributes);
}
//end model

//Check implications of the model re context-sensitive preference:
var decoyImpact = function(A,B,decoy){ //where A, B, and decoy are Options, ie, [prob,payoff]. Returns [choseA(AB), choseA(ABC)]
console.log(".");//poorman's progress bar
//one noisy impression of the gamble each time this runs. model accesses the whole impression, model_2option just the A,B parts.
    var ord = ordObs([A,B,decoy]);
    var calc = map(calcObs,[A,B,decoy]);
    
//Inference functions (reference the impression)
var model = function(){
    //priors, reflect environment perfectly here
    var p1 = 	beta({a:env_a,b:env_b});
    var p2 =  	beta({a:env_a,b:env_b});
    var p3 =  	beta({a:env_a,b:env_b});
    var v1 =    gaussian({mu:env_mu,sigma:env_sigma});
    var v2 =    gaussian({mu:env_mu,sigma:env_sigma});
    var v3 =    gaussian({mu:env_mu,sigma:env_sigma});

    var option1 = [p1,v1];
    var option2 = [p2,v2];
    var option3 = [p3,v3];

    //calc[i] is the current impression, calcObs(option1) is the agent modeling what would have been seen given its local p1,v1.
    //so this "observe" line says: the process described in calcObs gave a result very near the impression 'calc' when applied to [p1,v1]
    //This constrains p1 and v1
    observe(Gaussian({mu:calcObs(option1),sigma:.5}),calc[0])
    observe(Gaussian({mu:calcObs(option2),sigma:.5}),calc[1])
    observe(Gaussian({mu:calcObs(option3),sigma:.5}),calc[2])

    var ord_infer = function(option1,option2,data,getAttribute,tolerance){
	var a = getAttribute(option1);
	var b = getAttribute(option2);
	var myComparison = flip(ord_err) ? categorical({vs:["<","=",">"]}) : Math.abs(a-b) < tolerance ? "=" : a > b ? ">" : "<"; //nested ? avoids reassignment.
	condition(myComparison==data) //myComparison depends on/constrains estimated attributes, passed inside option1 and option2.
    }

    //ord observations: splitting them like this looks repetitive, but anything that avoids calling each comparison individually is very cruel to the sampler.
    //PROB
    //ab
        ord_infer(option1, option2, ord[0][0],function(x){x[0]},prob_tolerance)
    //ac
        ord_infer(option1, option3, ord[0][1],function(x){x[0]},prob_tolerance)
    //bc
        ord_infer(option2, option3, ord[0][2],function(x){x[0]},prob_tolerance)
    //PAYOFF
    //ab
        ord_infer(option1, option2, ord[1][0],function(x){x[1]},payoff_tolerance)
    //ac
        ord_infer(option1, option3, ord[1][1],function(x){x[1]},payoff_tolerance)
    //bc
        ord_infer(option2, option3, ord[1][2],function(x){x[1]},payoff_tolerance)

    //Inferred value of each option:
    var value1 = p1*v1;
    var value2 = p2*v2;
    var value3 = p3*v3;

    //return your choice:
    if(value1>value2&&value1>value3)return "a";
    if(value2>value1&&value2>value3)return "b";
    if(value3>value1&&value3>value2)return "c";
    return "washout";//should never happen.
 }


var model_2option = function(){ //copy-paste of model[3-option] with one option removed! For checking the impact of decoys.
    //priors, reflect environment perfectly here
    var p1 = 	beta({a:env_a,b:env_b});
    var p2 =  	beta({a:env_a,b:env_b});
    var v1 =    gaussian({mu:env_mu,sigma:env_sigma});
    var v2 =    gaussian({mu:env_mu,sigma:env_sigma});

    var option1 = [p1,v1];
    var option2 = [p2,v2];

    observe(Gaussian({mu:calcObs(option1),sigma:.5}),calc[0])
    observe(Gaussian({mu:calcObs(option2),sigma:.5}),calc[1])

    var ord_infer = function(option1,option2,data,getAttribute,tolerance){
	var a = getAttribute(option1);
	var b = getAttribute(option2);
	var myComparison = flip(ord_err) ? categorical({vs:["<","=",">"]}) : Math.abs(a-b) < tolerance ? "=" : a > b ? ">" : "<"; //nested ? avoids reassignment.
	condition(myComparison==data)
    }
//prob
        ord_infer(option1, option2, ord[0][0],function(x){x[0]},prob_tolerance)
//payoff
        ord_infer(option1, option2, ord[1][0],function(x){x[1]},payoff_tolerance)

    //Inferred value of each option:
    var value1 = p1*v1;
    var value2 = p2*v2;

    //return your choice:
    return value1>value2 ? "a" : "b";
 }

var hm_samples = 500;
var result_2 =  Infer({method: 'MCMC', samples: hm_samples, lag: 10, burn: 100, model: model_2option});
var result_3 = Infer({method: 'MCMC', samples: hm_samples, lag: 10, burn: 100, model: model});
 
var A_2 = reduce(function(a,b){a+b},0,map(function(x){(x["value"]=="a") ? 1 : 0},result_2["samples"]))/hm_samples;
var A_3 = reduce(function(a,b){a+b},0,map(function(x){(x["value"]=="a") ? 1 : 0},result_3["samples"]))/hm_samples;

return [A_2,A_3];
}


//SANDPIT testing area:

//This is an 'R' decoy, the in-text example from Wedell
//Decoy should increase preference for A
var A = [.5,20];
var B = [.4,25];
var decoy = [.5,18];

//Same decoy now an 'F'
//decoy increases preference for B
//var A = [.67, 15];
//var B = [.5, 20];
//var decoy = [.5,18];

//This is a paranoia check, decoy should have no effect.
//var A = [.5,20];
//var B = [.4,25];
//var decoy = [.10,10] 

var results =repeat(50,function(){decoyImpact(A,B,decoy)});

var AB_prefs = map(function(x){x[0]>.5 ? "a" : "b"},results)
var ABC_prefs = map(function(x){x[1]> .5 ? "a" : "b"},results)
console.log("Two-option preferences")
viz.auto(AB_prefs);
console.log("Three-option preferences")
viz.auto(ABC_prefs);


parameters{
  //this model sees choice data and stimuli, tries to infer agent setup parameters.  
  vector[3] calc_observations[N];
  int ord_observations_prob[N,3]; //[N, #pairs formable from K options]
  int ord_observations_payoff[N,3];

  real calc_sd[hm_ppnts];
  real p_err[hm_ppnts];
  real tolerance_prob[hm_ppnts]; //tolerance vals from pg 374 LHS point 3.
  real tolerance_payoff[hm_ppnts];

  real prob[N,3]; //agent's subjective/noisy guesses at these values.
  real payoff[N,3];
  vector[3] estValue[N];
}

transformed parameters{
    for(i in 1:N){
      for(j in 1:3){
	estValue[i,j] = prob[i,j]*payoff[i,j];
      }
    }
}
model{  
  for(i in 1:N){  
    for(j in 1:3){ //for each option j of trial i:
      prob[i,j]~beta(5*true_prob[i,j],5*true_prob[i,j]/true_prob[i,j]-5*true_prob[i,j]); //Intention here was to express the idea that agent's subjective prob is probably somewhere near the known true prob, so use a prior with the mean over the true prob. Here beta(a,a/p-a) has mean p, but variance uneven in p with high and low p less variable, this is possibly Very Bad. The variance for beta is ab/((a+b)^2*(a+b+1)), so you should be able to find a and b for any p with a fixed variance. Also, in the agent-eye-view, the prior here reflects expectation about the attribute distribution, which is now missing from this model. Hmmm.
      payoff[i,j]~normal(true_payoff[i,j],10);//Simpler, but same questions about missing agent priors.
      
      calc_observations[i,j]~normal(prob[i,j]*payoff[i,j],calc_sd[ppntID[i]]);//eq4 without alpha or U().
    }//end for option j

    //ord observation
    ord_observations_prob[i,1] ~ categorical(get_ordtheta(prob[i,1], prob[i,2], tolerance_prob[ppntID[i]],p_err[ppntID[i]]));
    ord_observations_prob[i,2] ~ categorical(get_ordtheta(prob[i,1],prob[i,3],tolerance_prob[ppntID[i]],p_err[ppntID[i]]));
    ord_observations_prob[i,3] ~ categorical(get_ordtheta(prob[i,2],prob[i,3],tolerance_prob[ppntID[i]],p_err[ppntID[i]]));

    ord_observations_payoff[i,1] ~ categorical(get_ordtheta(payoff[i,1], payoff[i,2], tolerance_payoff[ppntID[i]],p_err[ppntID[i]]));
    ord_observations_payoff[i,2] ~ categorical(get_ordtheta(payoff[i,1],payoff[i,3],tolerance_payoff[ppntID[i]],p_err[ppntID[i]]));
    ord_observations_payoff[i,3] ~ categorical(get_ordtheta(payoff[i,2],payoff[i,3],tolerance_payoff[ppntID[i]],p_err[ppntID[i]]));

    choice[i]~choosemax(estValue[i,1],estValue[i,2],estValue[i,3]);
  }//end for trial i
}//end model block
