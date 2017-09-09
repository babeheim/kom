
calc_adoption_risk <- function( parameter_list, risk_table, cognition ){

  # need to make this more flexible

  if( !cognition %in% c("additive", "multiplicative") ) stop("not a valid cognitive model")

  logistic <- function(x) exp(x) / (1 + exp(x))

  if( cognition == "additive" ){

    baseline <- parameter_list$baseline_probability
    b_kin <- parameter_list$kin_network_effect
    b_neighbor <- parameter_list$neighbor_effect
    b_wealth <- parameter_list$wealth_effect

    alpha <- log( baseline / (1 - baseline) )

    logit_pr_adopt <- alpha + 
      b_kin * risk_table$kin_has + 
      b_neighbor * risk_table$neighbor_has + 
      b_wealth * risk_table$wealth

    pr_adopt <- logistic(logit_pr_adopt)

  }

  if( cognition == "multiplicative" ){

    b_kin <- parameter_list$kin_network_effect
    b_neighbor <- parameter_list$neighbor_effect
    b_wealth <- parameter_list$wealth_effect

    knowledge_baseline <- parameter_list$knowledge_baseline
    opportunity_baseline <- parameter_list$opportunity_baseline
    motivation_baseline <- parameter_list$motivation_baseline

    knowledge_alpha <- log( knowledge_baseline / 
     (1 - knowledge_baseline) )
    opportunity_alpha <- log( opportunity_baseline / 
     (1 - opportunity_baseline) )
    motivation_alpha <- log( motivation_baseline / 
     (1 - motivation_baseline) )

    logit_knowledge <- knowledge_alpha + b_kin * risk_table$kin_has
    logit_opportunity <- opportunity_alpha + b_wealth * risk_table$wealth
    logit_motivation <- motivation_alpha + b_neighbor * risk_table$neighbor_has

    knowledge <- logistic( logit_knowledge )
    opportunity <- logistic( logit_opportunity )
    motivation <- logistic( logit_motivation )

    logit_pr_adopt <- log(knowledge) + 
      log(opportunity) + log(motivation)

    pr_adopt <- exp(logit_pr_adopt)

  }

  return(pr_adopt)

}