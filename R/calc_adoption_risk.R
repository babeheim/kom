
calc_adoption_risk <- function( parameter_list, risk_table, cognition ){

  if( !cognition %in% c("additive", "multiplicative") ) stop("not a valid cognitive model")

  if( cognition == "additive" ){

    baseline <- parameter_list$baseline_probability # (0,1)
    b_kin <- parameter_list$kin_network_effect
    b_dist <- parameter_list$town_distance_effect
    b_neighbor <- parameter_list$neighbor_effect
    b_wealth <- parameter_list$wealth_effect

    alpha <- log( baseline / (1 - baseline) )

    logit_pr_adopt <- alpha + 
      b_kin * risk_table$kin_has + 
      b_dist * risk_table$log_dist_town + 
      b_neighbor * risk_table$neighbor_has + 
      b_wealth * risk_table$wealth

    pr_adopt <- logistic(logit_pr_adopt)

  }

  # # multiplicative model

  if( cognition == "multiplicative" ){

    b_kin <- parameter_list$kin_network_effect
    b_dist <- parameter_list$town_distance_effect
    b_neighbor <- parameter_list$neighbor_effect
    b_wealth <- parameter_list$wealth_effect

    knowledge_baseline <- 0.001
    opportunity_baseline <- 0.001
    motivation_baseline <- 1.0

    knowledge_alpha <- log( knowledge_baseline / 
     (1 - knowledge_baseline) )
    opportunity_alpha <- log( opportunity_baseline / 
     (1 - opportunity_baseline) )
    motivation_alpha <- log( motivation_baseline / 
     (1 - motivation_baseline) )

    logit_knowledge <- knowledge_alpha + b_kin * risk_table$kin_has + b_dist * risk_table$log_dist_town
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