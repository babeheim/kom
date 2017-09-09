
diffuse <- function( parameter_list, pop_data, n_seed = 5, n_years = 1, 
  neighbor_radius = 500, census_period = 10, quiet = TRUE, 
  cognition = "additive" ){

  preg <- pop_data$people
  hreg <- pop_data$households
  vreg <- pop_data$villages # not used, for now

  preg$ff_pid <- 

  preg$pr_adopt <- NA

  ######### Simulator Initializaiton #########

  if(any(is.na(preg$male))) stop("preg male has missing values")

  # initialize diffusion trait

  if(!"age" %in% colnames(preg)) stop("there is no age variable")

  preg$phi <- 0
  can <- which( is.na(preg$dod) & preg$male==1 
      & preg$age >= 16*365 & preg$phi==0 )
  if(length(can)==0) stop("no one is available to adopt the trait")
  if(n_seed >= length(can)) stop("there are fewer people available than seed amount")
  tar <- sample( can, n_seed )
  preg$phi[tar] <- 1

  ####

  day <- 1
  days_max <- ceiling(365*n_years)
  should_simulate_day <- day <= days_max

  #########

  output <- list()

  ####### Simulator Daily Loop #######

  while( should_simulate_day ){

    # phi_get
    if(day %% 10 == 0){

      can <- which( is.na(preg$dod) & preg$male==1 
        & preg$age >= 16*365 & preg$phi==0 )

      if(length(can)>0){
        
        risk <- preg[can,]

        risk$kin_has <- 0
        risk$neighbor_has <- 0

        for(i in 1:nrow(risk)){

          # identify kin phi status
          my_kin <- kin_retriever( risk$pid[i], preg )
          my_kin_rows <- which( preg$pid %in% my_kin )
          if( length( my_kin_rows ) > 0 ) risk$kin_has[i] <- any( 
            preg$phi[my_kin_rows]==1 )
          
          # identify neighbor phi status
          my_neighbors <- neighbor_finder( risk$pid[i], 
            preg, hreg, dist_radius = neighbor_radius)
          if( length( my_neighbors )>0 ) risk$neighbor_has[i] <- sum( 
            preg$phi[preg$pid %in% my_neighbors]==1 )

        }

        risk$pr_adopt <- calc_adoption_risk( parameter_list = 
          parameter_list, risk, cognition=cognition)

        preg$pr_adopt[can] <- risk$pr_adopt

        # determine phi acquisitions

        risk$got_phi <- rbinom( nrow(risk) , 1, risk$pr_adopt )
        new_phi_owners <- risk$pid[which(risk$got_phi==1)]
        if(length(new_phi_owners)>0){
          new_phi_owners_rows <- which(preg$pid %in% new_phi_owners)
          preg$phi[new_phi_owners_rows] <- 1
        }
      }
    }

    # here's where we model the observer behavior, or at least part of it
    if(day == 1 | day %% census_period == 0){

      cens <- preg[,c('pid', 'male', 'dob', 'age', 'dod', 'm_pid', 'f_pid', 
         'household', 'village', 'wealth', 'phi', 'pr_adopt')]
      cens$date <- day
      drop <- which(!is.na(cens$dod))
      if(length(drop)>0) cens <- cens[-drop,]
      cens_name <- paste0("day", day)
      if(length(output)==0){
        output <- cens
      } else {
        output <- rbind(output, cens)
      }

      if(quiet==FALSE) print(paste('day', day))

    }

    day <- day + 1
    
    should_simulate_day <- day <= days_max & any(is.na(preg$dod))
    
  }

  return(output)

}
