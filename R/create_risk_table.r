
create_risk_table <- function( pop_data, diffusion_history, neighbor_radius ){

  preg <- pop_data$people
  hreg <- pop_data$households
  vreg <- pop_data$villages

  d <- diffusion_history

  o <- order(d$date)
  d <- d[o,]

  not_at_risk <- which(d$age < 16*365 | d$male==0) 
  risk <- d[-not_at_risk,]
  trait_holders <- sort(unique(d$pid[d$phi==1]))
  census_acquired <- NA
  for(i in 1:length(trait_holders)){
    my_rows <- which(d$pid==trait_holders[i])
    row_acquired <- my_rows[min(which(d$phi[my_rows]==1))]
    census_acquired[i] <- d$date[row_acquired]
  }

  risk$dist_town <- vreg$dist_town[match(risk$village, vreg$village)]
  risk$dist_town <- risk$dist_town / 1000
  risk$log_dist_town <- log(risk$dist_town)

  risk$kin_has <- 0
  risk$neighbor_has <- 0

  # trait_holders[i]
  # census_acquired[i]
  # d[d$pid==trait_holders[i],]
  # i <- i + 1

  census_list <- sort(unique(risk$date))

  for(j in 1:length(census_list)){

    risk_now <- risk[which( risk$date == census_list[j] ),]
    risk_hist <- risk[which( risk$date < census_list[j] ),] # use the current census or no?

    for( i in 1:nrow( risk_now ) ){

      # check i's kin
      my_kin <- kin_retriever( risk_now$pid[i], preg )
      kin_have_trait <- as.numeric(my_kin %in% risk_hist$pid[risk_hist$phi==1])
      if( length( my_kin ) > 0 ) risk_now$kin_has[i] <- sum(kin_have_trait)
      
      # check i's neighbors
      my_neighbors <- neighbor_finder( risk_now$pid[i], preg, hreg, dist_radius=neighbor_radius )
      neighbors_have_trait <- as.numeric(my_neighbors %in% risk_hist$pid[risk_hist$phi==1])
      if( length( my_neighbors )>0 ) risk_now$neighbor_has[i] <- sum(neighbors_have_trait)
    
    }

    tar <- which( risk$date == census_list[j] )
    risk$kin_has[tar] <- as.numeric( risk_now$kin_has )
    risk$neighbor_has[tar] <- as.numeric( risk_now$neighbor_has )

  }

  trait_holders <- sort(unique(risk$pid[risk$phi==1]))

  census_acquired <- NA
  for(i in 1:length(trait_holders)){
    my_rows <- which(risk$pid==trait_holders[i])
    row_acquired <- my_rows[min(which(risk$phi[my_rows]==1))]
    census_acquired[i] <- risk$date[row_acquired]
  }

  post_event_rows <- integer()
  for(i in 1:length(trait_holders)) post_event_rows <- c(post_event_rows, 
    which(risk$pid==trait_holders[i] & risk$date > census_acquired[i]))

  risk$post <- 1:nrow(risk) %in% post_event_rows # diagnositc

  risk <- risk[-post_event_rows,]

  return(risk)

}