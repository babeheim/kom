


neighbor_finder <- function(pid, pop_reg, house_reg, dist_radius=50, units="m"){

    # for each pid, query their immediate neighbor network, 
    # not including other men in their own house

    # house_reg: household, village, x_coord, y_coord

    if(!units %in% c("m","km")) stop("units must be 'm' or 'km'")

    if(units=="km") dist_radius <- dist_radius * 1000
    # since house_reg's x and y should be in meters

    if( sd(house_reg$x_coord) < 10 | sd(house_reg$y_coord) < 10) warning("
        this village's x and y coordinates do not seem to be
        in meters; change to meters!")

    if( dist_radius < 10 ) warning("this search radius is 
        quite short, less than ten meters!")

    pid <- as.character(pid)
    pop_reg$pid <- as.character(pop_reg$pid)
    house_reg$household <- as.character(house_reg$household)
    house_reg$village <- as.character(house_reg$village)

    if(is.na(pid)) stop("pid must not be NA")
    if(!is.numeric(dist_radius)) stop("dist_radius must be numeric")
    if(is.numeric(dist_radius) & dist_radius <= 0) stop(
        "dist_radius must be > 0")
    if(!is.numeric(house_reg$x_coord)) stop(
        "house_reg$x_coord must be numeric")
    if(!is.numeric(house_reg$y_coord)) stop(
        "house_reg$y_coord must be numeric")
    if(!pid %in% pop_reg$pid) stop("pid is not in pop_reg")

    if(any(!is.na(pop_reg$household) & 
        is.na(pop_reg$village))) stop("pop_reg has household 
        with no villages - dangerous!")

    my_household <- pop_reg$household[which(pop_reg$pid==pid)]
    if(is.na(my_household)) stop("pid's household is NA")
    if(!my_household %in% house_reg$household) stop("pid's 
        household is not in the house_reg")

    my_village <- pop_reg$village[which(pop_reg$pid==pid)]
    my_house_x_coord <- house_reg$x_coord[which(
        house_reg$household==my_household)]
    my_house_y_coord <- house_reg$y_coord[which(
        house_reg$household==my_household)]
    village_house_rows <- which( house_reg$village==my_village )
    village_house_distances <- sqrt( 
        (my_house_x_coord - house_reg$x_coord[village_house_rows])^2 
        + (my_house_y_coord - house_reg$y_coord[village_house_rows])^2 )

    if( sum( village_house_distances < 
        dist_radius ) == length(village_house_distances) ) 
        warning("all houses in this village within the 
        distance specified")

    nearby_house_rows <- village_house_rows[which( village_house_distances < dist_radius )]
    nearby_houses <- house_reg$household[nearby_house_rows]
    nearby_houses <- setdiff( nearby_houses, my_household )
    my_neighbor_rows <- which( pop_reg$household %in% nearby_houses )
    my_neighbors <- pop_reg$pid[my_neighbor_rows]
    output <- setdiff( my_neighbors, pid )
    return(output)
}
