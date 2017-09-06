

kin_retriever <- function(pid, pop_reg){

    # takes pid, f_pid, m_pid, ff_pid, fm_pid, mf_pid, mm_pid
    pop_reg$pid <- as.character(pop_reg$pid)
    pop_reg$f_pid <- as.character(pop_reg$f_pid)
    pop_reg$m_pid <- as.character(pop_reg$m_pid)
    pop_reg$ff_pid <- as.character(pop_reg$ff_pid)
    pop_reg$fm_pid <- as.character(pop_reg$fm_pid)
    pop_reg$mf_pid <- as.character(pop_reg$mf_pid)
    pop_reg$mm_pid <- as.character(pop_reg$mm_pid)

    if(is.na(pid)) stop("pid must not be NA")
    if(!pid %in% pop_reg$pid) stop("pid must be in the pop_reg")

    my_dad <- pop_reg$f_pid[which(pop_reg$pid==pid)]
    my_mom <- pop_reg$m_pid[which(pop_reg$pid==pid)]
    my_sons <- pop_reg$pid[which(pop_reg$f_pid==pid)]
    my_mates <- sort(unique(pop_reg$m_pid[which(pop_reg$f_pid==pid)]))
    my_mates_dads <- sort(unique(pop_reg$f_pid[which(pop_reg$pid %in% my_mates)]))
    my_mates_bros <- which(pop_reg$f_pid %in% my_mates_dads & pop_reg$male==1)
    my_sibs <- pop_reg$pid[which((pop_reg$f_pid==my_dad | pop_reg$m_pid==my_mom) & 
        pop_reg$pid!=pid)]
    my_nephews <- pop_reg$pid[which(pop_reg$male==1 & (pop_reg$f_pid %in% my_sibs | 
        pop_reg$m_pid %in% my_sibs))]
    my_grandparents <- sort(unique(c(pop_reg$ff_pid[which(pop_reg$pid==pid)], pop_reg$fm_pid[which(pop_reg$pid==pid)], 
        pop_reg$mf_pid[which(pop_reg$pid==pid)], pop_reg$mm_pid[which(pop_reg$pid==pid)])))
    my_uncles <- integer(0)
    my_aunts <- integer(0)
    my_cousins <- integer(0)
    if(length(my_grandparents)>0){
        my_uncles <- pop_reg$pid[which(pop_reg$male==1 & 
            (pop_reg$f_pid %in% my_grandparents | pop_reg$m_pid %in% my_grandparents) & 
            pop_reg$pid!=my_dad)]
        my_aunts <- pop_reg$pid[which(pop_reg$male==0 & 
            (pop_reg$f_pid %in% my_grandparents | pop_reg$m_pid %in% my_grandparents) & 
            pop_reg$pid!=my_mom)]
        my_cousins <- pop_reg$pid[which((pop_reg$f_pid %in% my_uncles | 
            pop_reg$m_pid %in% my_aunts) & pop_reg$pid!=pid)]
    }
    output <- na.omit(c(my_dad, my_mom, my_sons, my_mates, 
        my_mates_dads, my_mates_bros, my_sibs, my_nephews, 
        my_grandparents, my_uncles, my_aunts, my_cousins))
    output <- as.numeric(output)
    return(output)
}

