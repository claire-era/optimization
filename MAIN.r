source("FUNCTIONS.r")
#---------------------------------------------------------------------------------------------------------------------------
#FUNCTION: SimplexBackend: run matrix using a sample data 
SimplexBackend <- function(supply, demand, data){
  if(sum(demand)>sum(supply)){
    return(NA) #No feasible solution
  }else{
    #PHASE A. GET A FEASIBLE TABLEAU
    result = CreateTab(supply, demand, data) #inside contains the inverse of the objective function embedded in the tableau
    
    # init_tab <<- result$initialTableau
    
    feasible_tab = GetFeasibleTab(result)

    #PHASE B. APPLY STANDARD MAXIMIZATION IN TABLEAU
    res=MaximizeTab(feasible_tab)
    
    final_tab=res$final_tab
    ftab <<- final_tab
    tablist=res$tab_list
    
    mcost = ((final_tab[nrow(final_tab),ncol(final_tab)])*-1)
    
    res = list(init_tab = feasible_tab, final_tab = final_tab, tablist = tablist, mincost = mcost)
    return(res)
  }
}

#MAIN PROGRAM
# SimplexBackend()