#FUNCTION: SimplexBackend: run matrix using a sample data 
SimplexBackend <- function(supply, demand, data){
  if(sum(demand)>sum(supply)){
    return(NA) #No feasible solution
  }else{
    #PHASE A. GET A FEASIBLE TABLEAU
    result = CreateTab(supply, demand, data) #inside contains the inverse of the objective function embedded in the tableau
    
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

#FUNCTION: CreateTab: creates tableau containing surplus variables. needs further modification
#RETURN VALUES: initialTableau, dataCnt, slackVarCnt -> basicially tableau na may negatives ganun.
CreateTab <- function(supply, demand, data){
  #VARIABLES
  supplyCnt=length(supply)
  demandCnt=length(demand)
  dataCnt=length(data)
  zCnt = 1
  solnCnt = 1
  varsCntCnt = 0  
  z=""
  maxConstListCnt = 0
  minConstListCnt = 0
  slackVarCnt = 0
  
  #VARIABLES with return values
  objFunc = ""
  objFuncRevised = NULL
  maxConstList = NULL
  minConstList = NULL
  constraintsList = NULL
  initTab = NULL
  
  #STEP 1: GENERATE OBJECTIVE FUNCTION
  for (i in 1:dataCnt) {
    if(i!=dataCnt)
      objFunc = paste(objFunc,"-",data[i],"*x",i, "+")
    else
      objFunc=paste(objFunc,"-",data[i],"*x",i)
    varsCnt=i
  }
  
  objFunc=unlist(strsplit(objFunc," "))
  for(i in objFunc){
    z=paste(z,i,sep="")
  }
  
  objFunc = z
  objFuncRevised = data*-1 #used for maximization instead of minimization
  
  # print(objFunc)
  # print(objFuncRevised)
  #STEP 2: LIST CONSTRAINTS (8)
  #STEP 2.1. COEFF OF MAXIMIZATION CONSTRAINTS
  e1 = c(1:5)
  e1 = c(e1, supply[1])
  e2 = c(6:10)
  e2 = c(e2, supply[2])
  e3 = c(11:15)
  e3 = c(e3, supply[3])
  
  maxConstList=list(e1,e2,e3)
  maxConstListCount=length(maxConstList)
  
  #STEP 2.2. COEFF OF MINIMIZATION CONSTRAINTS
  m = matrix(c(1:(varsCnt+demandCnt)), nrow=varsCnt/3,byrow=F)
  m[,ncol(m)]=t(t(demand))
  e4 = m[1,]
  e5 = m[2,]
  e6 = m[3,]
  e7 = m[4,]
  e8 = m[5,]
  minConstList=list(e4,e5,e6,e7,e8)
  minConstListCnt=length(minConstList)
  
  #STEP 2.3. COLLATE MINIMIZATION AND MAXIMIZATION CONSTRAINTS
  constraintsList = list(e1,e2,e3,e4,e5,e6,e7,e8)
  slackVarCnt = length(constraintsList)
  
  #STEP 3. SET UP INITIAL TABLEAU
  initTab = matrix(0:0, ncol=dataCnt+slackVarCnt+zCnt+solnCnt, nrow=slackVarCnt+zCnt)
  
  for(e in 1:length(constraintsList)){
    row = constraintsList[[e]]
    rlen=length(row)
    for(i in 1:(rlen-1)){
      initTab[e,row[i]]=1
    }
    if(row[rlen]==tail(row,n=1)){ #if last element, add the rhs of inequality
      initTab[e,ncol(initTab)]=row[rlen]
    }
    
    #if first three contraints, maximization constraint yun. set slk_var to 1, else set to -1
    if(e<=maxConstListCount){
      initTab[e,varsCnt+e] = 1
    }else{  #if e>maxConstListCount then it must be a minimization constraint already
      initTab[e,varsCnt+e] = -1
    }
  }
  
  #Create last revised z row of the init table: revised objective function + slack variables initialization
  lastRow=c(data,c(rep(0,slackVarCnt)))
  lastRow=c(lastRow,1,0)    
  
  #Augment to end of initTab: revised obj func
  initTab[slackVarCnt+zCnt,] = lastRow
  
  #RETURN VALUES: initialTableau, dataCnt, slackVarCnt
  tab = PutDimNames(initTab)
  result = list(initialTableau = tab, dataCnt = dataCnt, slackVarCnt = slackVarCnt)
  return(result)
}

#FUNCTION: GetBFS
#Generates the basic feasible solution (BFS) from the tableau
#RETURN VALUES: vars, val, isFeasible
GetBFS <- function(tab){    #all inactive variables must be 0: inactive == col na greater than 1 yung non zero
  #all active variables must be >0: active == col na isa lang yung hindi 0 sis. ok?
  cols = dimnames(tab)[[2]]
  values = c()
  coln = ncol(tab)
  rown = nrow(tab)
  soln = as.numeric(tab[,coln])
  zrow = as.numeric(tab[rown,])
  zval = zrow[ncol(tab)]
  tab = tab[1:(rown-1),]  # look at all cols except solution column

  for(j in 1:(ncol(tab)-1)){
    x = tab[,j]
    nz = 0
    val = 0
    val_index = 0
    for(i in 1:length(x)){
      c = x[i]
      if(c == 0){
        next
      }else{
        #take note of index + value
        #increment non-zero count
        val = c
        val_index = i
        nz = nz + 1
      }
    }

    if(nz > 1){
      values = c(values, 0)
    }else{
      value = as.numeric(soln[val_index]/val)
      values = c(values, value)
    }
  }
  
  feasible = NULL
  if(all(values>=0)) feasible=T
  else feasible=F
  
  #attach z value
  values = c(values, zval)

  return(list(vars = cols[1:(length(cols)-1)], val = values, feasible = feasible))
}

#FUNCTION: GetFeasibleTab
#Transforms infeasible tableau to a feasible one
GetFeasibleTab <- function(result){
  tab = result$initialTableau
  # samp = matrix(c(3,2,5,1,0,0,18,4,2,3,0,1,0,16,2,1,1,0,0,-1,4,-3,-2,-4,0,0,0,0),byrow=T,nrow=4,ncol=7)
  # tab = samp
  dataCnt = result$dataCnt
  slackVarCnt = result$slackVarCnt
  
  used = c()
  
  res = GetBFS(tab)
  vars = res$vars
  val = res$val
  isfeasible = res$feasible
  
  while(!isfeasible){
    #at first iteration, get row with a negative variable, this will be the basis for the pivot row---------------
    pr_index = GetNegRow(tab)
    # print(pr_index)
    
    # ##select pc index. perform trial and error, look at zrow and try to do trial and error. arbitrarily select from zrow -----------------------------
    zrow = as.numeric(tab[nrow(tab),])
    pc_index = GetRndIndex(dataCnt, zrow) #candidate pivot col index
    
    ##get piv element, NEVER pivot on 0.
    piv_e = tab[pr_index, pc_index]
    while(piv_e==0){
      pc_index = GetRndIndex(dataCnt,zrow)
      piv_e = tab[pr_index, pc_index]
    }
    # cat("tab[", pr_index,",",pc_index, "] = ", pe, "\n")
  
    ##pivot on pe using modified gauss jordan
    tab = ModifiedGJ(tab, pr_index, pc_index)
    
    ##update loop values 
    res = GetBFS(tab)
    isfeasible = res$feasible
  }
  
  return(tab)
}

#FUNCTION: GetNegRow
#Returns the first instance of a row in a tableau producing the infeasibility
GetNegRow <- function(tab){
  res = GetBFS(tab)
  vars = res$vars
  val = res$val
  feasible = res$feasible

  var_i = min(which((val)<0))
  ro = as.numeric(tab[,var_i])
  
  # nag supress nalang ako ng warning sad
  oldw <- getOption("warn")
  options(warn = -1)
  # -- start
  starred_r_index = min(which(ro<0))
  if(starred_r_index == Inf){
    starred_r_index = which(ro>0)
  }
  # -- end, turn warnings back on
  options(warn = oldw)
  #---- end warning
  
  starred_r = tab[starred_r_index,]
  starred_row = as.numeric(starred_r)
  pr_index = starred_r_index
  
  return(pr_index)
}

#FUNCTION: ModifiedGJ
#Performs pivoting for simplex!
ModifiedGJ <- function(m, pri, pci){
  n = dim(m)[1]
  
  m[pri,] = m[pri,]/m[pri,pci] #m[pri,pci] == pivot element
  j = 1
  while(j<=n){
    if(pri!=j){
      nr = m[j,pci] * m[pri,]
      m[j,] = m[j,] - nr
    }
    j = j + 1
  }
  return(m)
}

#FUNCTION: PutDimNames
#Puts dimension names within the tableau
PutDimNames <- function(tab){
  vars = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15")
  slack_vars = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8")
  columns = c(vars, slack_vars, "z", "soln")
  rows = c(slack_vars, "z")
  dimnames(tab) <- list(rows, columns)
  
  return(tab)
}

#FUNCTION: GetRndIndex
#Gets a random nonzero candidate pivot column index from the tableau
GetRndIndex <- function(dataCnt, starred_row){
  #--------- selection of max value in starred row
  col_index = sample(1:dataCnt, 1)
  val = starred_row[col_index]
  while(val==0){
    col_index = sample(1:dataCnt, 1)
    val = starred_row[col_index]
  }
  
  return(col_index)
  #----------------------------------------------
}

#FUNCTION: GetMinRatio
#Returns the index of the min ratio per acol/bcol
GetMinRatio <- function(acol, bcol){
  #for each positive number sa b, get test ratio a/b
  res = c()
  for(i in 1:length(bcol)){
    if(bcol[i]>0) res = c(res, (acol[i]/bcol[i]))
    else res = c(res, Inf)
  }
  # print(res)
  return(which.min(res))
}

#FUNCTION: MaximizeTab
#Maximizes tableau :>
#Global variable iter_tab is updated?
MaximizeTab <- function(tab){
  coln = ncol(tab)
  rown = nrow(tab)
  done = F
  i=1
  
  #create empty list lalagyan ng mga matrices for retrieval sa show tableau at step sa frontend
  tab_holder=c(list(tab))
  # print(GetBFS(tab))
  while(!done){
    zrow = as.numeric(tab[rown,])[1:coln-1] #disregard soln col
    
    #select pc from zrow
    pc_index = which.min(zrow)
    
    #select pr using test ratio, diregarding z value
    bcol = as.numeric(tab[,pc_index])[1:rown-1] #pivot column hehe, b
    acol = as.numeric(tab[,coln][1:rown-1]) #soln column hehe, a
    
    pr_index = GetMinRatio(acol, bcol)

    #identify pe
    piv_e = as.numeric(tab[pr_index, pc_index])
    
    #pivot at pe using gj
    tab = ModifiedGJ(tab, pr_index, pc_index)
    
    #check zrow if may negative pa
    zrow = as.numeric(tab[rown,])[1:coln-1]
    
    # print(zrow)
    lowest_num_index = which.min(zrow)
    if(zrow[lowest_num_index] >= 0) done = T
    else done = F
    
    #append to vector of matrices for viewing later
    tab_holder = c(tab_holder, list(tab))
    i=i+1
  }
  res=list(final_tab = tab, tab_list = tab_holder)
  return(res)
}

SolutionTable <- function(tab){
  res = (GetBFS(tab))$val
  # print(res)
  
  soltab = matrix(c(res[1:15]),nrow=3,ncol=5,byrow=T)
  
  # dimnames=list(NULL,c("Sacramento", "Salt Lake City","Albuquerque","Chicago","New York City","Total"))
  # soltab <- cbind(soltab, c(1,2,3,4))
  soltab = cbind(soltab, apply(soltab[,1:5],1,sum))
  soltab <- rbind(soltab, apply(soltab[,1:5],2,sum))
  soltab[nrow(soltab),ncol(soltab)] = NA
  dimnames(soltab) = list(NULL,c("Sacramento", "Salt Lake City","Albuquerque","Chicago","New York City","Total"))
  
  return(soltab)
}

Gaussian <- function(ma){
  n = dim(ma)[1]
  c = dim(ma)[2]
  
  # pivoting
  m = ma[order(ma[1:n,1],decreasing = TRUE),]
  for(i in seq(1, nrow(m)-1)){
    pivot_e = m[i,i]
    for(j in seq(i+1, nrow(m))){
      mult = m[j,i] / pivot_e
      vtbs = m[i,] * mult
      new_r = m[j,] - vtbs
      m[j,] = new_r
    }
  }
  x = c(1,2,3)
  b = m[,ncol(m)]
  j = i
  #backward substitution
  k = nrow(m)
  x = backsolve(m,b,k, upper.tri = TRUE, transpose = FALSE)
  
  return(x)
}

PolynomialRegression <- function(x, y, order){
  m = matrix(nrow = order + 1,ncol = order + 2) #create empty matrix with dimensions m(order+1, order+2)
  # x = datapoints[[1]]
  # y = datapoints[[2]]
  
  #CONSTRUCT ACM GIVEN THE DATA POINTS
  for(i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      if(i==1 && j==1){ #for the first element m[1,1]
        m[i,j] = length(x)
      }else{ #for the rest of the elements of the matrix
        m[i,j] = sum(x^((j-1)+(i-1)))
        if(j==ncol(m)){ #if last element of current row
          if(i==1) m[i,j] = sum(y)
          else  m[i,j] = sum((x^(i-1))*(y))
        }
      }
    }
  }
  
  # print(m)
  #GENERATE POLYNOMIAL STRING
  ss = Gaussian(m) #SOLVE FOR THE SYSTEM
  poly_string = "function(x) "
  for(i in 1:length(ss)){ #GENERATE POLYNOMIAL STRING
    if(i==length(ss)){
      poly_string = paste(poly_string,ss[i]," * x ^ ",i-1,sep="")
    }else if(i==1){
      poly_string = paste(poly_string,ss[i]," + ",sep="")
    }else{
      poly_string = paste(poly_string,ss[i]," * x ^ ",i-1," + ",sep="")
    }
  }
  #EVALUATE POLYNOMIAL STRING TO BE A FUNCTION
  poly_func = eval(parse(text=poly_string))
  results = list(augcoeffmatrix = m, unknowns = ss, s = poly_string, f = poly_func)
  return(results)
  
}