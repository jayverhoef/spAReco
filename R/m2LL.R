#-------------------------------------------------------------------------------
#
#           m2LL
#
#-------------------------------------------------------------------------------

#' two times the negative log-likelihood
#'
#' two times the negative log-likelihood
#'
#' @param theta covariance parameters, with overall variance parameter profiled out.
#' @param X design matrix for fixed effects
#' @param y vector of data for response variable
#' @param indComp an additive independent component to the model.  Default is TRUE.  
#' @param Nmat neighborhood matrix
#' @param distMat distance matrix
#' @param crsStk indicator matrix of whether locations cross stock boundaries
#' @param indSamp indicator vector for wheter location was sampled. Zero, or FALSE, indicates missing value
#' @param model either 'CAR' or 'SAR' to be the model associated with the Nmat argument
#' @param logical value on whether Nmat should be row-standardized
#' @param rhobound upper bound for rho.  This should be determined from the eigenvalues of Nmat prior to running function
#'
#' @return two times the negative log-likelihood
#'
#' @author Jay Ver Hoef jverhoef
#' @rdname m2LL
#' @export m2LL 

m2LL = function(theta, X, y, indComp, Nmat=NULL, 
  distMat=NULL, crsStk=NULL, indSamp, model = NULL, rowStand = NULL, 
  rhoBound = c(-1,1))
{
  if(any(abs(theta) > 10.1)) return(1e+32)

  ntheta = 0
  nn = length(y)
  nN = length(indSamp)
  V = matrix(1, nrow = nN, ncol = nN )
  diag(V) = 0
  if(!is.null(Nmat)) {
    V = as(Nmat, 'sparseMatrix')
  }
  if(!is.null(crsStk)) {
    ntheta = ntheta + 1
    attr(theta,'names')[ntheta] = 'logCrsStk'
    V = V * exp(-crsStk/exp(theta[ntheta]))
  }
  if(!is.null(distMat)) {
    ntheta = ntheta + 1
    attr(theta,'names')[ntheta] = 'logDist'
    V = V * exp(-distMat/exp(theta[ntheta]))
  }
  if(!is.null(model)) {
    ntheta = ntheta + 1
    rho = rhoBound[1] + .00005 + exp(theta[ntheta])/
      (1 + exp(theta[ntheta]))*.9999*(rhoBound[2] - rhoBound[1])
    attr(theta,'names')[ntheta] = 'logitRho'
    rs = rep(1, times = nN)
    if(rowStand) rs = apply(V,1,sum)
    if(model == 'CAR')  V = diag(rs) - rho*V
    if(model == 'SAR') V = (diag(nN) - rho*(1/rs)*V) %*%
      (diag(nN) - rho*t((1/rs)*V))
  }
  if(indComp & any(c(!is.null(Nmat), !is.null(distMat),!is.null(crsStk)))) {
    ntheta = ntheta + 1
    relEps = exp(theta[ntheta])
    if(relEps == 1) relEps = 1.1
    attr(theta,'names')[ntheta] = 'relEps'
    V = V -  V %*% solve(V + diag(rep(1/relEps,times = nN)),V)
  }
  if(indComp & !any(c(!is.null(Nmat), !is.null(distMat),!is.null(crsStk)))) {
    V = diag(nN)
  }
  WMi = V
  WMi.oo = WMi[indSamp,indSamp] 
  WMi.uu = WMi[!indSamp,!indSamp]
  WMi.uo = WMi[!indSamp,indSamp]
  WMi.ou = WMi[indSamp,!indSamp]
  Vi.oo = WMi.oo - WMi.ou %*% solve(WMi.uu, WMi.uo)
  XVi = t(X) %*% Vi.oo
  covbi = XVi %*% X
  covb = solve(covbi)
  bHat = covb %*% XVi %*% y
  r = y - X %*% bHat
  n = length(y)
  p = length(X[1,])
  m2LL = n*log(t(r) %*% Vi.oo %*% r) - 
    as.numeric(determinant(Vi.oo, logarithm = TRUE)$modulus) +
    n*(log(2*pi) + 1 - log(n))
 attr(m2LL,'covParms') = theta
 as.numeric(m2LL)
}


