#-------------------------------------------------------------------------------
#
#         lmspar
#
#-------------------------------------------------------------------------------

#' fit a linear model with spatial CAR and SAR covariances using maximum likelihood
#'
#' fit a linear model with spatial CAR and SAR covariances using maximum likelihood
#'
#' @param formula an R formula
#' @param data a data.frame containing the data
#' @param indComp should an independent component be added to the model
#' @param Nmat neighborhood matrix of binary values, with a 1 indicating a neighbor, 0 otherwise
#' @param distMat Euclidean distance matrix among polygon centroids
#' @param crsStk indicator matrix of whether locations cross stock boundaries
#' @param indSamp indicator vector for wheter location was sampled. Zero indicates missing value
#' @param model either 'CAR' or 'SAR'.  Only used if one of Nmat, distMat, or crsStk are not NULL.
#' @param rowStand should row-standardization be used on CAR or SAR models?  Default is TRUE.
#'
#' @return a list with parmEst as the estimated covariance parameters (with overall variance profiled out), sigma2 as the estimate of the overall variance parameter, m2LL as the minimized negative 2 times the log-likelihood, bHat as the fitted fixed effect parameters, covb as the estimated covariance matrix of the fitted fixed effect parameters, Vi is the inverse of the covariance matrix, Vi.oo as the inverse of the covariance matrix of the observed locations only, X returns the design matrix, and y returns the data vector.
#'
#' @author Jay Ver Hoef
#' @rdname lmspar
#' @export lmspar

lmspar = function(formula, data, indComp = FALSE, Nmat = NULL, distMat = NULL,
  crsStk = NULL, indSamp, model = NULL, rowStand = TRUE, rhoBound = c(-1,1))
{
  mfy = model.frame(formula, data = data)
  mty = attr(mfy, 'terms')
  y = mfy[[1]]
  X = model.matrix(mty, mfy, contrasts)
  ntheta = sum(c(indComp,!is.null(model),!is.null(distMat),!is.null(crsStk)))
  if(ntheta == 0) {
    indComp = TRUE
  }
  thetaGridList = NULL
  #optimize once overall, then use optimum value but start over with each starting value
  if(ntheta == 1) {
    optOut = optimize(m2LL, interval = c(-10,10), X = X, y = y, 
      indComp = indComp, Nmat = Nmat, distMat = distMat, crsStk = crsStk,
      indSamp = indSamp, model = model, rowStand = rowStand, rhoBound = rhoBound)
    theta = optOut$minimum
    m2LL = optOut$objective
  } else {
    optOut = optim(rep(0, times = ntheta), m2LL, X = X, y = y, 
        indComp = indComp, Nmat = Nmat, distMat = distMat, crsStk = crsStk,
        indSamp = indSamp, model = model, rowStand = rowStand, rhoBound = rhoBound)
    theta = optOut$par
    m2LL = optOut$value
  }
  nn = length(y)
  nN = length(indSamp)
  V = matrix(1, nrow = nN, ncol = nN )
  diag(V) = 0
  itheta = 0
  if(!is.null(Nmat)) {
    V = as(Nmat, 'sparseMatrix')
  }
  if(!is.null(crsStk)) {
    itheta = itheta + 1
    attr(theta,'names')[itheta] = 'logCrsStk'
    V = V * exp(-crsStk/exp(theta[itheta]))
  }
  if(!is.null(distMat)) {
    itheta = itheta + 1
    attr(theta,'names')[itheta] = 'logDist'
    V = V * exp(-distMat/exp(theta[itheta]))
  }
  if(!is.null(model)) {
    itheta = itheta + 1
    rho = rhoBound[1] + .00005 + exp(theta[ntheta])/
      (1 + exp(theta[ntheta]))*.9999*(rhoBound[2] - rhoBound[1])
    attr(theta,'names')[itheta] = 'logitRho'
    rs = rep(1, times = nN)
    if(rowStand) rs = apply(V,1,sum)
    if(model == 'CAR')  V = diag(rs) - rho*V
    if(model == 'SAR') V = (diag(nN) - rho*(1/rs)*V) %*%
      (diag(nN) - rho*t((1/rs)*V))
  }
  if(indComp & any(c(!is.null(Nmat), !is.null(distMat),!is.null(crsStk)))) {
    itheta = itheta + 1
    relEps = exp(theta[itheta])
    attr(theta,'names')[itheta] = 'relEps'
    V = V -  V %*% solve(V + diag(rep(relEps,times = nN)),V)
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
  r = as.matrix(y - X %*% bHat)
  n = length(y)
  p = length(X[1,])
  sigma = as.numeric((t(r) %*% Vi.oo %*% r)/n)
  return(list(m2LL = m2LL, parmEst = theta, bHat = bHat, covb = sigma*covb, 
    sigma = sigma, Vi = V/sigma, Vi.oo = Vi.oo/sigma, y = y, X = X)) 
}

