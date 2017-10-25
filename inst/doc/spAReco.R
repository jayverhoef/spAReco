## ----echo = FALSE, include = FALSE---------------------------------------
# smaller font size for chunks
setwd('/mnt/Hitachi2GB/00NMML/activePapers/spAReco/spAReco_package/spAReco/inst/doc')
opts_chunk$set(size = 'footnotesize')
opts_chunk$set(fig.path = ".")

## ----Fig-3x3Graph,  fig.width=7, fig.height=7, echo=FALSE, include=FALSE, cache=FALSE----
  plot(c(0,1),c(0,1), type = 'n', axes = FALSE, xlab = '',
	  ylab = '')
  text(x = c(.1,.5,.9,.1,.5,.9,.1,.5,.9), 
		y = c(.9,.9,.9,.5,.5,.5,.1,.1,.1),
	  labels = as.character(1:9), cex = 5)
  points(x = c(.1,.5,.9,.1,.5,.9,.1,.5,.9), 
		y = c(.9,.9,.9,.5,.5,.5,.1,.1,.1),
	  cex = 14, lwd = 3)
  lines(c(.2,.4),c(.9,.9), lwd = 3)
  lines(c(.6,.8),c(.9,.9), lwd = 3)
  lines(c(.2,.4),c(.5,.5), lwd = 3)
  lines(c(.6,.8),c(.5,.5), lwd = 3)
  lines(c(.2,.4),c(.1,.1), lwd = 3)
  lines(c(.6,.8),c(.1,.1), lwd = 3)
  lines(c(.1,.1),c(.61,.79), lwd = 3)
  lines(c(.5,.5),c(.61,.79), lwd = 3)
  lines(c(.9,.9),c(.61,.79), lwd = 3)
  lines(c(.1,.1),c(.21,.39), lwd = 3)
  lines(c(.5,.5),c(.21,.39), lwd = 3)
  lines(c(.9,.9),c(.21,.39), lwd = 3)

## ----echo = FALSE, include = FALSE, cache = FALSE------------------------
  system("pdfcrop _Fig-3x3Graph-1.pdf Fig-3x3Graph.pdf")

## ----Tab-Ex9W, echo= FALSE, include = FALSE------------------------------
  W9 = matrix(c(0,1,0,1,0,0,0,0,0,
		1,0,1,0,1,0,0,0,0,
		0,1,0,0,0,1,0,0,0,
		1,0,0,0,1,0,1,0,0,
		0,1,0,1,0,1,0,1,0,
		0,0,1,0,1,0,0,0,1,
		0,0,0,1,0,0,0,1,0,
		0,0,0,0,1,0,1,0,1,
		0,0,0,0,0,1,0,1,0),
		nrow = 9, byrow = TRUE)
  string = c(1,2,3,4,5,6,7,8,9)
  rownames(W9) = string

## ----results = 'asis', echo = FALSE--------------------------------------
   library(xtable)
  print(
    xtable(W9, 
      align = c('l',rep('l', times = length(W9[1,]))),
      digits = rep(0, times = 10),
      caption = '',
      label = 'tab:covList'
    ),
		hline.after = NULL,
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ----results = 'asis', echo = FALSE--------------------------------------
   library(xtable)
  print(
    xtable(solve(diag(9)- .2*W9), 
      align = c('l',rep('l', times = length(W9[1,]))),
      digits = rep(2, times = 10),
      caption = '',
      label = 'tab:covList'
    ),
		hline.after = NULL,
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ----results = 'asis', echo = FALSE--------------------------------------
   library(xtable)
  print(
    xtable(solve((diag(9)- .2*W9)%*%(diag(9) - t(.2*W9))), 
      align = c('l',rep('l', times = length(W9[1,]))),
      digits = rep(2, times = 10),
      caption = '',
      label = 'tab:covList'
    ),
		hline.after = NULL,
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ----results = 'asis', echo = FALSE--------------------------------------
   library(xtable)
  print(
    xtable(1/apply(W9,1,sum)*W9, 
      align = c('l',rep('l', times = length(W9[1,]))),
      digits = rep(2, times = 10),
      caption = '',
      label = 'tab:covList'
    ),
		hline.after = NULL,
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ----results = 'asis', echo = FALSE--------------------------------------
   library(xtable)
  print(
    xtable(solve(diag(apply(W9,1,sum)) - 0.8*W9), 
      align = c('l',rep('l', times = length(W9[1,]))),
      digits = rep(2, times = 10),
      caption = '',
      label = 'tab:covList'
    ),
		hline.after = NULL,
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ----echo = FALSE, include = FALSE---------------------------------------
  library(sp)
  library(spdep)
  library(spAReco)
  library(xtable)
  library(coda)
	library(maps)
	library(shape)

## ----Fig-Stocks, fig.width=12, fig.height=12, echo= FALSE, include = FALSE, cache = TRUE----
  data(pvpolys2)
  colPalStock = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0')
	colPalStock = c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba')
  plot(pvpolys2, col = colPalStock[pvpolys2@data$stockid - 7])
  text(980000,1085000,'1', cex = 6)
  text(1123764,1130000,'2', cex = 6)
  text(1134476,948464,'3', cex = 6)
  text(1230000,810000,'4', cex = 6)
  text(1380000,930000,'5', cex = 6)
	Arrows(x0 = 1346585, y0 = 1045586, x1 = 1346585, y1 = 1120000, lwd = 6)
	text(1346585,1150000,labels = 'N', cex = 4)
	par(usr = c(-180,-50,51.4,105))
	map("world", c("USA:Alaska"), add = TRUE, lwd = 2)
	lines(c(-138,-129),c(59.5,59.5), lwd = 3, col = 'red')
	lines(c(-138,-129),c(54.5,54.5), lwd = 3, col = 'red')
	lines(c(-138,-138),c(54.5,59.5), lwd = 3, col = 'red')
	lines(c(-129,-129),c(54.5,59.5), lwd = 3, col = 'red')

## ----Fig-MapRaw, fig.width=12, fig.height=12, echo= FALSE, include = FALSE, cache = TRUE----
  data(pvpolys2)
  data(d8out1)
  data(d9out1)
  data(d10out1)
  data(d11out1)
  data(d12out1)
#  col.pal7 = c('#d73027','#fc8d59','#fee08b','#ffffbf','#d9ef8b','#91cf60','#1a9850')
	col.pal7 = c("#440154FF", "#443A83FF", "#31688EFF", "#21908CFF", "#35B779FF", "#8FD744FF", "#FDE725FF")
  missingCol = 'grey80'
  coords = coordinates(pvpolys2)
  d8plots = pvpolys2[pvpolys2@data$polyid %in% d8out1$polyid,]
  d9plots = pvpolys2[pvpolys2@data$polyid %in% d9out1$polyid,]
  d10plots = pvpolys2[pvpolys2@data$polyid %in% d10out1$polyid,]
  d11plots = pvpolys2[pvpolys2@data$polyid %in% d11out1$polyid,]
  d12plots = pvpolys2[pvpolys2@data$polyid %in% d12out1$polyid,]
  brks7 = c(-5, -.15, -.07, -.02, .02, .07, .15, 5)
#  par(bg = 'grey70')
  par(mar = c(0,0,0,0))
  plot(pvpolys2, col = missingCol)
  points(coords, pch = 19, cex = 2, col = missingCol)
  #  xlim = c(896054, 1498427),ylim = c(712289, 1216913))
  plot(d8plots, add = TRUE, col = col.pal7[as.integer(cut(d8out1$Estimate, 
    breaks = brks7))])
  points(coordinates(d8plots), pch = 19, cex = 2, 
    col = col.pal7[as.integer(cut(d8out1$Estimate, breaks = brks7))])
  plot(d9plots, add = TRUE, col = col.pal7[as.integer(cut(d9out1$Estimate, 
    breaks = brks7))])
  points(coordinates(d9plots), pch = 19, cex = 2, 
    col = col.pal7[as.integer(cut(d9out1$Estimate, breaks = brks7))])
  plot(d10plots, add = TRUE, col = col.pal7[as.integer(cut(d10out1$Estimate, 
    breaks = brks7))])
  points(coordinates(d10plots), pch = 19, cex = 2, 
    col = col.pal7[as.integer(cut(d10out1$Estimate, breaks = brks7))])
  plot(d11plots, add = TRUE, col = col.pal7[as.integer(cut(d11out1$Estimate, 
    breaks = brks7))])
  points(coordinates(d11plots), pch = 19, cex = 2, 
    col = col.pal7[as.integer(cut(d11out1$Estimate, breaks = brks7))])
  plot(d12plots, add = TRUE, col = col.pal7[as.integer(cut(d12out1$Estimate, 
    breaks = brks7))])
  points(coordinates(d12plots), pch = 19, cex = 2, 
    col = col.pal7[as.integer(cut(d12out1$Estimate, breaks = brks7))])
 
  brks7[1] = min(d8out1$Estimate, d9out1$Estimate, d10out1$Estimate,
    d11out1$Estimate,d12out1$Estimate) - .00001
  brks7[8] = max(d8out1$Estimate, d9out1$Estimate, d10out1$Estimate,
    d11out1$Estimate,d12out1$Estimate)
  addBreakColorLegend(1304916, 981392, 1357619, 1198651, brks7, 
    colors = col.pal7, cex = 1.5, printFormat = "4.3")

  nTot = length(pvpolys2@data[,1])
  nObs = length(d8plots@data[,1]) + length(d9plots@data[,1]) + length(d10plots@data[,1]) + length(d11plots@data[,1]) + length(d12plots@data[,1])
  nMiss = nTot - nObs

## ----Fig-Neighbors, fig.width=12, fig.height=12, echo= FALSE, include = FALSE, cache = TRUE----
  mtextadj = -.04
	mtextpadj = -.4
  data(pvpolys2)
  coords = coordinates(pvpolys2)
  Nlist = poly2nb(pvpolys2, snap = 2000)
  Nlist[[79]] = as.integer(c(211, 463))
  Nlist[[211]] = as.integer(c(Nlist[[211]]), 79)
  Nlist[[463]] = as.integer(c(Nlist[[463]]), 79)
  Nlist[[130]] = as.integer(302)
  Nlist[[302]] = as.integer(c(Nlist[[302]],130))
  Nlist[[325]] = as.integer(c(326, 353))
  Nlist[[326]] = as.integer(c(Nlist[[326]],325))
  Nlist[[353]] = as.integer(c(Nlist[[353]],325))
  Nlist[[435]] = as.integer(437)
  Nlist[[437]] = as.integer(c(Nlist[[437]],435))
  Nlist[[436]] = as.integer(c(86, 88))
  Nlist[[86]] = as.integer(c(Nlist[[86]],436))
  Nlist[[88]] = as.integer(c(Nlist[[88]],436))
  Nlist[[437]] = as.integer(87)
  Nlist[[87]] = as.integer(c(Nlist[[87]],437))
  Nlist[[438]] = as.integer(436)
  Nlist[[436]] = as.integer(c(Nlist[[436]],438))
  Nlist[[439]] = as.integer(346)
  Nlist[[346]] = as.integer(c(Nlist[[346]],439))
  Nlist[[443]] = as.integer(281)
  Nlist[[281]] = as.integer(c(Nlist[[281]],443))
  Nlist[[463]] = as.integer(79)
  attr(Nlist,'polyid') = as.factor(as.character(pvpolys2@data$polyid))
  attr(Nlist,'stockid') = as.factor(as.character(pvpolys2@data$stockid))
  num = lapply(Nlist, function(x) length(x))
  num = unlist(num)
  Nmat = Neighmat(Nlist, num, length(num))
  Nmat1 = pmax(Nmat,t(Nmat))
  Nmat2 = (Nmat1 %*% Nmat1 > 0 | Nmat1 > 0)*1
  diag(Nmat2) = 0
  Nmat4 = (Nmat2 %*% Nmat2 > 0 | Nmat2 > 0)*1
  diag(Nmat4) = 0
  xleft = 1268000
  xright = 1390000
  ybottom = 738000
  ytop = 800000
  xexp = (xright - xleft)*.06
  yexp = (ytop - ybottom)*.35
  layout(matrix(1:4, nrow = 2, byrow = TRUE))
  par(mar = c(0,1,5,1))
  plot(pvpolys2)
#  lines(c(xleft - xexp, xright + xexp), c(ybottom - yexp, ybottom - yexp), lty = 2, lwd = 2) #, col = 'red')
#  lines(c(xleft - xexp, xright + xexp), c(ytop + yexp, ytop + yexp), lty = 2, lwd = 2) #, col = 'red')
#  lines(c(xleft - xexp, xleft - xexp), c(ybottom - yexp, ytop + yexp), lty = 2, lwd = 2) #, col = 'red')
#  lines(c(xright + xexp, xright + xexp), c(ybottom - yexp, ytop + yexp), lty = 2, lwd = 1) #, col = 'red')
	rect(xleft - xexp, ybottom - yexp, xright + xexp, ytop + yexp, col = 'grey80', border = NA)
  plot(Nlist, coords, add = TRUE, lwd = 2)
  mtext('(a)', adj = mtextadj, padj = mtextpadj, cex = 4)

  plot(pvpolys2, xlim = c(xleft, xright), ylim = c(ybottom, ytop))
  coords = coordinates(pvpolys2)
  plot(Nlist, coords, add = TRUE, lwd = 2)
  mtext('(b)', adj = mtextadj, padj = mtextpadj, cex = 4)  

  Nlist2 = apply(Nmat2, 1, function(x) as.integer(which(x > 0)))
  class(Nlist2) = 'nb'
  attr(Nlist2,'type') = 'queen'
  attr(Nlist2,'sym') = TRUE
  attr(Nlist2,'polyid') = attr(Nlist,'polyid')
  attr(Nlist2,'stockid') = attr(Nlist,'polyid')
  plot(pvpolys2, xlim = c(xleft, xright), ylim = c(ybottom, ytop))
  plot(Nlist2, coords, add = TRUE, lwd = 1)
  mtext('(c)', adj = mtextadj, padj = mtextpadj, cex = 4)

  Nlist4 = apply(Nmat4, 1, function(x) as.integer(which(x > 0)))
  class(Nlist4) = 'nb'
  attr(Nlist4,'type') = 'queen'
  attr(Nlist4,'sym') = TRUE
  attr(Nlist4,'polyid') = attr(Nlist,'polyid')
  attr(Nlist4,'stockid') = attr(Nlist,'polyid')
  plot(pvpolys2, xlim = c(xleft, xright), ylim = c(ybottom, ytop))
  plot(Nlist4, coords, add = TRUE, lwd = .5)
  mtext('(d)', adj = mtextadj, padj = mtextpadj, cex = 4)

## ----Data-ModelsM2LL, echo= FALSE, include = FALSE, cache = TRUE---------
  data(pvpolys2)
  data(d8out1)
  data(d9out1)
  data(d10out1)
  data(d11out1)
  data(d12out1)
  Dchange = rbind(d8out1, d9out1, d10out1, d11out1, d12out1)
  Dchange$stockid = as.factor(Dchange$stockid)
  Dchange$polyid = as.factor(as.character(Dchange$polyid))
  rownames(Dchange) = 1:length(Dchange[,1])
  AllPolyCentroids = data.frame(x = coordinates(pvpolys2)[,1], 
    y = coordinates(pvpolys2)[,2], stockid = as.factor(as.character(pvpolys2@data$stockid)),
    polyid = as.factor(as.character(pvpolys2@data$polyid)))
  formz = x ~ I(as.factor(stockid)) - 1
  mf <- model.frame(formz, data = AllPolyCentroids)
  mt <- attr(mf, "terms")
  Z <- model.matrix(mt, mf)
  ZZ = (Z %*% t(Z) > 0)*1
  crsStk = 1 - ZZ
  indSamp = AllPolyCentroids$polyid %in% Dchange$polyid
  ordD = match(AllPolyCentroids$polyid[indSamp],
    as.character(Dchange$polyid))
  Dchange = Dchange[ordD,]
  distMat = as.matrix(dist(AllPolyCentroids[,c('x','y')]))/1000
  distMat1 = distMat*Nmat
  rownames(distMat1) = attr(Nlist,'polyid')
  colnames(distMat1) = attr(Nlist,'polyid')
  distMat2 = distMat*Nmat2
  rownames(distMat2) = attr(Nlist,'polyid')
  colnames(distMat2) = attr(Nlist,'polyid')
  distMat4 = distMat*Nmat4
  rownames(distMat4) = attr(Nlist,'polyid')
  colnames(distMat4) = attr(Nlist,'polyid')

#  lm1 =  lm(Estimate ~ 1, data = Dchange)
#  summary(lm1)
#  logLik(lm1)
  spar1 = lmspar(Estimate ~ 1, data = Dchange,
    indSamp = indSamp)
  spar1CAR1rs = lmspar(Estimate ~ 1, data = Dchange, Nmat = Nmat1, 
    model = 'CAR', indSamp = indSamp)
#  lmX =  lm(Estimate ~ stockid, data = Dchange)
#  summary(lmX)
#  logLik(lmX)
  sparXI = lmspar(Estimate ~ stockid, data = Dchange,
    indSamp = indSamp)
  sparXCAR1rs = lmspar(Estimate ~ stockid, data = Dchange, Nmat = Nmat1, 
    model = 'CAR', indSamp = indSamp)
  sparXCAR1 = lmspar(Estimate ~ stockid, data = Dchange, Nmat = Nmat1, 
    model = 'CAR', indSamp = indSamp, rowStand = FALSE, 
    rhoBound = c(1/min(eigen(Nmat1)$values),1/max(eigen(Nmat1)$values)))
  sparXSAR1rs = lmspar(Estimate ~ stockid, data = Dchange, Nmat = Nmat1, 
    model = 'SAR', indSamp = indSamp)
  sparXSAR1 = lmspar(Estimate ~ stockid, data = Dchange, Nmat = Nmat1, 
    model = 'SAR', indSamp = indSamp, rowStand = FALSE, 
    rhoBound = c(1/min(eigen(Nmat1)$values),1/max(eigen(Nmat1)$values)))
  sparXCAR2rs = lmspar(Estimate ~ stockid, data = Dchange, Nmat = Nmat2, 
    model = 'CAR', indSamp = indSamp)
  sparXCAR4rs = lmspar(Estimate ~ stockid, data = Dchange, Nmat = Nmat4, 
    model = 'CAR', indSamp = indSamp)
  sparXCAR4 = lmspar(Estimate ~ stockid, data = Dchange, Nmat = Nmat4, 
    model = 'CAR', indSamp = indSamp, rowStand = FALSE,
    rhoBound = c(1/min(eigen(Nmat4)$values),1/max(eigen(Nmat4)$values)))
  sparXCAR4rsDist = lmspar(Estimate ~ stockid, data = Dchange, Nmat = Nmat4, 
    model = 'CAR', distMat = distMat4, indSamp = indSamp)
  sparXCAR4rsDistCrs = lmspar(Estimate ~ stockid, data = Dchange, Nmat = Nmat4, 
    model = 'CAR', distMat = distMat4, crsStk = crsStk, indSamp = indSamp)
  sparXCAR4rsDistInd = lmspar(Estimate ~ stockid, data = Dchange, Nmat = Nmat4, 
    model = 'CAR', distMat = distMat4, indComp = TRUE, indSamp = indSamp)
  spar1CAR4rsCrs = lmspar(Estimate ~ 1, data = Dchange, Nmat = Nmat4, 
    model = 'CAR', crsStk = crsStk, indSamp = indSamp)
  allm2LLs = c(-spar1$m2LL, -spar1CAR1rs$m2LL, -sparXI$m2LL, -sparXCAR1rs$m2LL, 
    -sparXCAR1$m2LL, -sparXSAR1rs$m2LL, -sparXSAR1$m2LL, -sparXCAR2rs$m2LL, 
    -sparXCAR4rs$m2LL, -sparXCAR4$m2LL, -sparXCAR4rsDist$m2LL, -sparXCAR4rsDistCrs$m2LL,
    -sparXCAR4rsDistInd$m2LL)

## ----Fig-ModelsM2LL, fig.width = 12, fig.heigh = 6, echo= FALSE, include = FALSE----
par(mar = c(5,5,1,1))
plot(2:13, allm2LLs[2:13], pch = 19, cex = 1.5, ylab = '2*log-likelihood', xaxt = 'n', 
  xlab = 'Model Parameters', ylim = c(375.2,415.5), xlim = c(1.5,13.5), cex.lab = 1.8, cex.axis = 1.5)
for(i in -1:40)
  lines(c(2,13), c(allm2LLs[3]+2*i,allm2LLs[3]+2*i), lty = 2, col = 'grey80') 
axis(1, at = c(2:13), labels = c('3','6','7','7','7','7','7','7','7','8','9','9'),
   cex.axis = 1.5)
textcex = 2
txtlabels = c('mC1R','XU','XC1R','XC1','XS1R','XS1','XC2R','XC4R','XC4','XC4RD','XC4RDS','XC4RDU')
for(i in c(2:5,7:11,13)) text(i,allm2LLs[i],txtlabels[i-1], pos = 3, 
  cex = textcex, family = 'mono', font = 2)
for(i in c(6,12)) text(i,allm2LLs[i],txtlabels[i-1], pos = 1, 
  cex = textcex, family = 'mono', font = 2)

## ----Data-rhoProfile, echo= FALSE, include = FALSE, cache = TRUE---------
  mfy = model.frame(Estimate ~ stockid, data = Dchange)
  mty = attr(mfy, 'terms')
  y = mfy[[1]]
  X = model.matrix(mty, mfy, contrasts)

prolik1 = NULL
for(i in -49:49)
  prolik1 = rbind(prolik1, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat1, crsStk = NULL, 
      indSamp = indSamp, model = 'CAR', rowStand = TRUE, rhoBound = c(-1,1))))
prolik2 = NULL
for(i in -49:49)
  prolik2 = rbind(prolik2, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat2, crsStk = NULL, 
      indSamp = indSamp, model = 'CAR', rowStand = TRUE, rhoBound = c(-1,1))))

prolik4 = NULL
for(i in -49:49)
  prolik4 = rbind(prolik4, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat4, crsStk = NULL, 
      indSamp = indSamp, model = 'CAR', rowStand = TRUE, rhoBound = c(-1,1))))

mlogitr = prolik4[which(prolik4[,2] == max(prolik4[,2])),1]
mvalCAR4R = -1 + exp(mlogitr)/(1+exp(mlogitr))*2

nnn = length(prolik4[,1])
lindx = which(prolik4[1:(nnn-1),2] < (max(prolik4[,2]) - 3.841) &
  prolik4[2:nnn,2] > (max(prolik4[,2]) - 3.841))
prolik4[lindx,1]
prolik4[lindx + 1,1]
rlow = (max(prolik4[,2]) - 3.841 - prolik4[lindx,2])/
  (prolik4[lindx + 1,2] - prolik4[lindx,2])
logitLCI = prolik4[lindx,1] + rlow*(prolik4[lindx+1,1] - prolik4[lindx,1])
LCI = -1 + exp(logitLCI)/(1+exp(logitLCI))*2
uindx = which(prolik4[1:(nnn-1),2] > (max(prolik4[,2]) - 3.841) &
  prolik4[2:nnn,2] < (max(prolik4[,2]) - 3.841))
prolik4[uindx,1]
prolik4[uindx + 1,1]
rupp = 1 - (max(prolik4[,2]) - 3.841 - prolik4[uindx + 1,2])/
  (prolik4[uindx,2] - prolik4[uindx + 1,2])
logitUCI = prolik4[uindx+1,1] + rupp*(prolik4[uindx,1] - prolik4[uindx+1,1])
UCI = -1 + exp(logitUCI)/(1+exp(logitUCI))*2

prolik1Un = NULL
eV1 = eigen(Nmat1)$values
rB1min = 1/min(eV1)
rB1max = 1/max(eV1)
for(i in -49:49)
  prolik1Un = rbind(prolik1Un, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat1, crsStk = NULL, 
      indSamp = indSamp, model = 'CAR', rowStand = FALSE, 
      rhoBound = c(rB1min,rB1max))))

prolik2Un = NULL
eV2 = eigen(Nmat2)$values
rB2min = 1/min(eV2)
rB2max = 1/max(eV2)
for(i in -49:49)
  prolik2Un = rbind(prolik2Un, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat2, crsStk = NULL, 
      indSamp = indSamp, model = 'CAR', rowStand = FALSE, 
      rhoBound = c(rB2min,rB2max))))

prolik4Un = NULL
eV4 = eigen(Nmat4)$values
rB4min = 1/min(eV4)
rB4max = 1/max(eV4)
for(i in -49:49)
  prolik4Un = rbind(prolik4Un, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat4, crsStk = NULL, 
      indSamp = indSamp, model = 'CAR', rowStand = FALSE, 
      rhoBound = c(rB4min,rB4max))))


prolikSAR1 = NULL
for(i in -49:49)
  prolikSAR1 = rbind(prolikSAR1, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat1, crsStk = NULL, 
      indSamp = indSamp, model = 'SAR', rowStand = TRUE, rhoBound = c(-1,1))))

prolikSAR2 = NULL
for(i in -49:49)
  prolikSAR2 = rbind(prolikSAR2, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat2, crsStk = NULL, 
      indSamp = indSamp, model = 'SAR', rowStand = TRUE, rhoBound = c(-1,1))))

prolikSAR4 = NULL
for(i in -49:49)
  prolikSAR4 = rbind(prolikSAR4, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat4, crsStk = NULL, 
      indSamp = indSamp, model = 'SAR', rowStand = TRUE, rhoBound = c(-1,1))))

prolikSAR1Un = NULL
eV1 = eigen(Nmat1)$values
rB1min = 1/min(eV1)
rB1max = 1/max(eV1)
for(i in -49:49)
  prolikSAR1Un = rbind(prolikSAR1Un, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat1, crsStk = NULL, 
      indSamp = indSamp, model = 'SAR', rowStand = FALSE, 
      rhoBound = c(rB1min,rB1max))))

prolikSAR2Un = NULL
eV2 = eigen(Nmat2)$values
rB2min = 1/min(eV2)
rB2max = 1/max(eV2)
for(i in -49:49)
  prolikSAR2Un = rbind(prolikSAR2Un, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat2, crsStk = NULL, 
      indSamp = indSamp, model = 'SAR', rowStand = FALSE, 
      rhoBound = c(rB2min,rB2max))))

prolikSAR4Un = NULL
eV4 = eigen(Nmat4)$values
rB4min = 1/min(eV4)
rB4max = 1/max(eV4)
for(i in -49:49)
  prolikSAR4Un = rbind(prolikSAR4Un, data.frame(rho = i/5,
    m2LLrho = -m2LL(i/5, X,y,indComp = FALSE, Nmat = Nmat4, crsStk = NULL, 
      indSamp = indSamp, model = 'SAR', rowStand = FALSE, 
      rhoBound = c(rB4min,rB4max))))

path1 = paste0(system.file('bugs/codaCAR',package = 'spAReco'),'/')
beta0CAR = read.coda(paste0(path1,'beta0','1.txt'), paste0(path1,'beta0', 'Index.txt'))
beta1CAR = read.coda(paste0(path1,'beta','1.txt'), paste0(path1,'beta', 'Index.txt'))
sigmaZCAR = read.coda(paste0(path1,'sigmaZ','1.txt'), paste0(path1,'sigmaZ', 'Index.txt'))
rho = read.coda(paste0(path1,'rho','1.txt'), paste0(path1,'rho', 'Index.txt'))
muSmooCARBayes = read.coda(paste0(path1,'mu','1.txt'), paste0(path1,'mu', 'Index.txt'))
muSmooCARBayesSum = summary(muSmooCARBayes)
muSmoCARBayesEst = muSmooCARBayesSum$statistics[,'Mean']

## ----Fig-rhoProfile, echo= FALSE, include = FALSE------------------------
colCAR1 = '#1b9e77'
colCAR2 = '#d95f02'
colCAR4 = '#7570b3'
colBayes = '#e7298a'
colCAR1 = 'black'
colCAR2 = 'black'
colCAR4 = 'black'
colBayes = 'black'
par(mar = c(5,5,1,5))
plot(-1 + exp(prolik1[,1])/(1+exp(prolik1[,1]))*2,prolik1[,2], type = 'l', lty = 1,
  col = colCAR1, xlab = expression(rho), ylab = '2*log-likelihood', ylim = c(400,412), 
  lwd = 4, cex.lab = 2, cex.axis = 1.5, xlim = c(-1,1))
lines(-1 + exp(prolik2[,1])/(1+exp(prolik2[,1]))*2,prolik2[,2], col = colCAR2, lwd = 4, lty = 2)
lines(-1 + exp(prolik4[,1])/(1+exp(prolik4[,1]))*2,prolik4[,2],col = colCAR4, lwd = 4, lty = 4)
lines(c(-1,1), rep(-sparXCAR4rs$m2LL - 3.84, times = 2), lty = 3, col = 'black', lwd = 3)
legend(-1,412, lty = c(1,2,4,5), lwd = rep(1.5, times = 4), col = c(colCAR1, colCAR2, colCAR4, colBayes),
   legend = c('XC1R-MLE', 'XC2R-MLE', 'XC4R-MLE','XC4R-MCMC'))
par(new = TRUE)
plot(density(rho, bw = .05), type = 'l', axes = FALSE, bty = 'n', 
  main = '', xlab = '', ylab = '', col = colBayes, lwd = 4, xlim = c(-1,1), lty = 5)
axis(side=4, at = c(0,1,2), cex.axis = 1.5)
mtext("Posterior Density", side=4, line=3, cex = 2)

## ----Data-thetaProfiles, echo= FALSE, include = FALSE, cache = TRUE------
  m2LLFixTheta1 = function(thetaOthers, theta1, X, y, indComp=FALSE, 
	  Nmat=NULL, distMat=NULL, crsStk=NULL, indSamp, model=NULL, 
	  rowStand=NULL, rhoBound = c(-1,1))
  {
    theta = c(theta1,thetaOthers)
    m2LL(theta, X=X, y=y, indComp=indComp, Nmat=Nmat, distMat=distMat, 
	    crsStk=crsStk, indSamp=indSamp, model=model, rowStand=rowStand,
		  rhoBound=rhoBound)
  }

  m2LLFixTheta2 = function(thetaOthers, theta2, X, y, indComp=FALSE, 
	  Nmat=NULL, distMat=NULL, crsStk=NULL, indSamp, model=NULL, 
	  rowStand=NULL, rhoBound = c(-1,1))
  {
    theta = c(theta2,thetaOthers)
    m2LL(theta, X=X, y=y, indComp=indComp, Nmat=Nmat, distMat=distMat, 
	    crsStk=crsStk, indSamp=indSamp, model=model, rowStand=rowStand,
		  rhoBound=rhoBound)
  }

  mfy = model.frame(as.formula(Estimate ~ stockid), data = Dchange)
  mty = attr(mfy, 'terms')
  y = mfy[[1]]
  X = model.matrix(mty, mfy, contrasts)

  theta1Prof = NULL
  for(i in c(-5:-2,(-9:14)/5,3:10)) theta1Prof = rbind(theta1Prof, c(i, 
	  optim(sparXCAR4rsDistCrs$parmEst[2:3], m2LLFixTheta1, theta1 = i, 
	  X=X, y=y, Nmat = Nmat4, distMat = distMat4,
	  crsStk = crsStk, indSamp = indSamp, model = 'CAR', 
		rowStand = TRUE)$value))

	theta2Prof = NULL
	for(i in c((11:24)/5, 5:10)) theta2Prof = rbind(theta2Prof, c(i, 
	  optimize(m2LLFixTheta2, interval = c(-10,10), theta2 = i, 
	  X=X, y=y, Nmat = Nmat4, distMat = distMat4,
	  crsStk = NULL, indSamp = indSamp, model = 'CAR', 
		rowStand = TRUE)$objective))

  rat = (-sparXCAR4rsDist$m2LL -3.841 - 
	  max(-theta2Prof[-theta2Prof[,2] < (-sparXCAR4rsDist$m2LL -3.841),2]))/
    (min(-theta2Prof[-theta2Prof[,2] > (-sparXCAR4rsDist$m2LL -3.841),2]) -
    max(-theta2Prof[-theta2Prof[,2] < (-sparXCAR4rsDist$m2LL -3.841),2]))
  #linear interpolator
  LCItheta2 = max(theta2Prof[-theta2Prof[,2] < (-sparXCAR4rsDist$m2LL -3.841),1]) +
	  rat*(min(theta2Prof[-theta2Prof[,2] > (-sparXCAR4rsDist$m2LL -3.841),1]) -
    max(theta2Prof[-theta2Prof[,2] < (-sparXCAR4rsDist$m2LL -3.841),1]))

  rat = (-sparXCAR4rsDistCrs$m2LL -3.841 - 
	  max(-theta1Prof[-theta1Prof[,2] < (-sparXCAR4rsDistCrs$m2LL -3.841),2]))/
    (min(-theta1Prof[-theta1Prof[,2] > (-sparXCAR4rsDistCrs$m2LL -3.841),2]) -
    max(-theta1Prof[-theta1Prof[,2] < (-sparXCAR4rsDistCrs$m2LL -3.841),2]))
  #linear interpolator
  LCItheta1 = max(theta1Prof[-theta1Prof[,2] < (-sparXCAR4rsDistCrs$m2LL -3.841),1]) +
	  rat*(min(theta1Prof[-theta1Prof[,2] > (-sparXCAR4rsDistCrs$m2LL -3.841),1]) -
    max(theta1Prof[-theta1Prof[,2] < (-sparXCAR4rsDistCrs$m2LL -3.841),1]))

## ----Fig-thetaProfiles, fig.width = 12, fig.heigh = 6, echo= FALSE, include = FALSE, cache = TRUE----
	layout(matrix(1:2, nrow = 1, byrow = TRUE))
	par(mar = c(5,5,5,3))
	plot(theta2Prof[,1],-theta2Prof[,2], type = 'l', lwd = 3,
	  xlab = expression(paste('log(',theta[2],')')), ylab = '2*log-likelihood',
		cex.axis = 1.5, cex.lab = 2, ylim = c(397,414))
	lines(c(-5,10),rep(max(-theta2Prof[,2])-3.84,times =2), lty = 2, lwd = 3)
  mtext('(a)', adj = -.28, padj = -.4, cex = 4)

	par(mar = c(5,5,5,3))
	plot(theta1Prof[,1],-theta1Prof[,2], type = 'l', lwd = 3,
	  xlab = expression(paste('log(',theta[1],')')), ylab = '2*log-likelihood',
		cex.axis = 1.5, cex.lab = 2, ylim = c(397,414))
	lines(c(-5,10),rep(max(-theta1Prof[,2])-3.84,times =2), lty = 2, lwd = 3)
  mtext('(b)', adj = -.28, padj = -.4, cex = 4)

## ----Data-Ukrige, echo= FALSE, include = FALSE, cache = TRUE-------------
  alldata = pvpolys2@data
  alldata$stockid = as.factor(alldata$stockid)
  alldata$fakey = runif(length(alldata[,1]))
  formula = Estimate ~ stockid
  formXall = formula
  formXall = update(formula, fakey ~ .)
  mfy = model.frame(formXall, data = alldata)
  mty = attr(mfy, 'terms')
  Xall = model.matrix(mty, mfy, contrasts)
	 
  fitCAR4Map = sparXCAR4rs
  Sig = solve(fitCAR4Map$Vi)
  fits = Xall %*% fitCAR4Map$bHat
  nall = length(Sig[,1])
  yall = rep(NA, times = nall)
  yall[indSamp] = fitCAR4Map$y

	i = 1
  storeCE = matrix(NA, nrow = nall, ncol = 2)
  for(i in 1:nall) {
    indi = (1:nall) != i
    covvec = Sig[i,indi]
    if(any(1:nall == i & indSamp))  ind = indSamp & indi
    if(!any(1:nall == i & indSamp)) ind = indSamp
    SigObs = Sig[ind,ind]
    covvec = Sig[i, ind]
    mui = fits[i]
    muObs = fits[ind]
    SigObsi = solve(SigObs)
    storeCE[i,1] = as.numeric(mui - covvec %*% SigObsi %*% (muObs - yall[ind]))
		d = (Xall[i,] - t(Xall[ind,]) %*% SigObsi %*% covvec)
		storeCE[i,2] = sqrt(as.numeric(t(covvec) %*% SigObsi %*% covvec + 
			t(d) %*% solve(t(Xall[ind,]) %*% SigObsi %*% Xall[ind,]) %*% d))
  }
	preds = storeCE[!indSamp,1]
	predse = storeCE[!indSamp,2]

## ----Data-IAR, echo= FALSE, include = FALSE, cache = TRUE----------------
  d4WB = AllPolyCentroids[,c('polyid','stockid')]
  d4WB = cbind(d4WB, rep(NA, times = length(d4WB[,1])))
  colnames(d4WB) = c('polyid', 'stockid','trend')
  d4WB$trend[indSamp] = Dchange$Estimate
  adj = unlist(apply(Nmat1,1,function(v){which(v == 1)}))
  num = apply(Nmat1,1,sum)
  d4WBlist = list(N = length(d4WB[,1]),
    trend = d4WB$trend,
    stockid = as.integer(d4WB$stockid),
    adj = adj,
    num = num,
    weights=rep(1, times = sum(num))
  )
#  path1 = '/mnt/Hitachi2GB/00NMML/activePapers/netCARSAR/netCARSAR_package/'
#  path2 = 'netCARSAR/inst/bugs/'
#  writeDatafileR(d4WBlist,paste0(path1,path2,'d4WBIAR.txt'))

  path1 = paste0(system.file('bugs/codaIAR',package = 'spAReco'),'/')
  beta0 = read.coda(paste0(path1,'beta0','1.txt'), paste0(path1,'beta0', 'Index.txt'))
  beta1 = read.coda(paste0(path1,'beta','1.txt'), paste0(path1,'beta', 'Index.txt'))
  sigmaZ = read.coda(paste0(path1,'sigmaZ','1.txt'), paste0(path1,'sigmaZ', 'Index.txt'))
  sigmaEps = read.coda(paste0(path1,'sigmaEps','1.txt'), paste0(path1,'sigmaEps', 'Index.txt'))
  muSmoo = read.coda(paste0(path1,'mu','1.txt'), paste0(path1,'mu', 'Index.txt'))
  muSmooSum = summary(muSmoo)
  muSmoEst = muSmooSum$statistics[,'Mean']

## ----Fig-PredSmoo, fig.width=16, fig.height=16, echo= FALSE, include = FALSE, cache = TRUE----
#  par(bg = 'grey80')
  par(mar = c(0,0,0,0))
  brks7 = c(brks7[1],-.1,-.06,-.02,.02,.06,.1,brks7[8])
  layout(matrix(1:4, nrow = 2, byrow = TRUE))
  plot(pvpolys2)
  plot(d8plots, add = TRUE, col = col.pal7[as.integer(cut(d8out1$Estimate, 
    breaks = brks7))])
  points(coordinates(d8plots), pch = 19, cex = 2, 
    col = col.pal7[as.integer(cut(d8out1$Estimate, breaks = brks7))])
  plot(d9plots, add = TRUE, col = col.pal7[as.integer(cut(d9out1$Estimate, 
    breaks = brks7))])
  points(coordinates(d9plots), pch = 19, cex = 2, 
    col = col.pal7[as.integer(cut(d9out1$Estimate, breaks = brks7))])
  plot(d10plots, add = TRUE, col = col.pal7[as.integer(cut(d10out1$Estimate, 
    breaks = brks7))])
  points(coordinates(d10plots), pch = 19, cex = 2, 
    col = col.pal7[as.integer(cut(d10out1$Estimate, breaks = brks7))])
  plot(d11plots, add = TRUE, col = col.pal7[as.integer(cut(d11out1$Estimate, 
    breaks = brks7))])
  points(coordinates(d11plots), pch = 19, cex = 2, 
    col = col.pal7[as.integer(cut(d11out1$Estimate, breaks = brks7))])
  plot(d12plots, add = TRUE, col = col.pal7[as.integer(cut(d12out1$Estimate, 
    breaks = brks7))])
  points(coordinates(d12plots), pch = 19, cex = 2, 
    col = col.pal7[as.integer(cut(d12out1$Estimate, breaks = brks7))])
  plot(pvpolys2[!indSamp,], col = col.pal7[as.integer(cut(as.numeric(as.vector(preds)), 
    breaks = brks7))], add = TRUE)
  points(coords[!indSamp,], pch = 19, cex = 2, 
      col = col.pal7[as.integer(cut(as.numeric(as.vector(preds)), 
      breaks = brks7))])
  text(x=950000,y=1220000,labels = '(a)', cex = 6)
  addBreakColorLegend(1304916, 981392, 1357619, 1198651, brks7, 
    colors = col.pal7, cex = 2.5, printFormat = "4.3")

  brks4se = c(min(as.numeric(as.vector(predse)))-1e-20,
    quantile(as.numeric(as.vector(predse)), prob = c(.25,.5,.75)),
		max(as.numeric(as.vector(predse)))-1e-20)
  col.pal4se = c('#f2f0f7','#cbc9e2','#9e9ac8','#6a51a3')
  col.pal4se = c('#cbc9e2','#9e9ac8','#756bb1','#54278f')
  col.pal4se = c('#fe9929','#ec7014','#cc4c02','#8c2d04')
  par(mar = c(0,0,0,0))
  plot(pvpolys2, col = 'black')
#  plot(pvpolys2, col = missingCol)
#  points(coords, pch = 19, cex = 2, col = missingCol)
  plot(pvpolys2[!indSamp,], col = col.pal4se[as.integer(cut(as.numeric(as.vector(predse)), 
    breaks = brks4se))], add = TRUE)
  points(coords[!indSamp,], pch = 19, cex = 2, 
      col = col.pal4se[as.integer(cut(as.numeric(as.vector(predse)), 
      breaks = brks4se))])
  text(x=950000,y=1220000,labels = '(b)',cex = 6)
  addBreakColorLegend(1304916, 981392, 1357619, 1198651, brks4se, 
    colors = col.pal4se, cex = 2.5, printFormat = "4.3")

  plot(pvpolys2, col = col.pal7[as.integer(cut(storeCE[,1], 
    breaks = brks7))])
  points(coords, pch = 19, cex = 2, col = col.pal7[as.integer(cut(storeCE[,1], 
      breaks = brks7))])
  text(x=950000,y=1220000,labels = '(c)',cex = 6)
  addBreakColorLegend(1304916, 981392, 1357619, 1198651, brks7, 
    colors = col.pal7, cex = 2.5, printFormat = "4.3")

	plot(pvpolys2, col = col.pal7[as.integer(cut(muSmoEst, 
    breaks = brks7))])
  points(coords, pch = 19, cex = 2, col = col.pal7[as.integer(cut(muSmoEst, 
      breaks = brks7))])
  text(x=950000,y=1220000,labels = '(d)',cex = 6)
  addBreakColorLegend(1304916, 981392, 1357619, 1198654, brks7, 
    colors = col.pal7, cex = 2.5, printFormat = "4.3")




## ----Fig-MargVar, echo= FALSE, fig.height = 10, fig.width = 8, include = FALSE, cache = TRUE----
  colun = '#d73027'
  colrs = '#1a9850'
	colun = rgb(0,0,0,.1)
	colrs = rgb(0,0,0,.1)
  sparXCAR4Vi = as.matrix(sparXCAR4$Vi)
  sparXCAR4V = solve(sparXCAR4Vi)
  sparXCAR4cor = as.matrix(1/sqrt(diag(sparXCAR4V))*
    t(1/sqrt(diag(sparXCAR4V))*sparXCAR4V))
  sparXCAR4rsVi = as.matrix(sparXCAR4rs$Vi)
  sparXCAR4rsV = solve(sparXCAR4rsVi)
  sparXCAR4rscor = as.matrix(1/sqrt(diag(sparXCAR4rsV))*
    t(1/sqrt(diag(sparXCAR4rsV))*sparXCAR4rsV))

  layout(matrix(1:4, nrow = 2, byrow = TRUE))
  par(mar = c(5,5,7,1))
  plot(apply(Nmat4,1,sum)-.1, diag(solve(sparXCAR4rs$Vi)), pch = 19, col = colrs,
     xlab = 'Number of Neighbors', ylab = 'Variance', cex.lab = 2, 
     cex = 1.5, cex.axis = 1.5)
  points(apply(Nmat4,1,sum)+.1, diag(solve(sparXCAR4$Vi)), pch = 15, 
     cex = 1.5, col = colun)
  mtext('(a)', adj = -.35, padj = -.45, cex = 4)


  cN1 = sparXCAR4cor[Nmat1 == 1]
  cN2 = sparXCAR4cor[Nmat2 - Nmat1 == 1]
  Nmat3 = (Nmat1 %*% Nmat1 %*% Nmat1 > 0 | Nmat1 > 0)*1
  diag(Nmat3) = 0
  cN3 = sparXCAR4cor[Nmat3 - Nmat2 == 1]
  cN4 = sparXCAR4cor[Nmat4 - Nmat3 == 1]

  cN1rs = sparXCAR4rscor[Nmat1 == 1]
  cN2rs = sparXCAR4rscor[Nmat2 - Nmat1 == 1]
  cN3rs = sparXCAR4rscor[Nmat3 - Nmat2 == 1]
  cN4rs = sparXCAR4rscor[Nmat4 - Nmat3 == 1]

  cNrs = rbind(
    cbind(rep(1-.1, times = length(cN1rs)),cN1rs),
    cbind(rep(2-.1, times = length(cN2rs)),cN2rs),
    cbind(rep(3-.1, times = length(cN3rs)),cN3rs),
    cbind(rep(4-.1, times = length(cN4rs)),cN4rs))
  par(mar = c(5,5,7,1))
  plot(cNrs, pch = 19, col = colrs, xlim = c(.9,4.1),
    xlab = 'Neighbor Order', ylab = 'Correlation',
    cex.lab = 2, cex.axis = 1.5, xaxt = 'n')
  axis(1, at = c(1:4), labels = c('1','2','3','4'),
    cex.axis = 1.5)
  mtext('(b)', adj = -.35, padj = -.45, cex = 4)


  points(0.9,mean(cN1rs), pch = 19, col = 'black', cex = 3) 
  points(1.9,mean(cN2rs), pch = 19, col = 'black', cex = 3) 
  points(2.9,mean(cN3rs), pch = 19, col = 'black', cex = 3) 
  points(3.9,mean(cN4rs), pch = 19, col = 'black', cex = 3) 
  cN = rbind(
    cbind(rep(1+.1, times = length(cN1)),cN1),
    cbind(rep(2+.1, times = length(cN2)),cN2),
    cbind(rep(3+.1, times = length(cN3)),cN3),
    cbind(rep(4+.1, times = length(cN4)),cN4))
  points(cN, pch = 19, col = colun)
  points(1.1,mean(cN1), pch = 19, col = 'black', cex = 3) 
  points(2.1,mean(cN2), pch = 19, col = 'black', cex = 3) 
  points(3.1,mean(cN3), pch = 19, col = 'black', cex = 3) 
  points(4.1,mean(cN4), pch = 19, col = 'black', cex = 3) 

  par(mar = c(7,5,5,1))
  distDatrs = data.frame(dist = as.vector(distMat[lower.tri(distMat)]), 
    corr = as.vector(sparXCAR4rscor[lower.tri(distMat)]),
    class = cut(as.vector(distMat[lower.tri(distMat)]), breaks = c(0,10,20,40,80,160,700)))
  boxplot(corr ~ class, data = distDatrs, ylab = 'Correlation', xaxt = 'n',
    cex.lab = 2, col = colrs, ylim = c(0,.2), pch = 19, lwd = 2, cex.axis = 1.3)
  axis(1, at = c(1:6), labels = FALSE)
  text(c(1:6), par("usr")[3]-.015, labels = levels(distDatrs$class), srt = 45, pos = 1, xpd = TRUE, cex = 1.3)
  text(3.5, par("usr")[3]-.04, labels = 'Distance', pos = 1, xpd = TRUE, cex = 2.2)
  mtext('(c)', adj = -.35, padj = -.45, cex = 4)


  distDat = data.frame(dist = as.vector(distMat[lower.tri(distMat)]), 
    corr = as.vector(sparXCAR4cor[lower.tri(distMat)]),
    class = cut(as.vector(distMat[lower.tri(distMat)]), breaks = c(0,10,20,40,80,160,700)))
  boxplot(corr ~ class, data = distDat, ylab = 'Correlation', xaxt = 'n',
    cex.lab = 2, col = colun, ylim = c(0, .2), pch = 19, lwd = 2, cex.axis = 1.3)
  axis(1, at = c(1:6), labels = FALSE)
  text(c(1:6), par("usr")[3]-.015, labels = levels(distDatrs$class), srt = 45, pos = 1, xpd = TRUE, cex = 1.3)
  text(3.5, par("usr")[3]-.04, labels = 'Distance', pos = 1, xpd = TRUE, cex = 2.2)
  mtext('(d)', adj = -.35, padj = -.45, cex = 4)

## ----Tab-Bhat, echo= FALSE, include = FALSE------------------------------
  bHatTab = as.matrix( 
    cbind( 
      sparXI$bHat, 
      sqrt(diag(sparXI$covb)),
      sparXCAR4rs$bHat,
      sqrt(diag(sparXCAR4rs$covb)),
      c(summary(beta0CAR)$statistics['Mean'],
        summary(beta1CAR)$statistics[c(5,1:3),'Mean']),
      c(summary(beta0CAR)$statistics['SD'],
        summary(beta1CAR)$statistics[c(5,1:3),'SD']),
      sparXCAR4rsDist$bHat, 
      sqrt(diag(sparXCAR4rsDist$covb))
#      sparXCAR4rsDist$bHat/sqrt(diag(sparXCAR4rsDist$covb)), 
#      2*(1-pnorm(as.vector(abs(sparXCAR4rsDist$bHat/sqrt(diag(sparXCAR4rsDist$covb)))))),
    ) 
  ) 
  string = c("$\\mu$","$\\beta_{\\textrm{stock 2}}$", "$\\beta_{\\textrm{stock 3}}$",
    "$\\beta_{\\textrm{stock 4}}$", "$\\beta_{\\textrm{stock 5}}$")
  rownames(bHatTab) = string

## ----results = 'asis', echo = FALSE--------------------------------------
  print(
    xtable(bHatTab, 
      align = c('l',rep('l', times = length(bHatTab[1,]))),
      digits = c(0,3,4,3,4,3,4,3,4),
      caption = 'Covariates used for model-fitting',
      label = 'tab:covList'
    ),
    size = 'footnotesize',
    include.rownames = TRUE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ----echo = FALSE, include = FALSE, cache = TRUE-------------------------
  system("pdfcrop _Fig-Stocks-1.pdf Fig-Stocks.pdf")
  system("convert Fig-Stocks.pdf Fig-Stocks.png")

## ----echo = FALSE, include = FALSE, cache = TRUE-------------------------
  system("pdfcrop _Fig-MapRaw-1.pdf Fig-MapRaw.pdf")
  system("convert Fig-MapRaw.pdf Fig-MapRaw.png")

## ----echo = FALSE, include = FALSE, cache = TRUE-------------------------
  system("pdfcrop _Fig-Neighbors-1.pdf Fig-Neighbors.pdf")
	system("convert Fig-Neighbors.pdf Fig-Neighbors.png")

## ----echo = FALSE, include = FALSE, cache = TRUE-------------------------
  system("pdfcrop _Fig-ModelsM2LL-1.pdf Fig-ModelsM2LL.pdf")

## ----echo = FALSE, include = FALSE---------------------------------------
  system("pdfcrop _Fig-rhoProfile-1.pdf Fig-rhoProfile.pdf")

## ----echo = FALSE, include = FALSE, cache = TRUE-------------------------
  system("pdfcrop _Fig-thetaProfiles-1.pdf Fig-thetaProfiles.pdf")

## ----echo = FALSE, include = FALSE, cache = TRUE-------------------------
  system("pdfcrop _Fig-PredSmoo-1.pdf Fig-PredSmoo.pdf")
  system("convert Fig-PredSmoo.pdf Fig-PredSmoo.png")

## ----echo = FALSE, include = FALSE, cache = TRUE-------------------------
  system("pdfcrop _Fig-MargVar-1.pdf Fig-MargVar.pdf")
	system("convert Fig-MargVar.pdf Fig-MargVar.png")

