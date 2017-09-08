plot(c(0,1),c(0,1), type = 'n', axes = FALSE, xlab = '',
	ylab = '')
text(x = c(.1,.5,.9,.1,.5,.9,.1,.5,.9), y = c(.9,.9,.9,.5,.5,.5,.1,.1,.1),
	labels = as.character(1:9), cex = 5)
points(x = c(.1,.5,.9,.1,.5,.9,.1,.5,.9), y = c(.9,.9,.9,.5,.5,.5,.1,.1,.1),
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
