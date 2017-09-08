plot(c(0,1),c(0,1), type = 'n', xaxt = 'n', yaxt = 'n', 
	xlab = '', ylab = '', axes = FALSE)
text(x=c(.1,.5,.9,.1,.5,.9,.1,.5,.9),
	y=c(.9,.9,.9,.5,.5,.5,.1,.1,.1),
	labels = as.character(1:9),
	cex = 7)

plot(c(0,1),c(0,1), type = 'n', xaxt = 'n', yaxt = 'n', 
	xlab = '', ylab = '', axes = FALSE)
lines(c(0,0),c(0,1),lwd = 2)
lines(c(0,.1),c(0,0),lwd = 2)
lines(c(0,.1),c(1,1),lwd = 2)
text(x = seq(.1,.9, by = .1), y=rep(.9, times = 9), 
	labels = as.character(c(0,1,0,1,0,0,0,0,0)), cex = 3)

