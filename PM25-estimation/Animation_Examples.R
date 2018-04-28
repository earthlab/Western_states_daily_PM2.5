# https://www.r-bloggers.com/animate-gif-images-in-r-imagemagick/
dir.create("examples")
setwd("examples")

# example 1: simple animated countdown from 10 to "GO!".
png(file="example%02d.png", width=200, height=200)
for (i in c(10:1, "G0!")){
  plot.new()
  text(.5, .5, i, cex = 6)
}
dev.off()

# convert the .png files to one .gif file using ImageMagick. 
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
system("convert -delay 80 *.png example_1.gif")

# to not leave the directory with the single jpeg files
# I remove them.
file.remove(list.files(pattern=".png"))



#_____________________
# https://cran.r-project.org/web/packages/anim.plots/anim.plots.pdf
tmp <- volcano
tmp[] <- 200 - ((row(tmp) - 43)^2 + (col(tmp) - 30)^2)/20
cplot <- array(NA, dim=c(87,61,20))
cplot[,,1] <- tmp
cplot[,,20] <- volcano
cplot <- apply(cplot, 1:2, function(x) seq(x[1], x[20], length.out=20))
cplot <- aperm(cplot, c(2,3,1))
anim.contour(z=cplot, times=1:20, speed=3, levels=80 + 1:12*10, lty=c(1,2,2))
anim.filled.contour(z=cplot, times=1:20, speed=3, levels=80 + 1:12*10,
                    color.palette=terrain.colors)
cplot2 <- apply(cplot, 1:2, function(x) seq(0, x[20], length.out=20))
cplot2 <- aperm(cplot2, c(2,3,1))
anim.persp(z=cplot2, times=1:20, xlab="", ylab="", zlab="Height", phi=45,
           theta=30, speed=5, border=NA, r=3, col="yellowgreen", shade=.5, box=FALSE)


#______________________________________
# https://github.com/hughjonesd/anim.plots
x <- rep(rnorm(400), 10)
y <- rep(rnorm(400), 10)
xlims <- 4 * 2^-(1:10/10)
ylims <- xlims <- rbind(xlims, -xlims)
anim.plot(x, y, times = 10, speed = 2, xlim = xlims, ylim = ylims, col = rgb(0,1,0,.3), pch = 19) 

# https://cran.r-project.org/web/packages/anim.plots/anim.plots.pdf
tmp <- anim.plot(1:10, 1:10, pch=1:10, show=FALSE)
anim.save(tmp, "mygif.gif")
anim.save(replay(tmp, after=legend("topleft", legend="My legend")),
          "mygif2.gif")

#C:/Program File/ImageMagick-7.0.7-Q16/convert.exe -loop 0 -delay 100 Rplot1.png Rplot2.png Rplot3.png Rplot4.png Rplot5.png Rplot6.png Rplot7.png Rplot8.png Rplot9.png Rplot10.png "mygif.gif"
