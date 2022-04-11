jplot <- function( x1, x2, draw.line=T, ... )
{

	plot( x1, x2,
	      pch=19, 
	      col=gray(0.6, alpha = 0.4), 
	      cex=0.8,  
	      bty = "n",
	      cex.lab=1.5,
        ... )

	if( draw.line==T ){ 
		ok <- is.finite(x1) & is.finite(x2)
		lines( lowess(x2[ok]~x1[ok]), col="steelblue", lwd=3 ) }

}




panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use="pairwise.complete.obs")
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    
    test <- cor.test(x,y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
    
    text(0.5, 0.5, txt, cex = 2 )
    text(.7, .8, Signif, cex=3, col=2)
}


panel.smooth <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
  cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = 19, col = gray(0.5,0.5), 
         bg = bg, cex = 0.7)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
      col = col.smooth, lwd=2, ...)
}


# d2 <- select( dat, x, y, z )
# pairs( d2, lower.panel=panel.smooth, upper.panel=panel.cor )



###
###  CREATE QUANTILE GROUPS 
###

create_quantiles <- function( var, n.groups=5 )
{

  x <- 100/n.groups  # label increments 
  
  q.breaks <- quantile( var, 
                        probs=seq( 0, 1, by=1/n.groups ), 
                        na.rm=TRUE )
  if( max(var,na.rm=T) >= 1000000 )
  {                      
    q.dollars <- paste0( "$", round( q.breaks / 1000000, 1 ), "m" )                     
    q.labels <- paste0( "Q ", seq( 0, 100-x, x ), 
                          "-",  seq( x, 100, x ),
                         "   ( ",
                         q.dollars[-length(q.dollars)], 
                         " to ", q.dollars[-1],
                         " )" )
  }

  if( max(var,na.rm=T) < 1000000 )
  {                      
    q.levels <- round( q.breaks, 0 )                     
    q.labels <- paste0( "Q ", seq( 0, 100-x, x ), 
                          "-",  seq( x, 100, x ),
                         "   ( ",
                         q.levels[-length(q.levels)], 
                         " to ", q.levels[-1],
                         " )" )
  }                     
  groups <- cut(  var, 
                  breaks=q.breaks, 
                  labels=q.labels,
                  include.lowest=TRUE )
                  
  return( groups )
}




# BOXPLOT 

# core2 %>% 
#   filter( ! is.na(size.q10) ) %>% 
#   ggplot(  aes(y=dar) )  + 
#     geom_boxplot( col="gray30", alpha=0.7)  + 
#     ylim( -0.5, 1 ) + 
#     xlab( "Nonprofit Size (logged expenses)" ) + 
#     ylab( "Debt to Asset Ratio" ) +
#     facet_wrap( ~ size.q10, nrow=4 ) +
#     theme_minimal()
