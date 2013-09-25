#work with http://systematicinvestor.wordpress.com/2013/02/12/cluster-portfolio-allocation/
#visualize with d3

###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
#*****************************************************************
# Load historical data for Pimco Funds
#****************************************************************** 
library(quantmod)
tickers = c("PISIX","PSKIX","PSDIX","PSTKX","PCRIX","PFIIX","PHMIX","PFCIX","PCDIX","PTSHX","PFMIX","PLMIX","PSPTX","PCIMX","PSTIX","PNYIX","PLDTX","PLDIX","PTLDX","PAAIX","PXTIX","PHIYX","PSCSX","PAUIX","PTRIX","PGBIX","PFORX","PELBX","PDMIX","PMDRX","PEBIX","PDIIX","PRRSX","PMBIX","PTSAX","PTTRX","PIGLX","PRRIX","PFUIX","PIMIX","PIGIX","PRAIX","PLRIX","PGOVX","PEDIX","VFINX")

data <- new.env()
#reduce calls to Yahoo! Finance while experimenting
load("pimco_data.Rdata",envir=data)
#getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
#for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
#save(file="pimco_data.Rdata",list=ls(envir=data),envir=data)
bt.prep(data, align='remove.na')


ret <- data$prices / mlag(data$prices) - 1
ret[1,] <- 0

# create input assumptions
ia = create.historical.ia(ret, 252) 

# create clusters
grp = cluster.group.kmeans.90(ia)
ngroups = max(grp)


#get a melted data frame of the input assumptions(ia) and groups(group)
ia.df <- data.frame(ia$symbols,ia$geometric.return,ia$arithmetic.return,ia$risk,grp)
colnames(ia.df) <- c("symbol","geometric","arithmetic","risk","group")


#play with efficient frontier
# create long-only, fully invested efficient frontier
n = ia$n        

# 0 <= x.i <= 1
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(diag(n), type='>=', b=0, constraints)
constraints = add.constraints(diag(n), type='<=', b=1, constraints)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        

# create efficient frontier
ef = portopt(ia, constraints, 50, 'Efficient Frontier') 
plot.ef(ia, list(ef), transition.map=F)

# find maximum sharpe portfolio
max(portfolio.return(ef$weight,ia) /  portfolio.risk(ef$weight,ia))

# plot minimum variance portfolio
weight = min.var.portfolio(ia,constraints)  
points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='red')
portfolio.return(weight,ia) /  portfolio.risk(weight,ia)

# plot maximum Sharpe or tangency portfolio
weight = max.sharpe.portfolio()(ia,constraints) 
points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='orange')
portfolio.return(weight,ia) /  portfolio.risk(weight,ia)

plota.legend('Minimum Variance,Maximum Sharpe','red,orange', x='topright')  

#add cluster group colors and colored labels
points(x=ia$risk*100,y=ia$arithmetic.return*100,pch=19,col=grp)
text(ia$risk*100, ia$arithmetic.return*100, ia$symbols,  col = grp, adj = c(1,1), cex = 0.8)

#to get the efficient frontier line
x = 100 * portfolio.risk(ef$weight, ia)
y = 100 * portfolio.return(ef$weight, ia)



#do efficient frontier for each group
require(latticeExtra)

xyplot(
  geometric ~ risk | factor(group),
  groups = group,
  data = ia.df,
  pch = 19,
  type = c("p"), #,"r"),
  layout = c(ngroups,1),
  panel = function(x,y,type,...){
    #print(panel.number())
    ia.temp <- create.historical.ia(
      ret[,names(grp[which(grp==panel.number())])],
      252
    )
    ef <- portopt(ia.temp, constraints, 50, 'Efficient Frontier') 
    #to get the efficient frontier line
    ef.x = 100 * portfolio.risk(ef$weight, ia.temp)
    ef.y = 100 * portfolio.return(ef$weight, ia.temp)
    panel.xyplot(x,y,type=type,...)
    panel.xyplot(ef.x,ef.y,type="l",...)
  }
)

require(fAssets)
assetsCorEigenPlot(as.timeSeries(ret))

require(cluster)
correlation = cor(ret)    
dissimilarity = 1 - (correlation)
distance = as.dist(dissimilarity)

# get first 2 pricipal componenets
xy = cmdscale(distance)
fit = kmeans(xy, ngroups, iter.max=100, nstart=100)
clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
         main = paste('Major Market Clusters over 6 Clusters'), sub='')


pPlot <- xyplot(PC2~PC1|factor(grp),groups = grp,data=data.frame(pca$x),pch=19)
pPlot + layer(panel.abline(h=0,col="gray70")) + layer(panel.abline(v=0,col="gray70"))

pPlot2 <- xyplot(PC2~PC1,col= grp,data=data.frame(pca$x),pch=19)
pPlot2 + 
  layer(panel.abline(h=0,col="gray70")) +
  layer(panel.abline(v=0,col="gray70")) +
  layer(panel.text(
    y=data.frame(pca$x)$PC2,
    x=data.frame(pca$x)$PC1,
    labels=rownames(pca$x),
    col = grp,
    adj = c(1,1),
    cex = 0.8
  ))


pca.df <- data.frame(rownames(pca$x),grp,pca$x)
colnames(pca.df) <- c("symbol","group",colnames(pca.df)[-(1:2)])
dP <- dPlot(
  PC2 ~ PC1,
  groups = c("symbol","group"),
  data = pca.df,
  type = "bubble"
)
dP$xAxis( type = "addMeasureAxis" )
dP

nP <- nPlot(
  PC2 ~ PC1,
  group = "group",
  data = pca.df,
  type = "scatterChart")
nP$xAxis(tickFormat = "#!d3.format('.2%')!#")
nP$yAxis(tickFormat = "#!d3.format('.2%')!#")
nP$chart(
  showDistX = TRUE,
  showDistY = TRUE,
  tooltipContent = "#!function(d) {
    return '<h3>' + d + '</h3>';
  }!#")
nP

#do a pretty correlation on heat map on all Pimco Funds (institutional share class) that have existed longer 5 years
#symbol list obtained from
#http://investments.pimco.com/Products/pages/PlOEF.aspx?Level1=ulProducts&Center=ulProducts&Level2=liulProductsMutualFunds
#pasted into Excel and sorted by 5 yr return
#then copied and pasted transpose
#saved to csv
#and pasted in this ticker list eliminating the money fund and adding Vanguard S&P 500 for reference
require(quantmod)
#comment this out to reduce hits on Yahoo Finance
#instead save as .Rdata and load that in while experimenting
load("pimco_data.Rdata")
tckrs <- c("PISIX","PSKIX","PSDIX","PSTKX","PCRIX","PFIIX","PHMIX","PFCIX","PCDIX","PTSHX","PFMIX","PLMIX","PSPTX","PCIMX","PSTIX","PNYIX","PLDTX","PLDIX","PTLDX","PAAIX","PXTIX","PHIYX","PSCSX","PAUIX","PTRIX","PGBIX","PFORX","PELBX","PDMIX","PMDRX","PEBIX","PDIIX","PRRSX","PMBIX","PTSAX","PTTRX","PIGLX","PRRIX","PFUIX","PIMIX","PIGIX","PRAIX","PLRIX","PGOVX","PEDIX","VFINX")

#for (i in 1:length(tckrs)) {
#  ifelse (i == 1,
#          pimco <- get(getSymbols(tckrs[i],from="2000-01-01",adjust=TRUE))[,4],
#          pimco <- merge(pimco,get(getSymbols(tckrs[i],get="all",from="2000-01-01",adjust=TRUE))[,4]))
#}
#remove .close from each of the symbols
colnames(pimco) <- tckrs
pimco.clean <- na.omit(pimco)
pimco.roc <- ROC(pimco.clean,n=1,type="discrete")
pimco.roc[1,] <- 0

#get in data.frame
#get the function convert4corrwscatter from Broman example
source("createJSON.R")
#group by year for colors on scatterplot
yeargroup <- as.numeric(format(index(pimco.roc),"%Y"))
names(yeargroup) <- index(pimco.roc)
#sort by correlation to VFINX instead of hclust method from Karl
#get correlation table for sorting
ca <- cor(pimco.roc)
pimco.roc <- pimco.roc[,order(ca[,ncol(ca)])]

pimco.toplot <- convert4corrwscatter(as.matrix(pimco.roc),yeargroup,reorder=FALSE)
cat(pimco.toplot,file="pimco_data.json")






#do correlation table to use as reference
ca <- cor(pimco.roc)

#get colors to use for heat map
brew <- brewer.pal(name="RdBu",n=5)
#get color ramp
cc.brew <- colorRampPalette(brew)
#apply color ramp
cc <- cc.brew(nrow(ca))
#do heatmap and sort by degree of correlation to VFINX (Vanguard S&P 500)
heatmap(ca[order(ca[,ncol(ca)]),order(ca[,ncol(ca)])],symm=TRUE,Rowv=NA,Colv=NA,col=cc,RowSideColors=cc,main="")
title(main="Correlation Table (Ordered by Correlation with Vanguard S&P 500-VFINX)",font.main=1,outer=TRUE,line=-1,cex.main=1.3)

heatmap(ca[order(ca[,ncol(ca)]),order(ca[,ncol(ca)])],symm=TRUE,col=cc,RowSideColors=cc,main="")
title(main="Correlation Table (Ordered by Dendrogram)",font.main=1,outer=TRUE,line=-1,cex.main=1.3)


#do colors based on correlation but with gray so visible when labelling
cc.palette <- colorRampPalette(c(cc[1],"gray60",cc[length(cc)]))
cc.levpalette <- cc.palette(nrow(ca))
cc.levels <- level.colors(ca[order(ca[,ncol(ca)-1]),ncol(ca)-1], at = do.breaks(c(-1,1),nrow(ca)),
                          col.regions = cc.levpalette)
dotchart(ca[order(ca[,ncol(ca)]),ncol(ca)],col=cc.levels,pch=19,cex=0.75)
title(main="Correlation to Vanguard S&P 500 (VFINX)",font.main=1,outer=TRUE,line=-1,cex.main=1.3)