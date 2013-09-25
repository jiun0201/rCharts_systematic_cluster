# work with http://systematicinvestor.wordpress.com/category/cluster/
# visualize with d3 using rCharts

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
tickers = c("PISIX","PSKIX","PSDIX","PSTKX","PCRIX",
            "PFIIX","PHMIX","PFCIX","PCDIX","PTSHX",
            "PFMIX","PLMIX","PSPTX","PCIMX","PSTIX",
            "PNYIX","PLDTX","PLDIX","PTLDX","PAAIX",
            "PXTIX","PHIYX","PSCSX","PAUIX","PTRIX",
            "PGBIX","PFORX","PELBX","PDMIX","PMDRX",
            "PEBIX","PDIIX","PRRSX","PMBIX","PTSAX",
            "PTTRX","PIGLX","PRRIX","PFUIX","PIMIX",
            "PIGIX","PRAIX","PLRIX","PGOVX","PEDIX","VFINX")

data <- new.env()
#reduce calls to Yahoo! Finance while experimenting
load("pimco_data.Rdata",envir=data)
#getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
#for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
#save(file="pimco_data.Rdata",list=ls(envir=data),envir=data)
bt.prep(data, align='remove.na')

# get the one period returns
ret <- data$prices / mlag(data$prices) - 1
# make first returns 0 instead of NA
ret[1,] <- 0

# create input assumptions
ia = create.historical.ia(ret, 252) 

# create clusters
grp = cluster.group.kmeans.90(ia)
ngroups = max(grp)


# since cluster.group.kmeans.90 only returns the group
# get the components out manually with these bits
require(cluster)
correlation = cor(ret, use='complete.obs', method = "spearman")    
dissimilarity = 1 - (correlation)
distance = as.dist(dissimilarity)

# get principal coordinates
xy = cmdscale(distance)
fit = kmeans(xy, ngroups, iter.max=100, nstart=100)
# do a noninteractive plot of the coordinates
clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
         main = paste('Major Market Clusters over 6 Clusters'), sub='')
abline(v=0)
abline(h=0)

# now use rCharts to get some interactive d3 plots
pc.df <- data.frame(rownames(xy),grp,-xy)
colnames(pc.df) <- c("symbol","group","PC1","PC2")

# first a dimplejs version
dP <- dPlot(
  PC2 ~ PC1,
  groups = c("symbol","group"),
  data = pc.df,
  type = "bubble"
)
dP$xAxis( type = "addMeasureAxis" )
dP

# then a nvd3 version
nP <- nPlot(
  PC2 ~ PC1,
  group = "group",
  data = pc.df,
  type = "scatterChart")
nP$xAxis(tickFormat = "#!d3.format('.2%')!#")
nP$yAxis(tickFormat = "#!d3.format('.2%')!#")
nP$chart(
  showDistX = TRUE,
  showDistY = TRUE,
  tooltipContent = "#!function(key, y, e, graph) {
    return '<h3>Group: ' + key + '<br>' + graph.point.symbol + '</h3>';
  }!#")
nP
