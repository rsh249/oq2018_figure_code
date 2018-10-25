library(Bchron)
library(foreach)
library(doSNOW)
library(raster)
library(palinsol)
library(compiler)
library(plotrix)

root = "./";
out = paste(root, "data/", sep = "")
refdata = paste(root, "refdata/", sep = "")
files = list.files(path = out)
site_data <- read.table(paste(refdata, "report.tab", sep = ""), header=T, sep = "\t")


do_timeline3 <- function(x, type)
{
	#from MAT.merge get average diff between CRACLE and current for each year using +/- 1.96 stdev on ages
	x <- data.frame(x)
	x=na.omit(x)
	changesince <- vector();
	changesince[[1]] = 0;
	avg.dif = vector();avg.per = vector();avg.dif2 = vector();sites.used= vector();  stdev.dif = vector(); num_tax = vector(); altitude = vector();df=vector(); base.avg = vector(); base.sd = vector();
	y = 1;
	for(i in -500:0){
		diff.hold = vector();# diff2.hold = vector();
		per.hold = vector();
		base.hold = vector();
		num_tax.hold = vector();
		sites = 0;
		altitude.hold = vector();
		sites_hold = x[1,]
		lower = (i*100)-1000;
		upper = (i*100);
		#cat(upper, ", ", lower, "\n");
		for(n in 1:length(x[,8])){
			
			#  if(upper >= x[n, 9] & upper <= x[n,8] | lower <= x[n,8] & lower >= x[n,9] | (i==0 & x[n,8]>=0)) {
			if(upper >= x[n, 9] & lower <= x[n,8]  | (i==0 & x[n,8]>=0)) {
				#    diff = x[n,mod] - (x[n,13]+x[n,12])
				diff = (((x[n,'V4']+x[n,'V3'])/2) - (x[n,'V2'])); 
				percent.dif = ((x[n,'V2'] - (x[n,'V4']+x[n,'V3'])/2)) / (x[n,'V2']);
				#  diff2 = (x[n,13] - x[n,11]);
				num_tax.hold = c(num_tax.hold, x[n, 'total'])
				altitude.hold = c(altitude.hold, x[n, 'elev_mean'])
				#print(percent.dif); print(diff); print(x[n,11])
				diff.hold <- c(diff.hold, diff);
				per.hold <- c(per.hold, percent.dif);
				#   diff2.hold <- c(diff2.hold, diff2)
				sites = sites+1;
				sites_hold = rbind(sites_hold, x[n,])
				
			}
			
		}
		#print(diff.hold)
		#if(i==0){return(sites_hold)}
		#define weights by 1/nMillenia for each site?
		#w = 1/(sites_hold[-1,8] - sites_hold[-1,9])
		#return(w)
		num_tax[[y]] <- mean(((num_tax.hold)))
		altitude[[y]] <- mean(((altitude.hold)))
		#avg.wdif[[y]] = weighted.mean(na.omit((diff.hold)), w);
		avg.dif[[y]] = mean(((diff.hold)))
		avg.per[[y]] = mean(((per.hold)));
		#  avg.dif2[[y]] = mean(na.omit((diff2.hold)))
		stdev.dif[[y]] = sd((abs((diff.hold))))
		sites.used[[y]] = sites;
		#  base.avg[[y]] = mean(na.omit((base.hold)));
		#  base.sd[[y]] = sd(na.omit((base.hold)))
		#  if(sites==1 | sites==0){ df[[y]]=0} else {
		#   df[[y]] = qt(0.975, df = sites.used[[y]]-1);
		# }
		#if(y>5){
		#  changesince[[y]] <- avg.dif[[y]] - changesince[[y-5]]
		# }
		y = y+1;
		
		
	}
	return(cbind((avg.dif), (stdev.dif), (sites.used), (num_tax), (altitude), (avg.per)));
	
}
do_timeline3 = compiler::cmpfun(do_timeline3)


check.integer <- function(x) {
	x == round(x)
}

site_data <- subset(site_data, check.integer(site_data$lat) == FALSE)
site_data <- subset(site_data, check.integer(site_data$lon) == FALSE)

taxcount <- read.csv(paste(refdata, 'taxcount.tab', sep=''), header=T, sep = '\t')
colnames(taxcount) <- c("analysis_id", "genus", "spec", "total");
site_info <- merge(site_data, taxcount, by.x = 'analysis_id', by.y = 'analysis_id')
site_info <- subset(site_info, as.numeric(as.character(site_info$total)) >= 10);
site_data <- site_info[,-10:-12]

#site_data <- subset(site_data, site_data$plot_age <= 45000)


library(Bchron)
data(intcal13)


calib <- function(age, agesd, opt = 'max', curve = 'intcal13'){  
	
	#opt can be 'max' or 'ci'
	require(Bchron)
	if(any(is.na(age))){return(0);}
	
	if(age > 46400 | age < 71){
		
		
		if(opt == 'max'){
			max = age;
			return(max)
		}
		if(opt == 'ci'){
			ci = c(age-1.96*agesd, age+1.96*agesd);
			ci <- as.data.frame(cbind(ci[[1]], ci[[2]]))
			colnames(ci) <- c("cal_min", "cal_max")
			return(ci)
		}
		
	}
	
	calibtest <- BchronCalibrate(age, ageSds = agesd, calCurves = curve)
	#plot(calibtest$date1$ageGrid, calibtest$date1$densities)
	calibdens <- cbind(calibtest$Date1$ageGrid, calibtest$Date1$densities)
	if(opt == 'ci'){
		cumul <- vector();
		ci = vector();
		for(i in 1:length(calibdens[,1])){
			if(i==1){
				cumul[[i]] <- calibdens[i,2];
				if(cumul[[i]] >= 0.025){
					ci[[1]] <- calibdens[i,1];
				};
				if(cumul[[i]] >= 0.975){
					ci[[1]] <- calibdens[i,1];
					ci[[2]] <- calibdens[i,1];
					
				};
			} else {
				cumul[[i]] <- cumul[[i-1]] + calibdens[i,2]
				if(cumul[[i-1]] < 0.025 && cumul[[i]] >= 0.025){
					ci[[1]] <- calibdens[i,1];
				};
				if(cumul[[i-1]] < 0.975 && cumul[[i]] >= 0.975){
					ci[[2]] <- calibdens[i,1];
					
				};
			}
			
			
		}
		if(length(ci)==1){ci[[2]] = 50000}
		
		ci <- as.data.frame(cbind(ci[[1]], ci[[2]]))
		colnames(ci) <- c("cal_min", "cal_max")
		return(ci)
	} 
	if(opt == 'max'){
		wh = which(calibdens[,2] == max(calibdens[,2]))[1]
		maximum = calibdens[wh,1];
		return(maximum)
	}
}

for (x in 1:length(site_data[,1])){
	print(x);
	# if(site_data[x, 'plot_age'] <= 45000){
	hold = calib(site_data$plot_age[x], site_data$plot_agestdev[x], opt = 'ci', curve='intcal13')
	if(ncol(hold)>1){
		site_data[x, 'plot_age'] = hold[1,1]
		site_data[x, 'plot_agestdev'] = hold[1,2]
	} else {
		site_data[x,'plot_age'] = hold;
		site_data[x,'plot_agestdev'] = hold;
	}
}



###
files = list.files(out)
var = list();
for (i in 1:length(files)){
	filenow <- paste(out, files[i], sep ="/")
	
	fileroot = gsub(".tab", "", filenow)
	split <- strsplit(fileroot, "/", fixed = T)
	typesplit <- strsplit(split[[1]][length(split[[1]])], "[.]")
	# var[[i]] = typesplit[[1]][1];
	
	var[[i]] = paste(typesplit[[1]][1], typesplit[[1]][2], sep = ".");
	
};  
var <- unlist(var);
varlist <- unique(var);
varlist = varlist[-11]
varlist = varlist[-11]

doit = list();
variable = list();
types = list();
kdemin = list();
kdemax = list();
gaussmin = list();
gaussmax = list();

nclus=2;

library(foreach)
cl <- parallel::makeCluster(nclus, type = "SOCK")
doSNOW::registerDoSNOW(cl);

doit <-
	# foreach::foreach(i = 1:length(varlist)) %dopar% {
	foreach::foreach(i = c(284, 324, 294, 274, 364, 404, 94)) %dopar% {
		#This will run the summary stats ONLY for the variables needed for the figures here. If you want more then you need to pick different numbers corresponding to where in the file list they occur.
		#Look at varlist for more variables to do timelines for
		filegauss = files[grep(varlist[[i]], files)]
		
		datagauss <- read.table(paste(out, filegauss, sep="/"))
		mergegauss <- merge(site_data, datagauss, by.x = 'analysis_id', by.y = 'V1')
		
		mergegauss$plot_age = -1*mergegauss$plot_age;
		mergegauss$plot_agestdev = -1*mergegauss$plot_agestdev;
		
		gaussmin[[i]] = min(mergegauss$V3);
		gaussmax[[i]] = max(mergegauss$V4);
		
		do <- do_timeline3(mergegauss)
		colnames(do) = c("avg.dif", "stdev.dif", "sites.used", "num_tax", "altitude", "avg.per")
		return(do)
	}
parallel::stopCluster(cl)

## build collection matrix to match doit object list
filegauss = files[grep(varlist[[1]], files)]
datagauss <- read.table(paste(out, filegauss, sep="/"))
mergegauss <- merge(site_data, datagauss, by.x = 'analysis_id', by.y = 'V1')
mat = matrix(nrow=nrow(mergegauss), ncol = 13)
mat = as.data.frame(mat)
###
cc=2
for(i in c(284, 324, 294, 274, 364, 404, 94)){
	print(varlist[[i]]);
	filegauss = files[grep(varlist[[i]], files)]
	
	datagauss <- read.table(paste(out, filegauss, sep="/"))
	mergegauss <- merge(site_data, datagauss, by.x = 'analysis_id', by.y = 'V1')
	
	mergegauss$plot_age = -1*mergegauss$plot_age;
	mergegauss$plot_agestdev = -1*mergegauss$plot_agestdev;
	mat[,1] = mergegauss$plot_name;
	mat[,cc] = mergegauss[,"V2"]
	cc=cc+1
	print(cc)
	mat[,cc] = apply(mergegauss[,c('V3','V4')],1 , median)
	cc=cc+1
	print(cc)
	
}
colnames(mat) <- c('plot_name', 'MAT_wc', 'MAT_pred', 'MinT_wc', 'MinT_pred',
									 'MaxT_wc', 'MaxT_pred', 'MAP_wc', 'MAP_pred', 'wbalann_wc', 'wbalann_pred',
									 'winterlen_wc', 'winterlen_pred')
write.table(mat, file='data_supplement2.csv', sep = "\t")


library(raster)
ext = extent(c(-125, -100, 25, 45))

##Figures:
library(raster)
library(rasterVis)
library(rgeos)
#setwd(root)
#1 - > Spatial distribution of localities
alt1 = getData('alt', cou = 'MX')
alt2 = getData('alt', cou ='US')
alt3 = getData('alt', cou = 'CA')
x = c(alt1, alt2, alt3)
alt = do.call(merge,x)
alt <- crop(alt, ext)
r = stack('./refdata/sampleclim.gri')
r=crop(r,ext)
usa1=getData('GADM', cou="US", level=1)
mx1 = getData('GADM', cou="MX", level=1)
usa0 = getData('GADM', cou='US', level=0)
mx0 = getData('GADM', cou='MX', level=0)

usa1 = crop(usa1, ext)
mx1 = crop(mx1, ext)
usa0 = crop(usa0, ext)
mx0 = crop(mx0, ext)

#BELOW WORKS BUT RUNS LONG
tiff('figs/Figure_S3.tif',  
		 height=4, width =4.20, 
		 res = 750, 
		 units='in')
levelplot(alt, 
					colorkey=F, 
					pretty =T, 
					margin=F, 
					col.regions = gray.colors(99)) +
	layer(grid.points(site_data$lon, 
										site_data$lat,
										pch = 4,
										size = unit(0.4, "char"),
										default.units = "native")) +
	layer(sp.polygons(usa1)) +
	layer(sp.polygons(mx1))  +
	layer(sp.polygons(mx0, lwd=2)) +
	layer(sp.polygons(usa0, lwd=2))
dev.off();
#########################

#2 - > Taxon and midden # through time

##Resample taxon number curve
diff=vector()
in.start = vector()
in.end = vector()
start = vector()
end=vector()
for(i in 1:100000){
	start[i] = round(runif(1, min = 0, max = 500))+1
	
	in.start[i] = doit[[1]][start[i],4]
	if(start[i] < 470){
		end[i] = start[i]+30
		in.end[i] = doit[[1]][(end[i]),4]
	} else {
		start[i] = start[i]-30
		end[i] = start[i]+30
		in.end[i] = doit[[1]][(end[i]),4]
	}
	diff[i] = in.end[i]-in.start[i]
	# print(diff)
	start[i] = start[i] -500
	end[i] = end[i] - 500
	
}
lower=sort(diff)[2500]

upper = sort(diff)[97500]
#plot(-0.1*rev(start), rev(diff), pch =20)
#points(-0.1*rev(end), rev(diff), pch = 20)
#abline(h = upper, col = 'red')
#abline(h = lower, col = 'red')
chsam = cbind(start, end, diff)
chsam=chsam[order(chsam[,1]),]
#plot(chsam[,1], chsam[,3])
#points(chsam[,2], chsam[,3])



#Line (mean taxon number) over histogram of sample distribution?
tiff('figs/Figure_S2.tif', width = 7.25, height = 6, units='in', res = 450)
m = rbind(c(1,1), c(2,2))
layout(m)
par(mai = c(0.7, 0.7, 0.7, 0.7))

sig.inc=unique(subset(chsam, chsam[,3]>upper));
inc.inter = matrix(ncol=2, nrow=nrow(sig.inc));
n=1
for(i in 1:nrow(sig.inc)){
	inc.inter[i,1] = sig.inc[i,1];
	if( i < nrow(sig.inc) && sig.inc[i,2] >= sig.inc[i+1,1]){
		next;
	} else {
		inc.inter[n,2] = sig.inc[i,2];
		n=i+1;
	}
}
inc.inter = na.omit(inc.inter)

sig.dec=unique(subset(chsam, chsam[,3]<lower))
dec.inter = matrix(ncol=2, nrow=nrow(sig.dec));
n=1
for(i in 1:nrow(sig.dec)){
	dec.inter[i,1] = sig.dec[i,1];
	
	if( i < nrow(sig.dec) && sig.dec[i,2] >= sig.dec[i+1,1]){
		next;
	} else {
		dec.inter[n,2] = sig.dec[i,2];
		n=i+1;
	}
}
dec.inter=na.omit(dec.inter)

plot(0.1*(-500:0), doit[[1]][1:501,3], type = "h", ylab ='', xlab = "kya",  cex.lab =0.9, axes = F, lwd =2, xlim=c(-50, 0), ylim = c(0, max(doit[[1]][,3])))
#hist( doit[[i]][1:50,3], type = "l", ylab ='', xlab = "kya", axes = F, lwd =2, xlim=c(-50, 0), ylim = c(0, max(doit[[i]][,3])))
axis(side = 2, at = pretty(range(doit[[1]][,3], na.rm = T), n = 10), cex.axis = 1)
axis(side = 1, at = pretty(0.1*(-500:0), n = 10), labels=seq(50,0,-5), cex = 1)
mtext("Frequency of midden samples", side=2, line=3, cex=0.7, padj=0.5)

plot((-500:0)/10, doit[[1]][,4], type = "l", ylab = '',xlab = "kya", cex.lab =0.9,axes = F, col='black',  lwd=2, xlim = c(-50, 2), ylim = c(8, 26))

rect(0.1*inc.inter[,1], 11, 0.1*inc.inter[,2], 21, density = -1, col = rgb(0,1,1.0,alpha=0.3), lwd=0)
rect(0.1*dec.inter[,1], 11, 0.1*dec.inter[,2], 21, density = -1, col = rgb(0,0,1.0,alpha=0.3), lwd=0)
points((-500:0)/10, doit[[1]][,4], type = "l", lwd=2)

axis(side = 4, pos = 1, at = pretty(c(10, max(doit[[1]][,4], na.rm = T)), n = 10), cex.axis = 1, col = 'black',  col.axis = 'black')
mtext("Avg. number of taxa per sample", side=4, line=3, cex=0.7, padj = -5, col = 'black')
axis(side = 1, at = pretty(0.1*(-500:0), n = 10), labels=seq(50,0,-5), cex = 1)
legend(-50, 26.5, 
			 c('Significant Div. Increase', 'Significant Div. Decrease'), 
			 fill = c(rgb(0,1,1.0,alpha=0.5), 
			 				 rgb(0,0,1.0,alpha=0.5)))
dev.off()

library(palinsol)
#Daily-mean summer solstice insolation time series @ 35N (UNITS??)

insolation <- function(times, astrosol=ber78,...)
	sapply(times, function(tt) Insol(orbit=astrosol(tt), lat = 65*pi/180))

tts <- seq(from = -50000, to = 0, by = 200)
isl <- insolation(tts, ber78)
plot(tts, isl, typ='l')
library(plotrix)
#3 - > MAT vs N hemisphere proxies
tiff('figs/Figure_1.tif', 
		 height = 8.25, width = 4.25, 
		 res = 750, units='in')
par(mar = c(5, 4, 4, 4) + 0.1)  # Leave space for z axis

v=1; #MAT
MAT = doit[[v]][,1]/10;
MAT = MAT-MAT[[501]]
MAT.se = (doit[[v]][,2]/10)/sqrt(doit[[v]][,3]);
plot(0.1*(-500:0), c(MAT), type="l", lwd = 2.5, lty = 1, axes = F, ylim=c(-45, 3), xlab ='', ylab = '', panel.first = grid(NULL, 0)) # first plot
points(0.1*(-500:0), c(MAT)+1.96*MAT.se, type ='l', lty=3)
points(0.1*(-500:0), c(MAT)-1.96*MAT.se, type ='l', lty=3)
textbox(c(-50, -11.8),4.7,c("Pleistocene"), 
				box = T,cex =0.7, lwd=1.5, adj=c(0,0.2),
				fill ='grey', border = 'white', justify='c')
textbox(c(-11.7,0),4.7,c("Holocene"), 
				box = T, cex = 0.7, lwd=1.5, adj=c(0,0.2), 
				fill ='grey', border = 'white', justify='c')
textbox(c(-26,-19),3.5,c("LGM"), 
				box = T,  cex = 0.7, lwd=1.5, adj=c(0,0.2), 
				fill ='grey', border = 'white', justify='c')

abline(0,0, lty = 1)
axis(side = 1, at = pretty(0.1*(-500:0), n = 10), labels=seq(50,0,-5), 
		 cex.axis = 0.7)
axis(side = 2, at = pretty(range(c(MAT+2*MAT.se, MAT-2*MAT.se), na.rm = T), 
													 n = 5), cex.axis = 0.7)
textbox(c(-50, -49),2,
				c("A)"), box = F, cex = 0.7)
#mtext("MAT (°C)", side = 2, line = 3)

par(new = TRUE)
shakun_stack <-read.csv(paste(refdata, 'Shakun_data.tab', sep = ''), sep = "\t")
shakun_stack[,1]= -1*shakun_stack[,1]
plot(shakun_stack[,1], shakun_stack[,5], ylim = c(-25, 8.5), xlim= c(-50, 0), lwd = 2.5, axes=F, ylab = '', xlab = '', type = "l")
abline(0,0, lty = 1.5)
axis(side = 4, at = pretty(range(shakun_stack[,5], na.rm=T), n = 5), cex.axis = 0.7, tick = T)
textbox(c(-50, -49),1,
				c("B)"), box = F, cex = 0.7)

par(new = TRUE)
gisp <- read.csv(paste(refdata, 'gisp2_temp.txt', sep =''), sep = "\t")
gisp[,1] = gisp[,1]*-1000;
gisp.mean = vector();
gisp.sd = vector();
gisp.count = vector();
for(i in 1:500){
	print((-i*100)+100)
	sub=subset(gisp[,2], gisp[,1] <= (-i*100)+100 & gisp[,1]>= ((-i*100)+100)-1000)
	gisp.mean[[i]] = mean(sub);
	gisp.sd[[i]] = sd(sub);
	gisp.count[[i]] = length(sub)
}
gisp.mean = gisp.mean-gisp.mean[[1]];
plot(-100*(500:1), rev(gisp.mean), 
		 type ='l', ylim = c(-80,60), 
		 xlim= c(-50000, 0), lwd = 1.5, 
		 axes=F, ylab = '', xlab = '')
abline(h=0)
axis(side = 2, at = pretty(range(gisp.mean, na.rm=T), n = 5), cex.axis = 0.7, tick = T)
textbox(c(-50000, -49000),5,
				c("C)"), box = F, cex = 0.7)

par(new = TRUE)

plot(tts, isl, typ='l', ylim = c(350, 800), axes=F, lwd = 2.5, ylab = '', xlab = '')
#abline(median(isl),0, lty = 3)
axis(side = 4, at = pretty(range(isl, na.rm=T), n = 5), cex.axis = 0.7, tick = T)
textbox(c(-50000, -49000),520,
				c("D)"), box = F, cex = 0.7)

par(new = TRUE)
co2 <- read.csv(paste(refdata, "antarctica2015co2.txt", sep = ''), sep = "\t")
co2 <- subset(co2, co2[,1] <= 50000)
co2[,1] = co2[,1] *-1;
co2 = subset(co2, co2[,1] <=0)
plot(co2[,1], co2[,2],type = "l", ylim = c(175, 1000), xlim= c(-50000, 0), lwd = 2.5, axes=F, ylab = '', xlab = '')
axis(side = 2, at = pretty(range(co2[,2], na.rm=T), n = 5), cex.axis = 0.7, tick = T)


mtext("CRACLE MAT anomaly (°C)", 
			side=2, line=3, cex=0.7,las=0, adj = 1)
mtext("N. Hemis. mean anomaly (°C)", side=4, 
			line=3, cex=0.7,las=0, adj = 0.73)
mtext("GISP2 MAT anomaly (°C)", side=2, line=3, 
			cex=0.7,las=0, adj = 0.5)

mtext("65°N June insolation [W/m2]", 
			side=4, line=3, cex=0.7,las=0, adj = 0.3)
mtext(expression(paste("Antarctic CO"^"2", " Composite (ppm)")), 
								 side=2, line=3, cex=0.7,las=0, adj = 0)
			mtext("kya", side=1, line=3, cex=0.7,las=0)
			textbox(c(-50000, -49000),320,
							c("E)"), box = F, cex = 0.7)
			
			
			dev.off()
			
			
			millav <- function(x){
				
				#x is a two column matrix with x[,1] containing ages in years (negative values) and x[,2] containing values to be averaged
				# co2 and gisp objects work in this
				#simple millenial averaging. Assuming 50ka range
				m = vector()
				for(i in -500:0){
					sub <- subset(x, x[,1] >= i*100 & x[,1] <= (i*100)+1000)
					m = c(m, mean(sub[,2]));
				}
				return(m)
			}
			
			
			
			
		
			
			tiff('figs/Figure_2.tif', height = 8.5, width = 5, units ='in', res = 450) ##Want: MaximumT.do_timelapse.1, MinimumT.do_timelapse.1, do_time.1.MAP,do_time.1.wbal,WINTERLEN.do_time.1
			m = rbind(c(1,1), c(2,2), c(3,3), c(4,4), c(5,5))
			layout(m)
			library(plotrix)
			v=2; #MinT
			par(mai=c(0,0.8,0.1,0))
			MinT = doit[[v]][,1]/10;
			MinT = MinT - MinT[501]
			MinT.se = (doit[[v]][,2]/10)/sqrt(doit[[v]][,3]);
			plot(0.1*(-500:0), c(MinT), type="l", lwd = 2.5, lty = 1, axes = F, xlab ='', cex.lab = 0.9, ylab = "Avg. Minimum Temp. \nAnomaly (°C)", ylim =c(-4.5,5), panel.first = grid(NULL, 0))
			rect(0.1*inc.inter[,1], -500, 0.1*inc.inter[,2], 3.5, density = -1, col = rgb(0,1,1.0,alpha=0.3), lwd=0)
			rect(0.1*dec.inter[,1], -500, 0.1*dec.inter[,2], 3.5, density = -1, col = rgb(0,0,1.0,alpha=0.3), lwd=0)
			points(0.1*(-500:0), c(MinT), type="l", lwd = 2.5, lty = 1)
			points(0.1*(-500:0), c(MinT)+1.96*MinT.se, type ='l', lty=3)
			points(0.1*(-500:0), c(MinT)-1.96*MinT.se, type ='l', lty=3)
			abline(h=0)
			axis(side=2, at = pretty(c(max(c(MinT)+1.96*MinT.se), min(c(MinT)-1.96*MinT.se)), n =5),cex.axis = 0.8, tick = TRUE)
			textbox(c(-50, -11.8),4.7,c("Pleistocene"), 
							box = T,cex =0.7, lwd=1.5, adj=c(0,0.2),
							fill ='grey', border = 'white', justify='c')
			textbox(c(-11.7,0),4.7,c("Holocene"), 
							box = T, cex = 0.7, lwd=1.5, adj=c(0,0.2), 
							fill ='grey', border = 'white', justify='c')
			textbox(c(-26,-19),4.1,c("LGM"), 
							box = T,  cex = 0.7, lwd=1.5, adj=c(0,0.2), 
							fill ='grey', border = 'white', justify='c')
			textbox(c(-50, -49),2,
							c("A)"), box = F, cex = 1.2)
			
			par(mai=c(0,0.8,0.1,0))
			v=3; #MaxT
			MaxT = doit[[v]][,1]/10;
			MaxT = MaxT-MaxT[501]
			MaxT.se = (doit[[v]][,2]/10)/sqrt(doit[[v]][,3]);
			plot(0.1*(-500:0), c(MaxT), type="l", lwd = 2.5, lty = 1, axes = F, xlab ='', cex.lab = 0.9, ylab = "Avg. Maximum Temp.\nAnomaly (°C)", ylim = c(-7,3), panel.first = grid(NULL, 0)) # first plot
			rect(0.1*inc.inter[,1], -500, 0.1*inc.inter[,2], 4, density = -1, col = rgb(0,1,1.0,alpha=0.3), lwd=0)
			rect(0.1*dec.inter[,1], -500, 0.1*dec.inter[,2], 4, density = -1, col = rgb(0,0,1.0,alpha=0.3), lwd=0)
			points(0.1*(-500:0), c(MaxT), type="l", lwd = 2.5, lty = 1)
			points(0.1*(-500:0), c(MaxT)+1.96*MaxT.se, type ='l', lty=3)
			points(0.1*(-500:0), c(MaxT)-1.96*MaxT.se, type ='l', lty=3)
			abline(h=0)
			box('outer')
			
			axis(side=2, at = pretty(c(max(c(MaxT)+1.96*MaxT.se), min(c(MaxT)-1.96*MaxT.se)), n =5),cex.axis = 0.8, tick = TRUE)
			textbox(c(-50, -49),2,
							c("B)"), box = F, cex = 1.2)
			
			v=4; #MAP
			par(mai=c(0,0.8,0.1,0))
			
			MAPT = doit[[v]][,1];
			MAPT = MAPT - MAPT[501]
			MAPT.se = (doit[[v]][,2])/sqrt(doit[[v]][,3]);
			plot(0.1*(-500:0), c(MAPT), type="l", lwd = 2.5, lty = 1, axes = F, xlab ='', cex.lab = 0.9, ylab = "Ann. Precip. \nAnomaly (mm)", ylim = c(-75, 125), panel.first = grid(NULL, 0)) # first plot
			rect(0.1*inc.inter[,1], -500, 0.1*inc.inter[,2], 500, density = -1, col = rgb(0,1,1.0,alpha=0.3), lwd=0)
			rect(0.1*dec.inter[,1], -500, 0.1*dec.inter[,2], 500, density = -1, col = rgb(0,0,1.0,alpha=0.3), lwd=0)
			points(0.1*(-500:0), c(MAPT), type="l", lwd = 2.5, lty = 1)
			points(0.1*(-500:0), c(MAPT)+1.96*MAPT.se, type ='l', lty=3)
			points(0.1*(-500:0), c(MAPT)-1.96*MAPT.se, type ='l', lty=3)
			abline(h=0)
			axis(side=2, at = pretty(c(max(c(MAPT)+1.96*MAPT.se), min(c(MAPT)-1.96*MAPT.se)), n =5),cex.axis = 0.8, tick = TRUE)
			textbox(c(-50, -49),100,
							c("C)"), box = F, cex = 1.2)
			
			v=5; #wbal
			par(mai=c(0,0.8,0.1,0))
			
			wbal = doit[[v]][,1];
			wbal = wbal-wbal[501]
			wbal.se = (doit[[v]][,2])/sqrt(doit[[v]][,3]);
			plot(0.1*(-500:0), c(wbal), type="l", lwd = 2.5, lty = 1, axes = F, xlab ='', cex.lab = 0.9, ylab = "Ann. Water Balance \nAnomaly (mm)", ylim =c(-250, 400), panel.first = grid(NULL, 0)) # first plot
			rect(0.1*inc.inter[,1], -500, 0.1*inc.inter[,2], 500, density = -1, col = rgb(0,1,1.0,alpha=0.3), lwd=0)
			rect(0.1*dec.inter[,1], -500, 0.1*dec.inter[,2], 500, density = -1, col = rgb(0,0,1.0,alpha=0.3), lwd=0)
			points(0.1*(-500:0), c(wbal), type="l", lwd = 2.5, lty = 1)
			points(0.1*(-500:0), c(wbal)+1.96*wbal.se, type ='l', lty=3)
			points(0.1*(-500:0), c(wbal)-1.96*wbal.se, type ='l', lty=3)
			abline(h=0)

			axis(side=2, at = pretty(c(max(c(wbal)+1.96*wbal.se), min(c(wbal)-1.96*wbal.se)), n =5),cex.axis = 0.8, tick = TRUE)
			textbox(c(-50, -49),400,
							c("D)"), box = F, cex = 1.2)
			
			legend(-50, -100, c('Significant Div. Increase', 'Significant Div. Decrease'), fill = c(rgb(0,1,1.0,alpha=0.5), rgb(0,0,1.0,alpha=0.5)))
			
			
			
			v=6; #winter
			win = doit[[v]][,1];
			win = win-win[501]
			win.se = (doit[[v]][,2])/sqrt(doit[[v]][,3]);
			par(mai=c(0.6,0.8,0.1,0))
			plot(0.1*(-500:0), c(win), type="l", lwd = 2.5, 
					 lty = 1, axes = F, xlab ='', cex.lab = 0.9, 
					 ylab = "Winter Length\nAnomaly (months)", ylim = c(-1.5,3.5), panel.first = grid(NULL, 0)) # first plot
			rect(0.1*inc.inter[,1], -1.5, 0.1*inc.inter[,2], 4, density = -1, col = rgb(0,1,1.0,alpha=0.3), lwd=0)
			rect(0.1*dec.inter[,1], -1.5, 0.1*dec.inter[,2], 4, density = -1, col = rgb(0,0,1.0,alpha=0.3), lwd=0)
			points(0.1*(-500:0), c(win), type="l", lwd = 2.5, lty = 1)
			points(0.1*(-500:0), c(win)+1.96*win.se, type ='l', lty=3)
			points(0.1*(-500:0), c(win)-1.96*win.se, type ='l', lty=3)
			abline(h=0)
			axis(side=2, at = pretty(c(max(c(win)+1.96*win.se), min(c(win)-1.96*win.se)), n = 4),cex.axis = 0.8, tick = TRUE)
			axis(side = 1, at = pretty(0.1*(-500:0), n = 10), labels=seq(50,0,-5), cex = 0.7)
			mtext("kya", side=1, line=3, cex=0.7,las=0)
			textbox(c(-50, -49),2,
							c("E)"), box = F, cex = 1.2)
		
			dev.off()
			
			
			## LGM projected wbalann (Thornwaite's water balance)
			
			#subset mergegauss for wbalann to include LGM only
			i=364 #Verify this is wbalan_condi.origk
			print(varlist[[i]])
			filegauss = files[grep(varlist[[i]], files)]
			
			datagauss <- read.table(paste(out, filegauss, sep="/"))
			mergegauss <- merge(site_data, datagauss, by.x = 'analysis_id', by.y = 'V1')
			
			mergegauss$plot_age = -1*mergegauss$plot_age;
			mergegauss$plot_agestdev = -1*mergegauss$plot_agestdev;
			head(mergegauss)
			m = subset(mergegauss, mergegauss$plot_agestdev <= -19000 & mergegauss$plot_age >= -26000)
			m.holo = subset(mergegauss, mergegauss$plot_agestdev <= -2000 & mergegauss$plot_age >= -8000)
			#m.holo = subset(m.holo, m.holo$plot_agestdev <= -4000 & m.holo$plot_age >= -6000)
			
			elev=vector()
			for(nn in 1:nrow(m)){
				elev[[nn]] = as.numeric(strsplit(as.character(m$elev_mean[nn]), split = " ")[[1]][1])
			}
			m$elev_mean = elev
			diff = (m$V3+m$V4)/2 - m$V2
			plot(m$lat, diff, ylim=c(-2000, 2000))
			cor.test(m$lat, diff); #0.35, p=0.006
			cor.test(m$elev_mean, diff); #not sig
			cor.test(m$lon, diff); #not sig
			
			l = lm(diff ~ m$lat)
			c = confint(l, level = 0.4);
			abline(l)
			abline(c[[1]], c[[2]])
			abline(c[[3]], c[[4]])
			library(raster)
			library(rgdal)
			#build lm against latitude (it is significant)
			
			spts = rasterToPoints(r[['wbalann']])
			#with parameters from the linear model get an adjustment value
			spts <- cbind(spts, cbind(l[[1]][[2]]*spts[,2]+l[[1]][[1]]))
			#plot(subset(spts, spts[,3]+spts[,4] > 0)) #apply the adjustment do the entire grid and plot
			lgm.w = rasterFromXYZ(subset(spts, spts[,3]+spts[,4] > -100000), crs = crs(r))
			ext2 = extent(c(-125, -105, 25,45))
			lgm.w = crop(lgm.w, ext2)
			##Holocene wb
			
			elev=vector()
			for(nn in 1:nrow(m.holo)){
				elev[[nn]] = as.numeric(strsplit(as.character(m.holo$elev_mean[nn]), split = " ")[[1]][1])
			}
			m.holo$elev_mean = elev
			diffh = (m.holo$V3+m.holo$V4)/2 - m.holo$V2
			plot(m.holo$lat, diffh, ylim=c(-2000, 2000))
			cor.test(m.holo$lat, diffh); #0.33, p = 0.000
			cor.test(m.holo$elev_mean, diffh); #not sig
			cor.test(m.holo$lon, diffh); #not sig
			
			lh = lm(diffh ~ m.holo$lat)
			ch = confint(lh, level = 0.4);
			abline(lh)
			abline(ch[[1]], ch[[2]])
			abline(ch[[3]], ch[[4]])
			library(raster)
			library(rgdal)
			#build lm against latitude (it is significant)
			sptsh = rasterToPoints(r[['wbalann']])
			#with parameters from the linear model get an adjustment value
			sptsh <- cbind(sptsh, cbind(lh[[1]][[2]]*sptsh[,2]+lh[[1]][[1]]))
			#plot(subset(spts, spts[,3]+spts[,4] > 0)) #apply the adjustment do the entire grid and plot
			holo.w = rasterFromXYZ(subset(sptsh, sptsh[,3]+sptsh[,4] > -100000), crs = crs(r))
			holo.w = crop(holo.w,ext2)
			
			
			library(rasterVis)
			library(gridExtra)
			library(latticeExtra)
			
			breaks <- c(-Inf, -1200, -800, -400, 0, 400, 800, 1200, Inf, NA)
			#cols <- colorRampPalette(c( 'gray25', 'gray55','gray85','blue3','dodgerblue4'))(length(breaks)-1)
			cols <- colorRampPalette(c( 'red4', 
																	
																	'red',
																	'orangered3',
																	'goldenrod2',
																	
																	'aquamarine4',
																	'blue', 
																	'darkblue', 
																	'purple4', 'black'))(length(breaks)-1)
			
			## Use `at` and `col.regions` arguments to set the color scheme
			st = stack(c(lgm.w[[1]]+lgm.w[[2]], holo.w[[1]], holo.w[[1]]+holo.w[[2]]))
			names(st) = c("LGM", "Modern Avg.",  "Holocene")
			
			myTheme <- BTCTheme()
			myTheme$panel.background$col = 'black' 
			p1 = levelplot(st, at=breaks,
										 margin=FALSE,
										 col.regions=cols,
										 colorkey=list(title="Water Balance \n(mm)\n", 
										 							title.gpar = list(cex = 0.65), 
										 							lab.gpar = list(cex = 0.65), 
										 							vjust=4),
										 xlab=list("Longitude", cex=0.7),
										 ylab=list("Latitude", cex=0.7),
										 p.strip.text=list(cex=0.7),
										 
										 main=list('C) Geographic Projection', cex=0.9),
										 scales=list(x=list(cex=0.65), y=list(cex=0.65), alternating=3),
										 par.settings=myTheme
			) 
		#		layer(sp.polygons(usa1)) +
		#		layer(sp.polygons(mx1))  +
		#		layer(sp.polygons(mx0, lwd=2)) +
		#		layer(sp.polygons(usa0, lwd=2))
			p4 = xyplot(diff ~ m$lat, data.frame(cbind(m$lat, diff)), pch = 20, cex = 0.65, xlim = c(30,40), main=list('A) LGM Regression',cex=0.9), ylim = c(-400, 1000),
									xlab = list("Latitude",cex=0.65), ylab = list("Estimated change in\nwater balance (mm)", cex=0.65), type = c("p", "r"), col = 'black', scales=list(x=list(cex=0.65), y=list(cex=0.65)))# +
			#      layer(panel.ablineq(l[[1]][[1]], l[[1]][[2]], col ='darkgrey', adj=c(0.5,-1), cex = 0.8))
			
			p5 = xyplot(diffh ~ m.holo$lat, data.frame(cbind(m.holo$lat, diffh)), main=list("B) Holocene Regression", cex=0.9), pch = 20, cex = 0.65, xlim = c(30,40), ylim = c(-1000, 500),
									xlab = list("Latitude", cex =0.65), ylab = list("Estimated change in\nwater balance (mm)", cex=0.65), type = c("p", "r"), col = 'black', scales=list(x=list(cex=0.65), y=list(cex=0.65))) #+
			#  layer(panel.ablineq(lh[[1]][[1]], lh[[1]][[2]], col ='darkgrey', adj=c(0.5,1.5), cex =0.8))
			
			
			png('./figs/Figure_3.png', pointsize = 2, 
					height=5.25, width =7.25, res = 500, units='in')
			grid.arrange(arrangeGrob(p4, p5,  nrow =1), 
									 arrangeGrob(p1, nrow=1),
									 widths=c(1, 1), heights=c(0.7,1),
									 layout_matrix= rbind(c(1,1),c(2)))
			dev.off()
			
			#layout(rbind(c(1,2), c(3,3)))
			
			