

source("./sim_index.R")
require(readr)
require(readxl)
require(plyr)
require(ggplot2)
require(geomnet)
options(stringsAsFactors = F)


# intro time series of RTY and 1x and 3x ETFs
do_fig_rty6mo_intro <- function()
{
  plot_df1 <- price_val$RTY[names(price_val$RTY) >= 20151015]
  plot_df1 <- data.frame(Date = ymd(names(plot_df1), tz = "UTC"),
                         Return = (plot_df1 / price_val$RTY["20151015"]) - 1,
                         Ticker = "RTY")
  plot_df2 <- price_val$IWM[names(price_val$IWM) >= 20151015]
  plot_df2 <- data.frame(Date = ymd(names(plot_df2), tz = "UTC"),
                         Return = (plot_df2 / price_val$IWM["20151015"]) - 1,
                         Ticker = "IWM")
  plot_df3 <- price_val$TNA[names(price_val$TNA) >= 20151015]
  plot_df3 <- data.frame(Date = ymd(names(plot_df3), tz = "UTC"),
                         Return = (plot_df3 / price_val$TNA["20151015"]) - 1,
                         Ticker = "TNA")
  plot_df0 <- rbind(plot_df1, plot_df2, plot_df3)
  plot_df0$Return <- plot_df0$Return * 100.0
  
  # Format Date
  # review axis label sizes, fonts, etc
  # add annotation
  
  text_df <- data.frame(Date = ymd(c(20151115, 20151115, 20151115), tz = "UTC"),
  											Return = c(-33.0, -28.0, -38.0),
  											Value = subset(plot_df0, Date == ymd(20160415, tz = "UTC"))$Return,
  											Ticker = subset(plot_df0, Date == ymd(20160415, tz = "UTC"))$Ticker)
  text_df$Label <- paste("R[", text_df$Ticker, "]==",
  											 formatC(text_df$Value, digits = 2, format = "f"), "*'%'", sep = "")

  gg <- ggplot(plot_df0)
  gg <- gg + geom_line(aes(Date, Return, colour = Ticker), size = 0.5)
  gg <- gg + ylab("Return (%)")
  gg <- gg + geom_text(aes(Date, Return, colour = Ticker, label = Label), size = 7,
  										 data = text_df, parse = T, show.legend = F)
  gg <- gg + scale_x_datetime(breaks = ymd(c("20151101", "20151201", "20160101",
  																					 "20160201", "20160301", "20160401"),
  																				 tz = "UTC"),
  														labels = c("11/1/15", "12/1/15", "1/1/16",
                                				 "2/1/16", "3/1/16", "4/1/16"))
  gg <- gg + theme(legend.position = "bottom",
  								 axis.text = element_text(size = 16),
                   axis.title = element_text(size = 16),
  								 legend.text = element_text(size = 16),
                   legend.title = element_text(size = 16))
  gg
}


# visualize regions of interest with actual 1x and 3x ETF data
do_fig_rty62day_returns <- function()
{
	plot_df <- do_findataframe(list(RTY = gross_val$RTY,
																	IWM = gross_val$IWM,
																	TNA = gross_val$TNA),
														 20081105, 20160415, missing = "Returns")
	plot_df <- data.frame(IWM = (apply(1 + do_findatamatroll(plot_df$IWM, 62), 1, prod) - 1) * 100,
												TNA = (apply(1 + do_findatamatroll(plot_df$TNA, 62), 1, prod) - 1) * 100,
												RVol = (apply(log(1 + do_findatamatroll(plot_df$RTY, 62)),
																			1, sd) * sqrt(62) * sqrt(4)) * 100)
	
	vol_max <- ceiling(max(plot_df$RVol) / 10)
  vol_colors <- c("#330000","#660000","#990000","#CC0000","#FF0000",
                            "#FF3300","#FF6600","#FF9900","#FFCC00","#FFFF00","#FFFF33")
  
	gg <- ggplot(subset(plot_df, IWM < 50))
	gg <- gg + geom_abline(slope = 3, colour = "black")
	gg <- gg + geom_vline(xintercept = 0, colour = "black")
	gg <- gg + geom_hline(yintercept = 0, colour = "black")
	gg <- gg + geom_point(aes(IWM, TNA, colour = RVol), size = 0.5)
	gg <- gg + scale_colour_gradientn(name = expression(paste(sigma[252])),
																		colours = vol_colors[1:vol_max],
																		breaks = c(20, 40, 60))
	gg <- gg + xlab("IWM Returns (%)") + xlim(c(-50, 50))
	gg <- gg + ylab("TNA Returns (%)") + ylim(c(-100, 200))
	gg <- gg + theme(legend.position = "right",
  								 axis.text = element_text(size = 16),
                   axis.title = element_text(size = 16),
  								 legend.text = element_text(size = 16),
                   legend.title = element_text(size = 16))
	gg
}


# visualize regions of interest with actual 1x and 3x ETF data
do_fig_rty62day_levdex <- function()
{
	plot_df <- do_findataframe(list(RTY = gross_val$RTY,
																	RTYx1 = gross_val$RTY,
																	RTYx3 = 3*gross_val$RTY),
														 19790102, 20160415, missing = "Returns")
	plot_df <- data.frame(RTYx1 = (apply(1 + do_findatamatroll(plot_df$RTYx1, 62),
																			 1, prod) - 1) * 100,
												RTYx3 = (apply(1 + do_findatamatroll(plot_df$RTYx3, 62),
																			 1, prod) - 1) * 100,
												RVol = (apply(log(1 + do_findatamatroll(plot_df$RTY, 62)),
																			1, sd) * sqrt(62) * sqrt(4)) * 100)
	
	vol_max <- ceiling(max(plot_df$RVol) / 10)
  vol_colors <- c("#330000","#660000","#990000","#CC0000","#FF0000",
                            "#FF3300","#FF6600","#FF9900","#FFCC00","#FFFF00","#FFFF33")
  
	gg <- ggplot(subset(plot_df, RTYx1 < 50))
	gg <- gg + geom_abline(slope = 3, colour = "black")
	gg <- gg + geom_vline(xintercept = 0, colour = "black")
	gg <- gg + geom_hline(yintercept = 0, colour = "black")
	gg <- gg + geom_point(aes(RTYx1, RTYx3, colour = RVol), size = 0.5)
	gg <- gg + scale_colour_gradientn(name = expression(paste(sigma[252])),
																		colours = vol_colors[1:vol_max],
																		breaks = c(20, 40, 60))
	gg <- gg + xlab("RTYx1 Returns (%)") + xlim(c(-50, 50))
	gg <- gg + ylab("RTYx3 Returns (%)") + ylim(c(-100, 200))
	gg <- gg + theme(legend.position = "right",
  								 axis.text = element_text(size = 16),
                   axis.title = element_text(size = 16),
  								 legend.text = element_text(size = 16),
                   legend.title = element_text(size = 16))
	gg
}


# hist demo
do_fig_histdemo <- function()
{
	bw = 1
  mu = c(-1.6, -0.6, 1.6)
  plot_df <- data.frame(x = mu)
  
  p <- ggplot(plot_df)
  p <- p + geom_histogram(aes(x), binwidth = 1, colour = "grey")
  p <- p + annotate("bar", x = -2, y = 1, colour = "red", fill = "grey") + annotate("point", x = mu[1], y = 0, size = 3, colour = "red")
  p <- p + annotate("bar", x = -1, y = 1, colour = "green", fill = "grey") + annotate("point", x = mu[2], y = 0, size = 3, colour = "green")
  p <- p + annotate("bar", x = 2, y = 1, colour = "blue", fill = "grey") + annotate("point", x = mu[3], y = 0, size = 3, colour = "blue")
  p <- p + scale_x_continuous("x", limits = c(-5, 5), breaks = c(-5:5))
  p <- p + scale_y_continuous("count", breaks = c(0, 1))
  p
}


# kde demo
do_fig_kdedemo <- function()
{
	bw = 1
  mu = c(-1.6, -0.6, 1.6)
  plot_df <- data.frame(x = seq(-5, 5, 0.01))
  plot_df$K1 <- (1 / 3) * dnorm(plot_df$x, mu[1], bw)
  plot_df$K2 <- (1 / 3) * dnorm(plot_df$x, mu[2], bw)
  plot_df$K3 <- (1 / 3) * dnorm(plot_df$x, mu[3], bw)
  plot_df$density <- (plot_df$K1 + plot_df$K2 + plot_df$K3)
  
  p <- ggplot(plot_df)
  p <- p + geom_density(aes(x), bw = bw, colour = "white", fill = "grey", data = data.frame(x = mu))
  p <- p + geom_line(aes(x, K1), colour = "red") + annotate("point", x = mu[1], y = 0, size = 3, colour = "red")
  p <- p + geom_line(aes(x, K2), colour = "green") + annotate("point", x = mu[2], y = 0, size = 3, colour = "green")
  p <- p + geom_line(aes(x, K3), colour = "blue") + annotate("point", x = mu[3], y = 0, size = 3, colour = "blue")
  p <- p + scale_x_continuous("x", limits = c(-5, 5), breaks = c(-5:5))
  p <- p + scale_y_continuous("density", breaks = c(0, 0.3))
  p
}


# kde demo
do_fig_kdedemo_2D <- function()
{
	bw = 0.5 * (1 / 3)
	r <- c(0.7, 0.4, 0.5, 0.1)
	plot_df <- data.frame(x = r[1:(length(r) - 1)],
	                      y = r[2:length(r)],
												c = c("red", "green", "blue"))
	cp <- seq(0, 2 * pi, length.out = 100)

	gg <- ggplot(plot_df)
	for(i in 1:nrow(plot_df)) {
		gg <- gg + annotate("point", x = plot_df$x[i], y = plot_df$y[i], size = 3, colour = plot_df$c[i])
		gg <- gg + annotate("path", x = plot_df$x[i] + bw * cos(cp),
												y = plot_df$y[i] + bw * sin(cp), colour = plot_df$c[i])
		gg <- gg + annotate("path", x = plot_df$x[i] + 2 * bw * cos(cp),
												y = plot_df$y[i] + 2 * bw * sin(cp), colour = plot_df$c[i], size = 0.1)
	}
	gg <- gg + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
	gg <- gg + geom_abline(slope = -1, intercept = 1, colour = "orange")
	gg <- gg + xlim(c(-0.5, 1.5)) + ylim(c(-0.5, 1.5))
	gg <- gg + xlab("Log Return (%), Day 1") + ylab("Log Return (%), Day 2")
	gg
}


# visualize regions of interest
do_fig_regions_interest <- function(expiry = 20160415, traded = 20160115)
{
	expiry = 20160415; traded = 20160115
  load(paste("./letf_rfinance_RTY_options_", traded, "_", expiry, ".RData", sep = ""))
  
  curve_df <- data.frame(x = seq(-0.33, 0.5, 0.01))
  curve_df$y <- (1 + 3 * ((1 + curve_df$x) ^ (1 / p) - 1)) ^ p - 1
  curve_df$x <- curve_df$x * 100
  curve_df$y <- curve_df$y * 100
  
  a <- (under_prices[paste(expiry), "IWM"] - under_prices[paste(traded), "IWM"]) /
  	under_prices[paste(traded), "IWM"]
  b <- (under_prices[paste(expiry), "TNA"] - under_prices[paste(traded), "TNA"]) /
  	under_prices[paste(traded), "TNA"]
  s <- 5
  
  c <- ggplot(curve_df)
  c <- c + geom_line(aes(x, y), colour = "blue", size = 2)
  c <- c + geom_vline(xintercept = 0, colour = "black")
  c <- c + geom_hline(yintercept = 0, colour = "black")
  #c <- c + annotate("point", x = a*100, y = b*100)  #, label = paste(traded))
  c <- c + geom_vline(xintercept = 15, colour = "red", size = 2)
  c <- c + geom_hline(yintercept = 65, colour = "green", size = 2)
  c <- c + annotate("text", x = 25, y = 153, label = "+1x Call Strike",
  									colour = "red", size = s)
  c <- c + annotate("text", x = -20, y = 80, label = "+3x Call Strike",
  									colour = "green", size = s)
  c <- c + annotate("text", x = 35, y = 210, label = "+3x LETF MAX",
  									colour = "blue", size = s)
  c <- c + xlab("+1x ETF Return (%)") + ylab("+3x ETF Return (%)")
  c <- c + theme(axis.text = element_text(size = 16),
                 axis.title = element_text(size = 16))
  c
}


# demonstrate payouts over ETF and LETF strikes
do_fig_option_payouts <- function(expiry = 20160415, traded = 20151215)
{
	#expiry = 20160415; traded = 20160115
  load(paste("./letf_rfinance_RTY_options_", traded, "_", expiry, ".RData", sep = ""))
  # Important
  plot_df <- subset(plot_df, returnTNAC >= limitIWMC)
  
  gg <- ggplot(plot_df)
  gg <- gg + geom_point(aes(results, meanW3UC_payout, colour = (netResults > 0)))
  gg <- gg + xlab("Actual Profit ($)") + xlim(c(0, 7000))
  gg <- gg + ylab("Mean Forecast Profit ($)") + ylim(c(0, 7000))
  gg <- gg + scale_colour_manual("Result", values = c("red", "black"),
  															 breaks = c(F, T), labels = c("Loser", "Winner"))
  gg <- gg + theme(legend.position = "right",
  								 axis.text = element_text(size = 16),
                   axis.title = element_text(size = 16),
  								 legend.text = element_text(size = 16),
                   legend.title = element_text(size = 16))
  gg
}


# aux function used to calculate payouts
do_option_payout <- function(R_sim_list, option_strikes, under_close, call = T, beta = 1)
{
  under_finals <- under_close * apply(1 + beta * R_sim_list[[1]], 1, prod)
	expiry_payout <-  (rep(1, length(option_strikes)) %*% t(under_finals)) - option_strikes
	if (length(R_sim_list) > 1) {
		under_finals <- under_close * t(sapply(1:length(R_sim_list),
																					 function(i) apply(1 + beta * R_sim_list[[i]], 1, prod)))
	  expiry_payout <- under_finals - option_strikes
	}
	expiry_payout <- ((-1) ^ (call != T)) * expiry_payout
	expiry_payout <- apply(cbind(as.vector(expiry_payout), 0.0), 1, max, na.rm = T)
	expiry_payout <- matrix(expiry_payout, length(option_strikes), nrow(R_sim_list[[1]])) 
  expiry_payout
}


# Using the data for 20160415 expiration, simulates data for a strategy consisting of
#  write 1 share 3x BULL CALL, buy 3 shares 1x BULL CALL
do_simdata_W3UC <- function(expiry = 20160415, traded = 20151215, k = 10000, mult = 3)
{
	#expiry = 20160415; traded = 20160212; k = 10000; mult = 3
	# store num days in period, rolling p day returns of underlying index, and underlying prices
	p <- length(names(price_val$RTY)[names(price_val$RTY) >= traded &
																	 	names(price_val$RTY) <= expiry]) - 1
	R_mat <- do_findatamatroll(gross_val$RTY, p)
	under_prices <- do_findataframe(list(RTY = price_val$RTY,
																			 IWM = price_val$IWM,
																			 TNA = price_val$TNA),
																	traded, expiry, missing = "Remove")

	# 1x BULL PUT details
	IWMC_trades <- ivol_IWMC_mat[rownames(ivol_IWMC_mat) == traded, ]
  IWMC_trades <- t(IWMC_trades[, is.na(IWMC_trades) == F])
  IWMC <- data.frame(ivols = as.vector(IWMC_trades))
  IWMC_trades <- priceIWMC_mat[rownames(priceIWMC_mat) == traded, ]
  IWMC_trades <- t(IWMC_trades[, is.na(IWMC_trades) == F])
  IWMC$closes <- as.vector(IWMC_trades)
  IWMC$strikes <- as.numeric(sub("X", "", rownames(IWMC_trades)))
  IWMC$returns <- (IWMC$strikes - under_prices[paste(traded), "IWM"]) /
  	under_prices[paste(traded), "IWM"]
  IWMC$results <- apply(cbind((under_prices[paste(expiry), "IWM"] - IWMC$strikes), 0), 1, max)
  
  # ETF option return thresholds for ensuring LETF option does not payout
  IWMC$limits <- (1 + 3 * ((1 + IWMC$returns) ^ (1 / p) - 1)) ^ p - 1
  
  # 3x BULL PUT details
  TNAC_trades <- ivol_TNAC_mat[rownames(ivol_TNAC_mat) == traded, ]
  TNAC_trades <- t(TNAC_trades[, is.na(TNAC_trades) == F])
  TNAC <- data.frame(ivols = as.vector(TNAC_trades))
  TNAC_trades <- priceTNAC_mat[rownames(priceTNAC_mat) == traded, ]
  TNAC_trades <- t(TNAC_trades[, is.na(TNAC_trades) == F])
  TNAC$closes <- as.vector(TNAC_trades)
  TNAC$strikes <- as.numeric(sub("X", "", rownames(TNAC_trades)))
  TNAC$returns <- (TNAC$strikes - under_prices[paste(traded), "TNA"]) /
  	under_prices[paste(traded), "TNA"]
  TNAC$results <- apply(cbind((under_prices[paste(expiry), "TNA"] - TNAC$strikes), 0), 1, max)
  
  # final actual, payouts, write 1 share 3x BULL CALL, buy 3 shares 1x BULL CALL
  W3UC_prems <- mult * -100 * IWMC$closes + 100 * (rep(1, nrow(IWMC)) %*% t(TNAC$closes))
  W3UC_results <- mult * 100 * IWMC$results - 100 * (rep(1, nrow(IWMC)) %*% t(TNAC$results))
  W3UC_netResults <- W3UC_results + W3UC_prems

  # simulate data s.t. returns are greater than IWMC_return
  R_sim <- list()
  for(i in 1:nrow(IWMC)) {
  	R_sim[[paste("X", IWMC$strikes[i], sep = "")]] <- sim_index(R_mat,
  																															r_0 = IWMC$returns[i],
  																															k = k, h_factor = 0.005,
  																															r_ab = Inf)
  }
  
  # IWMC Payout distributions
  IWMC_payouts <- do_option_payout(R_sim, IWMC$strikes,
  																 under_prices[paste(traded), "IWM"],
  																 call = T, beta = 1)
  
  # TNAC and net, final Payout distributions.  One iteration for each strike on IWMC
  TNAC_payouts <- list()
  W3UC_payouts <- list()
  W3UC_netPayouts <- list()
  for(i in 1:nrow(IWMC)) {
  	lindex <- paste("X", IWMC$strikes[i], sep = "")
  	TNAC_payouts[[lindex]] <- do_option_payout(R_sim[i], TNAC$strikes,
  																             under_prices[paste(traded), "TNA"],
  																						 call = T, beta = 3)
  	W3UC_payouts[[lindex]] <- mult * 100 * (rep(1, nrow(TNAC)) %*% t(IWMC_payouts[i, ]))
  	W3UC_payouts[[lindex]] <- W3UC_payouts[[lindex]] - 100 * TNAC_payouts[[lindex]]
  	W3UC_netPayouts[[lindex]] <- W3UC_payouts[[lindex]] + as.vector(t(W3UC_prems[i, ]))
  }
  
  # normalized df for easy plotting
  plot_df <- data.frame()
  for(i in 1:nrow(IWMC)) {
  	lindex <- paste("X", IWMC$strikes[i], sep = "")
  	melt_df <- data.frame(strikeIWMC = IWMC$strikes[i],
  												strikeTNAC = TNAC$strikes,
  												closeIWMC = IWMC$closes[i],
  												closeTNAC = TNAC$closes,
  												returnIWMC = IWMC$returns[i],
  												returnTNAC = TNAC$returns,
  												ivolIWMC = IWMC$ivols[i],
  												ivolTNAC = TNAC$ivols,
  												prems = W3UC_prems[i, ],
  												results = W3UC_results[i, ],
  												netResults = W3UC_netResults[i, ],
  												limitIWMC = IWMC$limits[i],
  												meanIWMC_payout = mean(IWMC_payouts[i, ], na.rm = T),
  												meanTNAC_payout = rowMeans(TNAC_payouts[[lindex]], na.rm = T),
  												meanW3UC_payout = rowMeans(W3UC_payouts[[lindex]], na.rm = T),
  												meanW3UC_netPayout = rowMeans(W3UC_netPayouts[[lindex]], na.rm = T),
  												meanProb = -W3UC_prems[i, ] /
  													rowMeans(W3UC_payouts[[lindex]], na.rm = T),
                          meanReturn = rowMeans(W3UC_netPayouts[[lindex]], na.rm = T) /
  													-W3UC_prems[i, ],
                          sdProb = abs(W3UC_prems[i, ]) * apply(1 / W3UC_payouts[[lindex]],
                          																	     1, sd, na.rm = T),
                          sdReturn = abs(1 / W3UC_prems[i, ]) * apply(W3UC_netPayouts[[lindex]],
                          																					   1, sd, na.rm = T))
  	plot_df <- rbind(plot_df, melt_df)
  }
  
  # save for plotting and evaluation
  save(expiry, traded, k, mult, p, R_mat, under_prices,
  		 IWMC, TNAC, R_sim, W3UC_prems, W3UC_results, W3UC_netResults,
  		 IWMC_payouts, TNAC_payouts, W3UC_payouts, W3UC_netPayouts,
  		 plot_df,
  		 file = paste("./letf_rfinance_RTY_options_", traded, "_", expiry, ".RData", sep = ""))
}


# prep oil dataset
do_oildata <- function()
{
	#
  # INDEX/ETF PRICES AND RETURNS
  #
  
  # read in raw prices/returns and reference date sequence
	# set first 2 cols as text because of bad Bloomberg output
  under_raw <- data.frame(read_excel("./LETF_Indexes_20160415_Russell2000_formulas.xlsx",
  									                 col_types = c("text", "text", "date", "numeric",
  									      							           "date", "numeric", "date", "numeric",
  																                 "date", "numeric", "date", "numeric",
  									      							           "date", "numeric", "date", "numeric",
  																                 "date", "numeric", "date", "numeric",
  									      							           "date", "numeric", "date", "numeric",
  																                 "date", "numeric", "date", "numeric",
  									      							           "date", "numeric", "date", "numeric",
  																                 "date", "numeric", "date", "numeric",
  									      							           "date", "numeric", "date", "numeric"), skip = 1))
  price_raw <- under_raw[, c(3, 4, 11, 12, 19, 20, 27, 28, 35, 36)]
  gross_raw <- under_raw[, c(5, 6, 13, 14, 21, 22, 29, 30, 37, 38)]
 
  # rename raw data
  names_raw <- c("DATE.RTY", "RTY", "DATE.IWM", "IWM", "DATE.RWM", "RWM",
  							 "DATE.TNA", "TNA", "DATE.TZA", "TZA")
  colnames(price_raw) <- names_raw
  colnames(gross_raw) <- names_raw
  
  # transform raw dfs into lists
  price_val <- do_findatalist(price_raw, missing = "Remove", date.format = "ymd")
  gross_val <- do_findatalist(gross_raw, missing = "Remove", date.format = "ymd", multiple = 0.01)

  # ticker and name references
  namedf_tickers <- data.frame(read_csv("./LETF_Indexes_20160415_Reference.csv"))
  namedf_tickers$Side <- with(namedf_tickers, ifelse(Side == "LONG", "Long",
                                                     ifelse(Side == "SHORT", "Short", Side)))
  
  # save
  save(under_raw, namedf_tickers,
  		 price_raw, gross_raw,
  		 price_val, gross_val,
  		 file = "./LETF_Indexes_20160415_Russell2000.RData")
  
  #
  # OPTIONS PRICES
  #
  
  # execute loop 8 times (4 ETFs x call/put)
  data_name <- c("IWMC", "IWMP", "RWMC", "RWMP", "TNAC", "TNAP", "TZAC", "TZAP")
  for(j in 1:length(data_name)) {
  	# set first 2 cols as text because of bad Bloomberg output
  	col_types <- rep(c("text", "text"), 401)
  	# read in raw prices/returns and reference date sequence
  	data_raw <- data.frame(read_excel("./RTY_LETF_Options_20160415_Prices_Formulas.xlsx",
  																		sheet = j, col_types = col_types))
  	# revise column names and remove columns without any valid prices (strikes without trades)
  	colnames(data_raw)[seq(2, 802, 2) - 1] <- rep("DATE", 401)
  	na_cols <- is.na(data_raw[1, 1:ncol(data_raw)])
  	keepers <- which(na_cols != T & colnames(data_raw) != "DATE")
  	keepers <- as.vector(sapply(1:length(keepers), function(x) c(keepers[x] - 1, keepers[x])))
  	data_raw <- data_raw[, keepers]
  	# convert raw data to dates and numerics as a list with all na's removed
  	pr_cols <- seq(2, ncol(data_raw), 2)
  	for(i in c(pr_cols - 1)) {
  		data_raw[, i] <- as.Date(as.numeric(data_raw[, i]), origin = "1899-12-30")
  		data_raw[, i + 1] <- as.numeric(data_raw[, i + 1])
  	}
  	data_val <- do_findatalist(data_raw, missing = "Remove", date.format = "ymd")
  	# also create a clean, date-aligned matrix/df
  	#  20150824 is the earliest trade date across all 8 option groups
  	data_mat <- do_findataframe(data_val, 20150824, 20160415)
  															#min(as.vector(as.matrix(data_raw[, pr_cols - 1])), na.rm = T),
  															#max(as.vector(as.matrix(data_raw[, pr_cols - 1])), na.rm = T))
  	# save the 3 structs with proper names for later
  	assign(paste("price", data_name[j], "_raw", sep = ""), data_raw)
  	assign(paste("price", data_name[j], "_val", sep = ""), data_val)
  	assign(paste("price", data_name[j], "_mat", sep = ""), data_mat)
  }
  
  # save
  my_env <- objects()
  save(list = my_env[as.vector(sapply(data_name, function(x) grep(x, my_env)))],
  		 file = "./RTY_LETF_Options_20160415_Prices.RData")
  
  #
  # OPTIONS IVOL
  #
  
  # execute loop 8 times (4 ETFs x call/put)
  data_name <- c("IWMC", "IWMP", "RWMC", "RWMP", "TNAC", "TNAP", "TZAC", "TZAP")
  for(j in 1:length(data_name)) {
  	# set first 2 cols as text because of bad Bloomberg output
  	col_types <- rep(c("text", "text"), 401)
  	# read in raw prices/returns and reference date sequence
  	data_raw <- data.frame(read_excel("./RTY_LETF_Options_20160415_IVOL_Formulas.xlsx",
  																		sheet = j, col_types = col_types))
  	# revise column names and remove columns without any valid prices (strikes without trades)
  	colnames(data_raw)[seq(2, 802, 2) - 1] <- rep("DATE", 401)
  	na_cols <- is.na(data_raw[1, 1:ncol(data_raw)])
  	keepers <- which(na_cols != T & colnames(data_raw) != "DATE")
  	keepers <- as.vector(sapply(1:length(keepers), function(x) c(keepers[x] - 1, keepers[x])))
  	data_raw <- data_raw[, keepers]
  	# convert raw data to dates and numerics as a list with all na's removed
  	pr_cols <- seq(2, ncol(data_raw), 2)
  	for(i in c(pr_cols - 1)) {
  		data_raw[, i] <- as.Date(as.numeric(data_raw[, i]), origin = "1899-12-30")
  		data_raw[, i + 1] <- as.numeric(data_raw[, i + 1])
  	}
  	data_val <- do_findatalist(data_raw, missing = "Remove", date.format = "ymd")
  	# also create a clean, date-aligned matrix/df
  	#  20150824 is the earliest trade date across all 8 option groups
  	data_mat <- do_findataframe(data_val, 20150824, 20160415)
  															#min(as.vector(as.matrix(data_raw[, pr_cols - 1])), na.rm = T),
  															#max(as.vector(as.matrix(data_raw[, pr_cols - 1])), na.rm = T))
  	# save the 3 structs with proper names for later
  	assign(paste("ivol_", data_name[j], "_raw", sep = ""), data_raw)
  	assign(paste("ivol_", data_name[j], "_val", sep = ""), data_val)
  	assign(paste("ivol_", data_name[j], "_mat", sep = ""), data_mat)
  }
  
  # save
  my_env <- objects()
  my_env <- my_env[grep("ivol_", my_env)]
  save(list = my_env[as.vector(sapply(data_name, function(x) grep(x, my_env)))],
  		 file = "./RTY_LETF_Options_20160415_IVOL.RData")
  
}

