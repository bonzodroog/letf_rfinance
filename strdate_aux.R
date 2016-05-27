

require(lubridate)
options(stringsAsFactors = F)


#
# auxiliary and support functions
#


# input vector of strings and list of pairings c("out_1" = "in_1", ...)
# output vector of strings
repply <- function(x, sublist)
{
  for (i in 1:length(names(sublist)))
    x <- replace(x, which(x == names(sublist)[i]), sublist[[i]])
  x
}

# Starting 2007, NYSE Market Holidays,
# consider using http://www.tradingtheodds.com/nyse-full-day-closings/
holidays = c("2006-01-02", "2006-01-16", "2006-02-20", "2006-04-14",               #2006
             "2006-05-29", "2006-07-04", "2006-09-04", "2006-11-23", "2006-12-25",
             "2007-01-01", "2007-01-02", "2007-01-15", "2007-02-19", "2007-04-06", #2007
             "2007-05-28", "2007-07-04", "2007-09-03", "2007-11-22", "2007-12-25",
             "2008-01-01", "2008-01-21", "2008-02-18", "2008-03-21",               #2008
             "2008-05-26", "2008-07-04", "2008-09-01", "2008-11-27", "2008-12-25",
             "2009-01-01", "2009-01-19", "2009-02-16", "2009-04-10",               #2009
             "2009-05-25", "2009-07-03", "2009-09-07", "2009-11-26", "2009-12-25",
             "2010-01-01", "2010-01-18", "2010-02-15", "2010-04-02",               #2010
             "2010-05-31", "2010-07-05", "2010-09-06", "2010-11-25", "2010-12-24",
                           "2011-01-17", "2011-02-21", "2011-04-22",               #2011
             "2011-05-30", "2011-07-04", "2011-09-05", "2011-11-24", "2011-12-26",
             "2012-01-02", "2012-01-16", "2012-02-20", "2012-04-06",               #2012
             "2012-05-28", "2012-07-04", "2012-09-03", "2012-11-22", "2012-12-25",
             "2013-01-01", "2013-01-21", "2013-02-18", "2013-03-29",               #2013
             "2013-05-27", "2013-07-04", "2013-09-02", "2013-11-28", "2013-12-25",
             "2014-01-01", "2014-01-20", "2014-02-17", "2014-04-18",               #2014
             "2014-05-26", "2014-07-04", "2014-09-01", "2014-11-27", "2014-12-25",
             "2015-01-01", "2015-01-19", "2015-02-16", "2015-04-03",               #2015
             "2015-05-25", "2015-07-03", "2015-09-07", "2015-11-26", "2015-12-25",
             "2016-01-01", "2016-01-18", "2016-02-15", "2016-03-25",               #2016
             "2016-05-30", "2016-07-04", "2016-09-05", "2016-11-24", "2016-12-26")

# input Date as any ymd format
# output Date string as "YYYYMMDD"
#   will not work for dates before minimum holiday
busdate <- function(myDate)
{
  myDays <- seq(as.Date(ymd(min(holidays))), as.Date(ymd(myDate)), "days")
  hdday <- paste(myDays) %in% c(holidays)
  ssday <- weekdays(as.Date(myDays)) %in% c("Saturday", "Sunday")
  format(myDays[max(which((hdday | ssday) == F))], "%Y%m%d")
}

# input seq(as.Date("YYYY-MM-DD"), as.Date("YYYY-MM-DD"), "days")
# output integer
#  same begin and end date ==> 1 business day (if date is bus day)
#  same begin and end date ==> 0 business day (if date is not bus day)
busdays <- function(myDays, startdate = "", enddate = "")
{
  if (startdate != "" & enddate != "")
    myDays <- seq(as.Date(ymd(startdate)), as.Date(ymd(enddate)), "days")
  hddays <- paste(myDays) %in% c(holidays)
  ssdays <- weekdays(myDays) %in% c("Saturday", "Sunday")
  length(myDays) - sum(hddays) - sum(ssdays)
}

# input Date as any ymd format
# output Date string as "YYYYMMDD"
#   will not work for dates after maximum holiday
bdays_fwd <- function(myDate, offset = 0)
{
  bus_days <- seq(as.Date(ymd(myDate)), as.Date(ymd(max(holidays))), "days")
  hddays <- paste(bus_days) %in% c(holidays)
  ssdays <- weekdays(bus_days) %in% c("Saturday", "Sunday")
  theday <- paste(bus_days) == paste(ymd(myDate))
  if ((hddays[which(theday)] == T | ssdays[which(theday)] == T) & offset == 0)
    offset <- 1
  bus_days <- bus_days[((hddays | ssdays) == F) | theday == T]
  format(bus_days[which(paste(bus_days) == paste(ymd(myDate))) + offset], "%Y%m%d")
}
vector_fwd <- Vectorize(bdays_fwd)

# input filename of csv data where odd numbered columns are dates,
#                                  even numbered columns are named fin data
# output is clean list
do_findatalist <- function(raw_df, missing = "", date.format = "mdy", multiple = 1.0)
{
  final_list <- list()
  for (i in 1:(dim(raw_df)[2] / 2))
  {
    final_list[[i]] <- raw_df[, 2*i] * multiple
    names(final_list)[i] <- colnames(raw_df)[2*i]
    if (date.format == "ymd")
      names(final_list[[i]]) <- as.integer(format(ymd(raw_df[, 2*i - 1]), "%Y%m%d"))
    else
      names(final_list[[i]]) <- as.integer(format(mdy(raw_df[, 2*i - 1]), "%Y%m%d"))
    
    #if missing == "", do nothing, all NAs will be left included in the child vectors 
    if (missing == "Returns")
    {
      final_list[[i]] <- ifelse(is.na(final_list[[i]]) == T, 0.0, final_list[[i]])
    }
    if (missing == "Prices")
    {
      # fill in proper handling routine for missing price data
    }
    if (missing == "Remove")
    {
      final_list[[i]] <- final_list[[i]][is.na(final_list[[i]]) == F]
    }
  }  
  final_list
}

# input date-name vector of fin data, and p, the number of days in your period
# output is clean mat with p columns and rows equal to length of vector - p + 1
do_findatamatroll <- function(returns_vector, p)
{
  final_mat <- t(sapply(p:length(returns_vector), function(x) returns_vector[(x - p + 1):x]))
  if (p == 1) final_mat <- t(final_mat)
  rownames(final_mat) <- names(returns_vector)[p:length(returns_vector)]
  colnames(final_mat) <- c(1:p)
  final_mat
}

# input clean list produced by do_findatalist
#   and start, end date strings YYYYMMDD for joining
# output is clean-joined data frame
do_findataframe <- function(clean_list, startdate, enddate, missing = "")
{
  if ("LETF" %in% names(clean_list))
  {
    startdate = min(names(clean_list$LETF))
    enddate = max(names(clean_list$LETF))
  }
  rowdates <- seq(as.Date(paste(ymd(startdate))), as.Date(paste(ymd(enddate))), "days")
  rowdates <- rowdates[(weekdays(rowdates) %in% c("Saturday", "Sunday")) == F &
                       (paste(rowdates) %in% holidays) == F]
  final_df <- data.frame(Date = format(rowdates, "%Y%m%d"), stringsAsFactors = F)
  for (i in 1:length(names(clean_list)))
  {
    temp_df <- data.frame(names(clean_list[[i]]), clean_list[[i]], stringsAsFactors = F)
    colnames(temp_df) <- c("Date", names(clean_list)[i])
    final_df <- join(final_df, temp_df, by = "Date")
    
    #if missing == "", do nothing, all NAs induced by date join will remain 
    if (missing == "Returns")
    {
      final_df[,i+1] <- ifelse(is.na(final_df[,i+1]) == T, 0.0, final_df[,i+1])
    }
    if (missing == "Prices")
    {
      # fill in proper handling routine for missing price data
    }
    #remove NAs.  final_df will only have rows where all input vectors had a value (intersection)
    if (missing == "Remove")
    {
      final_df <- final_df[is.na(final_df[,i+1]) == F, ]
    }
  }
  rownames(final_df) <- final_df$Date
  final_df[, -1]
}

# LETF helper functions
#
eqcurve <- function(x)
{
  sapply(1:length(x), function(i) prod(1 + x[1:i]))
}
gean <- function(x)
{
  exp(mean(log(1 + x))) - 1
}
psd <- function(x)
{
  sum((log(1 + x) - mean(log(1 + x))) ^ 2) ^ (1/2)
}
comp <- function(x)
{
  prod(1 + x) - 1
}
rmc <- function(x, b)
{
  ((1 + b*gean(x)) ^ length(x)) - 1
}
smc <- function(x, b, y = NULL)
{
  if (is.null(y) == T)
    (1 + rmc(x, b)) / (1 + comp(b*x)) - 1
  else
    (1 + rmc(x, b)) / (1 + comp(y)) - 1
}
