library(PEcAn.all)

settings <- read.settings("/fs/data2/output/PEcAn_1000009937/pecan.CHECKED.xml")

start.year = year(settings$run$start.date)
end.year = year(settings$run$end.date)


out <- read.output(runid = 1002444116, outdir = "/fs/data2/output/PEcAn_1000009937/out/1002444116/", start.year = start.year, end.year = end.year, variables = c("GPP", "LAI", "AGB", "NEE", "RECO"), dataframe = TRUE)

library(dygraphs)
library(xts)
library(tidyverse)
library(lubridate)

out_xts =xts(x = out$NEE, order.by = out$posix)

par(mfrow = c(3,1))
dygraph(out_xts) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)


nc <- nc_open("/fs/data2/output/PEcAn_1000009937/out/1002444116/2004.nc")
nc.get.variable.list(nc)

grid.arrange(nrow = 6,
             ggplot(out_9937) + geom_line(aes(x=posix, y=Tair)) + ggtitle("Tair"),
             ggplot(out_9937) + geom_line(aes(x=posix, y=SWdown)) + ggtitle("SWdown"),
             ggplot(out_9937) + geom_line(aes(x=posix, y=LWdown)) + ggtitle("LWdown"),
             ggplot(out_9937) + geom_line(aes(x=posix, y=Rainf)) + ggtitle("Rainf"),
             ggplot(out_9937) + geom_line(aes(x=posix, y=LAI)) + ggtitle("LAI"),
             ggplot(out_9937) + geom_line(aes(x=posix, y=NPP)) + ggtitle("NPP")
)

grid.arrange(nrow = 6,
             ggplot(out_9940) + geom_line(aes(x=posix, y=Tair)) + ggtitle("Tair"),
             ggplot(out_9940) + geom_line(aes(x=posix, y=SWdown)) + ggtitle("SWdown"),
             ggplot(out_9940) + geom_line(aes(x=posix, y=LWdown)) + ggtitle("LWdown"),
             ggplot(out_9940) + geom_line(aes(x=posix, y=Rainf)) + ggtitle("Rainf"),
             ggplot(out_9940) + geom_line(aes(x=posix, y=LAI)) + ggtitle("LAI"),
             ggplot(out_9940) + geom_line(aes(x=posix, y=NPP)) + ggtitle("NPP")
)



settings_9944 <- read.settings("/fs/data2/output/PEcAn_1000009944/pecan.CHECKED.xml")

start.year_9944 = year(settings_9944$run$start.date)
end.year_9944 = year(settings_9944$run$end.date)


out_9944 <- read.output(runid = 1002445182, 
                        outdir = "/fs/data2/output/PEcAn_1000009944/out/1002445182/", 
                        start.year = start.year_9944, end.year = end.year_9944, 
                        variables = c("AbvGrndWood", "GPP", "LAI", "NPP", "NEE", 
                                      "Rainf", "Tair", "SWdown", "LWdown"), 
                        dataframe = TRUE)

grid.arrange(nrow = 6,
             ggplot(out_9944) + geom_line(aes(x=posix, y=Tair)) + ggtitle("Tair"),
             ggplot(out_9944) + geom_line(aes(x=posix, y=SWdown)) + ggtitle("SWdown"),
             ggplot(out_9944) + geom_line(aes(x=posix, y=LWdown)) + ggtitle("LWdown"),
             ggplot(out_9944) + geom_line(aes(x=posix, y=Rainf)) + ggtitle("Rainf"),
             ggplot(out_9944) + geom_line(aes(x=posix, y=LAI)) + ggtitle("LAI"),
             ggplot(out_9944) + geom_line(aes(x=posix, y=NPP)) + ggtitle("NPP")
)


nc <- nc_open("/fs/data2/output/PEcAn_1000009944/out/1002445182/2004.nc")
nc.get.variable.list(nc)

c(GPP, NPP, TotalResp, LAI, Evap, SoilMoist, Qh, Qle, SoilWet, TVeg)
