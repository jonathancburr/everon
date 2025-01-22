### parameters
tz = 'Europe/London'
bins = 96
max.days = 45
max.date = Sys.Date()-1
minseglen.periodic = 20
minseglen.global.days = 7
pen.global.mult=1
max.periodic.cpts = 4
wt.confidence = .99
nightat='03:00'
plotdays=45

### credentials
config.folder <- 'connection_strings/'
live <- "config_live.txt"
live <- paste(config.folder, live, sep = '')
routinecon <- "config_routine.txt"
routinecon <- paste(config.folder, routinecon, sep = '')
credentials <- read.csv(paste0(config.folder,'credentials.txt'))

sitename <- 6072446308
to <- as.integer(Sys.time())
from <- as.integer(to - lubridate::days(50))

### get activity messages
pquery <- paste0("SELECT h.sitename,datecreated,short_description,sensor_name,macsidid,activity_tracking,icon FROM messages m
  JOIN messagetext USING (textid)
  LEFT JOIN device_howzsite_mappings dhm on m.macsidid = dhm.mac_sid_id
  JOIN howzsites h ON h.hid = m.hid
  LEFT JOIN howzsite_group_mappings hgm ON h.hid = hgm.hid
  LEFT JOIN groups g ON g.group_id = hgm.group_id
  WHERE activity_tracking AND sitename = ",sitename,
  "AND datecreated >=",from," AND datecreated <=",to) 

activity <-
  howzfunc::hsql(live,pquery)

PG <-
activity |>
PGCircRoutine(minseglen.periodic = 4,max.periodic.cpts = 20,minseglen.global.days = 7,max.days=45,penalty.periodic=5)
PG$plot
PG$routines

