"/fs/data2/output/PEcAn_1000010014/pecan.xml"

pecan.xml.path <- "/fs/data3/ecowdery/ED_Tropics/pecan.BCI_IC.xml"
pecan.xml.path <- "/fs/data2/output/PEcAn_1000010014/pecan.xml"
pecan.xml.path <- "/fs/data2/output/PEcAn_1000010029/pecan.xml"
pecan.xml.path <- "/fs/data2/output/PEcAn_1000010050/pecan.xml"

pecan.xml.path <- "/fs/data3/ecowdery/ED_Tropics/pecan.BCI_new.xml"

pecan.xml.path <- "/fs/data2/output//PEcAn_1000010231/pecan.xml" # Was this a good run? I wonderrrrrrrr?????/

pecan.xml.path <- "/fs/data2/output//PEcAn_1000010332/pecan.xml"

# file.edit(pecan.xml.path)
# file.edit(file.path(dirname(pecan.xml.path),"STATUS"))

settings2 <- PEcAn.settings::read.settings("/fs/data2/output//PEcAn_1000010231/pecan.xml")
settings <- PEcAn.settings::read.settings(pecan.xml.path)

settings$pfts$pft$ed2_pft_number <- 3
settings$pfts$pft$constants$b1Bl_large <- 0.0132
settings$pfts$pft$constants$leaf_turnover_rate <- 0.33333
settings$pfts$pft$constants$root_turnover_rate <- 0.33333
settings$pfts$pft$constants$num <- NULL

# Probably check if there is a tunnel ... ? I dunno how to do that right now ...

remote_host <- settings$host$name
user <- settings$host$user
tunnel_dir = dirname(settings$host$tunnel)

PEcAn.remote::open_tunnel(remote_host,
            user, password = NULL,
            tunnel_dir,
            wait.time = 15,
            tunnel_script = 'web/sshtunnel.sh')

# file.edit("/fs/data3/ecowdery/ED_Tropics/workflow.BCI.R")
sprintf("Rscript /fs/data3/ecowdery/ED_Tropics/workflow.BCI.R %s", pecan.xml.path)

##################################################

settings <- read.settings("/fs/data2/output/PEcAn_1000009969/pecan.xml")

wood_psi50 <-  -418.3274

wood_Kexp = 54.4 *  (-wood_psi50) ** -1.17 / 100


slope = - wood_Kexp / (4 * wood_psi50)

4*36*(273)^-.17 / 100

4*36*(295.99) / 100

PD.ALL %>% filter(elasticities == 0)
file.edit("/fs/data2/output/PEcAn_1000010206/pecan.xml")
