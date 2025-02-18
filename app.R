library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Messing with NGC"),

    sidebarLayout(
        sidebarPanel(
          sliderInput("daysahead","How many days after today?",min=0,max=365,value=0),
          sliderInput("localtime","Hours before/after midnight:",min=-12, max=12, value=-3),
          sliderInput("minalt","Minimum Altitude (deg): ",min=0,max=90,value=30),
          checkboxInput("usemag","Rank with Magnitude?", value=TRUE),
          checkboxInput("usesurf","Rank with Surface Brightness?", value=FALSE),
          checkboxInput("usesize","Rank with Apparent Size?", value=FALSE),
          checkboxInput("usealt","Rank with Altitude?", value=FALSE),
          checkboxInput("usen","Rank with Number of Neighbors ...", value=FALSE),
          sliderInput("ndeg","Neighbors within (deg)",min=0,max=10,value=2.5,step=0.5),
          checkboxGroupInput("types","Which types?",
                             # choices=c("Ellip Gal",  "Glob Clust", "Neb",    "NGC Gal",    "Open Clust", "Plan Neb","Spiral Gal"),
                             choices=c("Spiral Gal", "Ellip Gal",    "NGC Gal",  "Neb", "Plan Neb","Glob Clust",     "Open Clust"),
                             selected=c("Ellip Gal",  "Glob Clust", "Neb",    "NGC Gal",    "Open Clust", "Plan Neb","Spiral Gal")),
          # sliderInput("thismany","How many to highlight?",min=1,max=20,value=5),
          sliderInput("whichhighlight","Which to highlight",min=1,max=30,value=1),
          sliderInput("lat_deg","Latitude (deg): ",min=-90,max=90,value=61.2176),
          sliderInput("long_deg","Longitude (deg): ",min=-180,max=180,value=-149.9),
          sliderInput("timezone","Time Zone: ",min=-12,max=12,value=-9),
          checkboxInput("DST", "Daylight Savings Time", value=FALSE)
        ),

        mainPanel(
          plotOutput("mainPlot", height="400px", width="1200px"),
          # tableOutput("theothertable"),
          plotOutput("subPlot", height="320px", width="600px"),
           tableOutput("thetable")
        )
    )
)

# Define server logic 
server <- function(input, output) {
  
  
  ngc_raw <- data.frame(
    stringsAsFactors = FALSE,
    check.names = FALSE,
    Season = c("Autumn","Autumn","Autumn",
               "Autumn","Autumn","Autumn","Autumn","Autumn","Autumn",
               "Autumn","Autumn","Autumn","Autumn","Autumn",
               "Autumn","Autumn","Autumn","Autumn","Autumn","Autumn",
               "Autumn","Autumn","Winter","Winter","Winter","Winter",
               "Winter","Winter","Winter","Winter","Winter",
               "Winter","Winter","Winter","Winter","Winter","Winter",
               "Winter","Spring","Spring","Spring","Spring","Spring",
               "Spring","Spring","Spring","Spring","Spring",
               "Spring","Spring","Spring","Spring","Spring","Spring",
               "Spring","Spring","Spring","Spring","Spring","Spring",
               "Spring","Spring","Spring","Spring","Spring","Spring",
               "Spring","Spring","Spring","Spring","Spring",
               "Spring","Spring","Spring","Spring","Spring","Spring",
               "Spring","Spring","Spring","Spring","Spring","Spring",
               "Spring","Spring","Spring","Spring","Spring",
               "Spring","Summer","Summer","Summer","Summer","Summer",
               "Summer","Summer","Summer","Summer","Summer","Summer",
               "Summer","Summer","Summer","Summer","Summer",
               "Summer","Summer","Summer","Summer","Summer","Summer"),
    Num = c("1","2","3","4","5","6",
            "7","8","9","10","11","12","13","14","15","16",
            "17","18","19","20","21","22","23","24","25","26",
            "27","28","29","30","31","32","33","34","35",
            "36","37","38","39","40","41","42","43","44","45",
            "46","47","48","49","50","51","52","53","54",
            "55","56","57","58","59","60","61","62","63","64",
            "65","66","67","68","69","70","71","72","73",
            "74","75","76","77","78","79","80","81","82","83",
            "84","85","86","87","88","89","90","91","92",
            "93","94","95","96","97","98","99a","99b","100",
            "101","102","103","104","105","106","107","108",
            "109","110"),
    Primary.ID = c("NGC 7009","NGC 7293",
                   "NGC 7331","NGC 7635","NGC 7789","NGC 185","NGC 281",
                   "NGC 457","NGC 663","IC 289","NGC 7662","NGC 891",
                   "NGC 253","NGC 772","NGC 246","NGC 936","NGC 884","NGC 1023",
                   "NGC 1491","NGC 1501","NGC 1232","NGC 1535",
                   "NGC 1514","NGC 1931","NGC 1788","NGC 1973","NGC 2022",
                   "NGC 2024","NGC 2194","NGC 2371","NGC 2392","NGC 2237",
                   "NGC 2261","NGC 2359","NGC 2440","NGC 2539","NGC 2403",
                   "NGC 2655","NGC 2683","NGC 2841","NGC 3079",
                   "NGC 3184","NGC 3877","NGC 3941","NGC 4026","NGC 4088",
                   "NGC 4157","NGC 4605","NGC 3115","NGC 3242","NGC 3003",
                   "NGC 3344","NGC 3432","NGC 2903","NGC 3384","NGC 3521",
                   "NGC 3607","NGC 3628","NGC 4111","NGC 4214",
                   "NGC 4244","NGC 4449","NGC 4490","NGC 4631","NGC 4656",
                   "NGC 5005","NGC 5033","NGC 4274","NGC 4414","NGC 4494",
                   "NGC 4559","NGC 4565","NGC 4725","NGC 4038","NGC 4361",
                   "NGC 4216","NGC 4388","NGC 4438","NGC 4517",
                   "NGC 4526","NGC 4535","NGC 4567","NGC 4699","NGC 4762",
                   "NGC 5746","NGC 5466","NGC 5907","NGC 6503","NGC 6543",
                   "NGC 6210","NGC 6369","NGC 6572","NGC 6633","NGC 6712",
                   "NGC 6781","NGC 6819","NGC 6826","NGC 6888",
                   "NGC 6992","NGC 6960","NGC 7000","NGC 7027","NGC 6445",
                   "NGC 6520","NGC 6818","NGC 6802","NGC 6940","NGC 6939",
                   "NGC 6946","NGC 7129","NGC 40"),
    Alternate.ID = c("Saturn Nebula","Helix",
                     "Deer Lick Group","Bubble Nebula","OCL 269","MCG 8-2-10",
                     "PacMan Nebula","ET Cluster","Collinder 20",
                     "PN G138.8+02.8","Blue Snowball","Silver Needle",
                     "Sculptor Galaxy","Arp 78","Skull Nebula","MCG 0-7-17",
                     "Double Cluster with 869","Arp 135","Sh 2-206","PN G144.5+06.5",
                     "Arp 41","Cleopatra's Eye","Crystal Ball Nebula",NA,
                     "vdB 33","with NGC 1975 & 1977","PN G196.6-10.9",
                     "Flame Nebula","Collinder 87","Gemini Nebula; 2372?",
                     "Clown Face","Rosette","Hubble's Variable Nebula",
                     "Thor's Helmet","PN G234.8+02.4","Collinder 176","MCG 11-10-7",
                     "Arp 225","UFO Galaxy","MCG 9-16-5","MCG 9-17-10",
                     "MCG 7-21-37","MCG 8-22-2","MCG 6-26-51","MCG 9-20-52",
                     "Arp 18","MCG 9-20-106","MCG 10-18-74","Spindle Galaxy",
                     "Ghost of Jupiter","MCG 6-22-13","MCG 4-25-46","Arp 206",
                     NA,"MCG 2-28-12","MCG 0-28-30","MCG 3-29-20",
                     "Arp 317","MCG 7-25-26","MCG 6-27-42","Silver Needle",
                     "MCG 7-26-9","Cocoon Galaxy","Whale Galaxy",
                     "Hockey Stick or Crowbar","MCG 6-29-52","MCG 6-29-62","MCG 5-29-60",
                     "MCG 5-29-85","MCG 4-30-2","MCG 5-30-30",
                     "Needle Galaxy","MCG 4-30-22","The Antennae with 4039",
                     "PN G294.1+43.6","MCG 2-31-72","MCG 2-32-41","The Eyes with 4435",
                     "often mislabelled NGC 4437","MCG 1-32-100",
                     "Lost Galaxy","Siamese Twins with 4568","MCG -1-33-13",
                     "MCG 2-33-33","MCG 0-38-5",NA,"Splinter Galaxy","MCG 12-17-9",
                     "Cat's Eye Nebula","Turtle Nebula",
                     "Little Ghost Nebula","Blue Racquetball","Collinder 380",NA,
                     "PN G041.8-02.9","Collinder 403","Blinking Planetary",
                     "Crescent Nebula","Veil Nebula (East)","Veil Nebula (West)",
                     "North American Nebula","Magic Carpet","He 2-290",
                     "Collinder 361","Little Gem","Collinder 400","Collinder 424",
                     "Collinder 423","Fireworks Galaxy; Arp 29",
                     "Small Cluster Nebula","Bowtie Nebula"),
    Con = c("Aqr","Aqr","Peg","Cas",
            "Cas","Cas","Cas","Cas","Cas","Cas","And","And",
            "Scl","Ari","Cet","Cet","Per","Per","Per","Cam","Eri",
            "Eri","Tau","Aur","Ori","Ori","Ori","Ori","Ori",
            "Gem","Gem","Mon","Mon","CMa","Pup","Pup","Cam",
            "Cam","Lyn","UMa","UMa","UMa","UMa","UMa","UMa",
            "UMa","UMa","UMa","Sex","Hya","LMi","LMi","LMi",
            "Leo","Leo","Leo","Leo","Leo","CVn","CVn","CVn",
            "CVn","CVn","CVn","CVn","CVn","CVn","Com","Com",
            "Com","Com","Com","Com","Crv","Crv","Vir","Vir","Vir",
            "Vir","Vir","Vir","Vir","Vir","Vir","Vir","Boo",
            "Dra","Dra","Dra","Her","Oph","Oph","Oph","Sct",
            "Aql","Cyg","Cyg","Cyg","Cyg","Cyg","Cyg","Cyg",
            "Sgr","Sgr","Sgr","Vul","Vul","Cep","Cyg","Cep",
            "Cep"),
    Cls = c("PN","PN","Gal","EN","OC",
            "Gal","EN","OC","OC","PN","PN","Gal","Gal","Gal",
            "PN","Gal","OC","Gal","EN","PN","Gal","PN","PN",
            "E/RN","RN","E/RN","PN","EN","OC","PN","PN","EN",
            "E/RN","EN","PN","OC","Gal","Gal","Gal","Gal",
            "Gal","Gal","Gal","Gal","Gal","Gal","Gal","Gal",
            "Gal","PN","Gal","Gal","Gal","Gal","Gal","Gal",
            "Gal","Gal","Gal","Gal","Gal","Gal","Gal","Gal","Gal",
            "Gal","Gal","Gal","Gal","Gal","Gal","Gal","Gal",
            "Gal","PN","Gal","Gal","Gal","Gal","Gal","Gal",
            "Gal","Gal","Gal","Gal","GC","Gal","Gal","PN","PN",
            "PN","PN","OC","GC","PN","OC","PN","EN","SNR",
            "SNR","EN","PN","PN","OC","PN","OC","OC","OC",
            "Gal","RN","PN"),
    RA.2000 = c("21h04m10.9s","22h29m38.5s",
                "22h37m04.3s","23h20m43.0s","23h57m24.0s","00h38m57.9s",
                "00h53m00.0s","01h19m35.0s","01h46m09.0s",
                "03h10m19.3s","23h25m54.0s","02h22m33.6s","00h47m33.1s",
                "01h59m20.0s","00h47m03.3s","02h27m37.5s","02h22m18.0s",
                "02h40m24.0s","04h03m20.6s","04h06m59.2s","03h09m45.3s",
                "04h14m15.8s","04h09m17.0s","05h31m27.7s","05h06m54.0s",
                "05h35m06.0s","05h42m06.2s","05h41m42.0s",
                "06h13m45.0s","07h25m34.7s","07h29m10.8s","06h32m02.0s",
                "06h39m10.0s","07h18m30.0s","07h41m55.4s","08h10m37.0s",
                "07h36m50.7s","08h55m38.9s","08h52m41.2s","09h22m02.5s",
                "10h01m58.5s","10h18m17.0s","11h46m07.8s","11h52m55.3s",
                "11h59m25.2s","12h05m33.7s","12h11m04.9s",
                "12h39m59.4s","10h05m14.0s","10h24m46.1s","09h48m35.6s",
                "10h43m31.2s","10h52m30.9s","09h32m09.7s","10h48m16.9s",
                "11h05m48.8s","11h16m54.5s","11h20m16.9s","12h07m03.2s",
                "12h15m39.1s","12h17m29.5s","12h28m10.9s","12h30m36.4s",
                "12h42m07.7s","12h43m57.6s","13h10m56.3s",
                "13h13m27.5s","12h19m50.5s","12h26m27.1s","12h31m24.0s",
                "12h35m57.7s","12h36m20.8s","12h50m26.6s","12h01m52.9s",
                "12h24m30.8s","12h15m54.3s","12h25m46.8s","12h27m45.9s",
                "12h32m45.6s","12h34m03.0s","12h34m20.3s","12h36m32.7s",
                "12h49m02.2s","12h52m55.9s","14h44m56.0s",
                "14h05m27.0s","15h15m53.7s","17h49m26.5s","17h58m33.4s",
                "16h44m29.5s","17h29m20.4s","18h12m06.4s","18h27m15.0s",
                "18h53m04.0s","19h18m28.1s","19h41m18.0s","19h44m48.2s",
                "20h12m12.0s","20h56m24.0s","20h45m42.0s","20h58m30.0s",
                "21h07m01.7s","17h49m15.1s","18h03m24.0s",
                "19h43m57.8s","19h30m35.0s","20h34m26.0s","20h31m30.0s",
                "20h34m52.7s","21h43m00.0s","00h13m01.0s"),
    Decl.2000 = c("-11°21'48\"","-20°50'14\"",
                  "+34°24'59\"","+61°12'31\"","+56°42'30\"","+48°20'15\"",
                  "+56°37'00\"","+58°17'12\"","+61°14'06\"",
                  "+61°19'01\"","+42°32'05\"","+42°20'46\"","-25°17'20\"",
                  "+19°00'28\"","-11°52'19\"","-01°09'20\"","+57°08'12\"",
                  "+39°03'48\"","+51°19'17\"","+60°55'14\"","-20°34'51\"",
                  "-12°44'22\"","+30°46'33\"","+34°14'55\"","-03°20'00\"",
                  "-04°44'00\"","+09°05'11\"","-01°51'00\"",
                  "+12°48'24\"","+29°29'26\"","+20°54'42\"","+04°59'10\"",
                  "+08°45'00\"","-13°14'00\"","-18°12'31\"","-12°49'06\"",
                  "+65°36'09\"","+78°13'25\"","+33°25'16\"","+50°58'35\"",
                  "+55°40'50\"","+41°25'28\"","+47°29'41\"","+36°59'11\"",
                  "+50°57'42\"","+50°32'22\"","+50°29'07\"",
                  "+61°36'33\"","-07°43'07\"","-18°38'32\"","+33°25'19\"",
                  "+24°55'20\"","+36°37'09\"","+21°30'03\"","+12°37'43\"",
                  "-00°02'06\"","+18°03'11\"","+13°35'28\"","+43°03'55\"",
                  "+36°19'41\"","+37°48'25\"","+44°05'33\"","+41°38'37\"",
                  "+32°32'34\"","+32°10'13\"","+37°03'30\"",
                  "+36°35'37\"","+29°36'51\"","+31°13'22\"","+25°46'30\"",
                  "+27°57'35\"","+25°59'15\"","+25°30'03\"","-18°52'03\"",
                  "-18°47'06\"","+13°09'00\"","+12°39'44\"","+13°00'32\"",
                  "+00°06'54\"","+07°41'57\"","+08°11'52\"","+11°15'28\"",
                  "-08°39'52\"","+11°13'50\"","+01°57'17\"",
                  "+28°32'06\"","+56°19'44\"","+70°08'40\"","+66°37'59\"",
                  "+23°48'00\"","-23°45'34\"","+06°51'12\"","+06°30'30\"",
                  "-08°42'18\"","+06°32'19\"","+40°11'12\"","+50°31'30\"",
                  "+38°19'00\"","+31°43'00\"","+30°43'00\"","+44°22'28\"",
                  "+42°14'10\"","-20°00'34\"","-27°53'18\"",
                  "-14°09'12\"","+20°15'42\"","+28°17'00\"","+60°39'42\"",
                  "+60°09'14\"","+66°07'00\"","+72°31'19\""),
    Mag = c(8.3,6.3,10.2,11,7.5,10.2,
            7.4,5.1,6.4,12,8.6,10.9,7.9,10.6,10.4,11.2,4.4,
            9.6,NA,12,10.6,9.4,10.8,14,NA,7,11.7,NA,10,
            11.2,8.6,5.5,NA,NA,11.5,8,8.8,11,10,10,11.4,
            10.4,11.8,11.3,11.7,11.3,12.1,10.8,10,8.6,12.2,
            10.5,11.6,10,10.9,9.9,10.9,10,11.7,10.2,10.4,9.5,
            9.8,9.5,9.7,10.5,10.7,11.3,11,10.7,10.3,10.1,
            9.9,10.9,10.9,11,11.9,10.9,11.1,10.6,10.5,12.1,
            10.4,11.1,11.4,9.2,11.1,10.8,8.3,9.7,11,8,5.6,
            8.1,11.8,9.5,8.8,10,7,7,4,9.6,13,7.6,10,11.7,
            7.2,10.1,9.8,11.5,10.7),
    Size = c("28\"","16.0'","9.1'x 3.4'",
             "16.0'x 9.0'","25.0'","10.7'x 8.9'","30.0'","20.0'",
             "14.0'","45\"","17\"","11.7'x 2.3'","28.2'x 5.5'",
             "5.9'x 3.2'","4.0'","4.5'x 3.4'","18.0'","7.6'x 2.8'",
             "9.0'","56\"","6.3'x 5.5'","20\"","2.0'","1.8'",
             "10.0'x 6.0'","13.1'","28\"","20.0'","9.0'","1.2'",
             "47\"","70.0'x 60.0'","2.2'x 1.5'","10.0'","54\"",
             "9.0'","20.0'x 10.0'","4.5'x 2.8'","8.7'x 2.5'",
             "7.8'x 3.7'","7.8'x 1.3'","7.2'x 7.1'","5.0'x 1.1'",
             "3.5'x 2.3'","4.7'x 0.9'","5.8'x 2.2'","6.6'x 1.2'","5.9'x 2.3'",
             "7.6'x 3.5'","40\"","5.1'x 1.3'","6.6'x 6.3'",
             "6.8'x 1.6'","11.5'x 4.2'","5.4'x 3.2'","9.5'x 4.6'",
             "4.6'x 4.0'","12.3'x 3.3'","2.0'x 0.6'","7.8'x 6.3'",
             "15.1'x 5.9'","5.2'x 3.3'","6.3'x 2.0'","13.2'x 2.4'",
             "8.5'x 1.1'","5.4'x 2.0'","9.5'x 4.3'","5.0'x 2.1'",
             "2.8'x 1.6'","4.4'x 4.2'","10.0'x 4.3'","14.5'x 2.3'",
             "9.5'x 6.6'","4.4'x 2.7'","1.3'","7.9'x 1.9'",
             "5.5'x 1.4'","8.9'x 5.1'","9.8'x 1.5'","7.9'x 2.9'","7.2'x 6.3'",
             "2.9'x 2.2'","4.0'x 3.0'","8.5'x 3.2'","6.8'x 1.1'",
             "9.0'","11.0'x 1.4'","6.5'x 2.1'","22\"","20\"",
             "30\"","15\"","20.0'","9.8'","1.9'","5.0'","27\"",
             "17.0'x 9.0'","60.0'x 7.0'","63.0'x 6.0'","120.0'",
             "18\"","35\"","5.0'","22\"","5.0'","25.0'","10.0'",
             "10.5'x 10.0'","2.7'","1.0'"),
    Rating = c("!!","!!","!!",NA,"!!",NA,
               "!!",NA,NA,NA,"!!","!!","!!",NA,NA,NA,"!!",NA,
               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"!!",
               NA,NA,NA,NA,NA,"!!",NA,NA,"!!",NA,NA,NA,NA,NA,
               NA,NA,NA,NA,"!!",NA,NA,NA,"!!",NA,NA,NA,NA,
               NA,NA,"!!",NA,NA,"!!","!!",NA,NA,NA,NA,NA,NA,
               "!!",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA,NA,"!!",NA,NA,NA,NA,NA,NA,NA,NA,NA,"!!",NA,
               "!!","!!","!!",NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA)
  )
  
  
  # ngc RA
  ngc_RAh <- as.numeric(substr(ngc_raw$RA.2000,1,2))
  ngc_RAm <- as.numeric(substr(ngc_raw$RA.2000,4,5))
  ngc_RAs <- as.numeric(substr(ngc_raw$RA.2000,7,10))
  ngc_raw$RA_deg <- ngc_RAh*360/24 + ngc_RAm*360/24/60 + ngc_RAs*360/24/60/60
  
  # ncg DEC
  ngc_DECd <- as.numeric(substr(ngc_raw$Decl.2000,2,3))
  ngc_DECm <- as.numeric(substr(ngc_raw$Decl.2000,5,6))
  ngc_DECs <- as.numeric(substr(ngc_raw$Decl.2000,8,9))
  ngc_DECp <- ifelse(substr(ngc_raw$Decl.2000,1,1)=="+",1,-1)
  
  ngc_raw$Dec_deg <- ngc_DECp*(ngc_DECd+(ngc_DECm/60)+(ngc_DECs/60/60))
  
  # plot(ngc_raw$RA_deg, ngc_raw$Dec_deg, col=as.numeric(as.factor(ngc_raw$Season)), pch=16)
  
  # ngc size
  ngc_sizelist <- strsplit(ngc_raw$Size, split="x ")
  ngc_size1 <- sapply(ngc_sizelist, "[", 1)
  ngc_size2 <- sapply(ngc_sizelist, "[", 2)
  
  units1 <- substr(ngc_size1, nchar(ngc_size1), nchar(ngc_size1))
  units2 <- substr(ngc_size2, nchar(ngc_size2), nchar(ngc_size2))
  
  ngc_size1q <- as.numeric(substr(ngc_size1, 1, nchar(ngc_size1)-1))
  ngc_size2q <- as.numeric(substr(ngc_size2, 1, nchar(ngc_size2)-1))
  
  ngc_raw$arcmin1 <- ifelse(units1=="'", ngc_size1q, ngc_size1q/60)
  ngc_raw$arcmin2 <- ifelse(is.na(units2), ngc_raw$arcmin1, 
                            ifelse(units2=="'", ngc_size2q, ngc_size2q/60))
  ngc <- ngc_raw
  
  
  mess_raw <- data.frame(
    stringsAsFactors = FALSE,
    check.names = FALSE,
    M = c(NA,"M1", 
          "M2","M3","M4","M5","M6","M7","M8",
          "M9","M10","M11","M12","M13","M14","M15",
          "M16","M17","M18","M19","M20","M21","M22",
          "M23","M24","M25","M26","M27","M28",
          "M29","M30","M31","M32","M33","M34","M35",
          "M36","M37","M38","M39","M40","M41",
          "M42","M43","M44","M45","M46","M47","M48",
          "M49","M50","M51","M52","M53","M54",
          "M55","M56","M57","M58","M59","M60","M61",
          "M62","M63","M64","M65","M66","M67","M68",
          "M69","M70","M71","M72","M73","M74",
          "M75","M76","M77","M78","M79","M80","M81",
          "M82","M83","M84","M85","M86","M87",
          "M88","M89","M90","M91","M92","M93","M94",
          "M95","M96","M97","M98","M99","M100",
          "M101","M102","M103","M104","M105","M106",
          "M107","M108","M109","M110"),
    NGC = c(NA,
            "1952","7089","5272","6121","5904","6405",
            "6475","6523","6333","6254","6705","6218",
            "6205","6402","7078","6611","6618","6613",
            "6273","6514","6531","6656","6494","-",
            "IC4725","6694","6853","6626","6913",
            "7099","224","221","598","1039","2168",
            "1960","2099","1912","7092","Win4","2287",
            "1976","1982","2632","-","2437","2422",
            "2548","4472","2323","5194","7654","5024",
            "6715","6809","6779","6720","4579","4621",
            "4649","4303","6266","5055","4826","3623",
            "3627","2682","4590","6637","6681",
            "6838","6981","6994","628","6864","650",
            "1068","2068","1904","6093","3031","3034",
            "5236","4374","4382","4406","4486","4501",
            "4552","4569","4548","6341","2447","4736",
            "3351","3368","3587","4192","4254",
            "4321","5457","5866","581","4594","3379",
            "4258","6171","3556","3992","205"),
    Type = c(NA,"Sn",
             "Gc","Gc","Gc","Gc","Oc","Oc","Di",
             "Gc","Gc","Oc","Gc","Gc","Gc","Gc","Oc",
             "Di","Oc","Gc","Di","Oc","Gc","Oc","MW",
             "Oc","Oc","Pl","Gc","Oc","Gc","Sp",
             "El","Sp","Oc","Oc","Oc","Oc","Oc","Oc",
             "Ds","Oc","Di","Di","Oc","Oc","Oc","Oc",
             "Oc","El","Oc","Sp","Oc","Gc","Gc",
             "Gc","Gc","Pl","Ba","El","El","Sp","Gc",
             "Sp","Sp","Sp","Sp","Oc","Gc","Gc","Gc",
             "Gc","Gc","As","Sp","Gc","Pl","Sp",
             "Di","Gc","Gc","Sp","Ir","Sp","Ln","Ln",
             "Ln","El","Sp","El","Sp","Ba","Gc","Oc",
             "Sp","Ba","Sp","Pl","Sp","Sp","Sp",
             "Sp","Ln","Oc","Sp","El","Sp","Gc","Sp",
             "Ba","El"),
    Mag. = c(NA,8.4,
             6.5,6.2,5.6,5.6,4.2,3.3,6,7.7,6.6,6.3,
             6.7,5.8,7.6,6.2,6.4,7,7.5,6.8,9,6.5,
             5.1,6.9,4.6,6.5,8,7.4,6.8,7.1,7.2,
             3.4,8.1,5.7,5.5,5.3,6.3,6.2,7.4,4.6,
             8.4,4.6,4,9,3.7,1.6,6,5.2,5.5,8.4,6.3,
             8.4,7.3,7.6,7.6,6.3,8.3,8.8,9.7,9.6,
             8.8,9.7,6.5,8.6,8.5,9.3,8.9,6.1,7.8,
             7.6,7.9,8.2,9.3,9,9.4,8.5,10.1,8.9,8.3,
             7.7,7.3,6.9,8.4,7.6,9.1,9.1,8.9,8.6,
             9.6,9.8,9.5,10.2,6.4,6,8.2,9.7,9.2,
             9.9,10.1,9.9,9.3,7.9,9.9,7.4,8,9.3,8.4,
             7.9,10,9.8,8.5),
    arcmin1 = c(NA,"6x4",
                "12.9","16.2","26.3","17.4","25","80",
                "90x40","9.3","15.1","14","14.5","16.6",
                "11.7","12.3","7","11","9","13.5","28",
                "13","24","27","90","40","15","8.0x5.7",
                "11.2","7","11","178x63","8x6","73x45",
                "35","28","12","24","21","32","0.8",
                "38","85x60","20x15","95","110","27","30",
                "54","9x7.5","16","11x7","13","12.6",
                "9.1","19","7.1","1.4x1.0","5.5x4.5","5x3.5",
                "7x6","6x5.5","14.1","10x6","9.3x5.4",
                "8x1.5","8x2.5","30","12","7.1","7.8",
                "7.2","5.9","2.8","10.2x9.5","6","2.7x1.8",
                "7x6","8x6","8.7","8.9","21x10","9x4",
                "11x10","5","7.1x5.2","7.5x5.5","7","7x4",
                "4","9.5x4.5","5.4x4.4","11.2","22","7x3",
                "4.4x3.3","6x4","3.4x3.3","9.5x3.2",
                "5.4x4.8","7x6","22","5.2x2.3","6","9x4","2",
                "19x8","10","8x1","7x4","17x10"),
    armin2 = c(NA,NA,
               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
               NA,NA,NA,NA,NA),
    `Distance.(ly)` = c(NA,
                        "6300","37900","33900","7200","24500","1600",
                        "800","5200","26700","14400","6000",
                        "16000","25100","29000","33600","7000","5000",
                        "4900","28400","5200","4250","10400",
                        "2150","10000","2000","5000","1250","18600",
                        "4000","26100","3 million","3 million",
                        "3 million","1400","2800","4100","4400",
                        "4200","825","510","2300","1600","1600",
                        "577","380","5400","1600","1500",
                        "60 million","3000","37 million","5000","59700",
                        "88700","17600","32900","2300","60 million",
                        "60 million","60 million","60 million",
                        "22500","37 million","19 million","35 million",
                        "35 million","2700","33300","28000",
                        "29400","12700","55400","2000","35 million",
                        "61300","3400","60 million","1600","42100",
                        "32600","12 million","12 million",
                        "15 million","60 million","60 million","60 million",
                        "60 million","60 million","60 million",
                        "60 million","60 million","26700","3600",
                        "15 million","38 million","38 million","2600",
                        "60 million","60 million","60 million",
                        "27 million","40 million","8500","50 million",
                        "38 million","25 million","20900",
                        "45 million","55 million","3 million"),
    Right_ascension = c(NA,
                        "5h 34.5m","21h 33.5m","13h 42.2m","16h 23.6m",
                        "15h 18.6m","17h 40.1m","17h 53.9m",
                        "18h 03.8m","17h 19.2m","16h 57.1m","18h 51.1m",
                        "16h 47.2m","16h 41.7m","17h 37.6m",
                        "21h 30m","18h 18.8m","18h 20.8m","18h 19.9m",
                        "17h 02.6m","18h 02.6m","18h 04.6m","18h 36.4m",
                        "17h 56.8m","18h 16.9m","18h 31.6m",
                        "18h 45.2m","19h 59.6m","18h 24.5m","20h 23.9m",
                        "21h 40.4m","0h 41.8m","0h 42.8m",
                        "1h 33.9m","2h 42m","6h 08.9m","5h 36.1m",
                        "5h 52.4m","5h 28.7m","21h 32.2m","12h 22.4m",
                        "6h 47m","5h 35.4m","5h 35.6m","8h 40.1m",
                        "3h 47m","7h 41.8m","7h 36.6m","8h 13.8m",
                        "12h 29.8m","7h 03.2m","13h 30m","23h 24.2m",
                        "13h 12.9m","18h 55.1m","19h 40m",
                        "19h 16.6m","18h 53.6m","12h 37.7m","12h 42m",
                        "12h 43.7m","12h 21.9m","17h 01.2m","13h 15.8m",
                        "12h 56.7m","11h 18.9m","11h 20.2m",
                        "8h 50.4m","12h 39.5m","18h 31.4m","18h 43.2m",
                        "19h 53.8m","20h 53.5m","20h 59m","1h 36.7m",
                        "20h 06.1m","1h 42.4m","2h 42.7m",
                        "5h 46.7m","5h 24.5m","16h 17m","9h 55.6m",
                        "9h 55.8m","13h 37m","12h 25.1m","12h 25.5m",
                        "12h 26.2m","12h 30.8m","12h 32.1m","12h 35.7m",
                        "12h 36.8m","12h 35.5m","17h 17.1m",
                        "7h 44.6m","12h 50.9m","10h 44m","10h 46.8m",
                        "11h 14.8m","12h 13.9m","12h 18.9m","12h 23m",
                        "14h 03.2m","15h 06.5m","1h 33.2m",
                        "12h 40m","10h 47.8m","12h 18.9m","16h 32.5m",
                        "11h 11.5m","11h 57.6m","00h 40.4m"),
    Declination = c(NA,
                    "+22° 01′","-00° 49′","+28° 23′","-26° 32′",
                    "+02° 05′","-32° 13′","-34° 49′","-24° 23′",
                    "-18° 31′","-04° 06′","-06° 16′","-01° 57′",
                    "+36° 28′","-03° 15′","+12° 10′",
                    "-13° 47′","-16° 11′","-17° 08′","-26° 16′",
                    "-23° 02′","-22° 30′","-23° 54′","-19° 01′",
                    "-18° 30′","-19° 15′","-09° 24′","+22° 43′",
                    "-24° 52′","+38° 32′","-23° 11′","+41° 16′",
                    "+40° 52′","+30° 39′","+42° 47′","+24° 20′",
                    "+34° 08′","+32° 33′","+35° 50′",
                    "+48° 26′","+58° 05′","-20° 44′","-05° 27′",
                    "-05° 16′","+19° 59′","+24° 07′","-14° 49′",
                    "-14° 30′","-05° 48′","+08° 00′","-08° 20′",
                    "+47° 11′","+61° 35′","+18° 10′","-30° 29′",
                    "-30° 58′","+30° 11′","+33° 02′","+11° 49′",
                    "+11° 39′","+11° 33′","+04° 28′",
                    "-30° 07′","+42° 02′","+21° 41′","+13° 05′",
                    "+12° 59′","+11° 49′","-26° 45′","-32° 21′",
                    "-32° 18′","+18° 47′","-12° 32′","-12° 38′",
                    "+15° 47′","-21° 55′","+51° 34′","+00° 02′",
                    "+00° 03′","-24° 33′","-22° 59′","+69° 04′",
                    "+69° 41′","-29° 52′","+12° 53′",
                    "+18° 12′","+12° 57′","+12° 24′","+14° 26′",
                    "+12° 33′","+13° 10′","+14° 30′","+43° 08′",
                    "-23° 52′","+41° 08′","+11° 42′","+11° 49′",
                    "+55° 01′","+14° 55′","+14° 26′","+15° 50′",
                    "+54° 21′","+55° 46′","+60° 42′","-11° 37′",
                    "+12° 35′","+47° 19′","-13° 03′",
                    "+55° 40′","+53° 23′","+41° 41′"),
    Con = c(NA,"Tau",
            "Aqr","CVn","Sco","Ser","Sco","Sco",
            "Sgr","Oph","Oph","Sct","Oph","Her","Oph",
            "Peg","Ser","Sgr","Sgr","Oph","Sgr",
            "Sgr","Sgr","Sgr","Sgr","Sgr","Sct","Vul",
            "Sgr","Cyg","Cap","And","And","Tri",
            "Per","Gem","Aur","Aur","Aur","Cyg","UMa",
            "CMa","Ori","Ori","Cnc","Tau","Pup","Pup",
            "Hya","Vir","Mon","CVn","Cas","Com",
            "Sgr","Sgr","Lyr","Lyr","Vir","Vir","Vir",
            "Vir","Oph","CVn","Com","Leo","Leo",
            "Cnc","Hya","Sgr","Sgr","Sge","Aqr","Aqr",
            "Psc","Sgr","Per","Cet","Ori","Lep",
            "Sco","UMa","UMa","Hya","Vir","Com","Vir",
            "Vir","Com","Vir","Vir","Com","Her","Pup",
            "CVn","Leo","Leo","UMa","Com","Com",
            "Com","UMa","Dra","Cas","Vir","Leo","CVn",
            "Oph","UMa","UMa","And"),
    Viewing.Season = c(NA,
                       "winter","autumn","spring","summer","summer",
                       "summer","summer","summer","summer",
                       "summer","summer","summer","summer","summer",
                       "autumn","summer","summer","summer","summer",
                       "summer","summer","summer","summer",
                       "summer","summer","summer","summer","summer",
                       "summer","autumn","autumn","autumn",
                       "autumn","autumn","winter","winter","winter",
                       "winter","autumn","spring","winter","winter",
                       "winter","winter","winter","winter",
                       "winter","winter","spring","winter","spring",
                       "autumn","spring","summer","summer",
                       "summer","summer","spring","spring","spring",
                       "spring","summer","spring","spring","spring",
                       "spring","winter","spring","summer",
                       "summer","summer","summer","summer","autumn",
                       "summer","autumn","autumn","winter",
                       "winter","summer","spring","spring","spring",
                       "spring","spring","spring","spring","spring",
                       "spring","spring","spring","summer",
                       "winter","spring","spring","spring","spring",
                       "spring","spring","spring","spring",
                       "summer","autumn","spring","spring","spring",
                       "summer","spring","spring","autumn"),
    Common.Name._______________ = c(NA,
                                    "Crab Nebula",NA,NA,NA,NA,"Butterfly Cluster",
                                    "Ptolemy's Cluster","Lagoon Nebula",NA,NA,
                                    "Wild Duck Cluster",NA,
                                    "Great Hercules Globular",NA,"Great Pegasus Globular",
                                    "Eagle Nebula","Omega Nebula",NA,NA,"Trifid Nebula",
                                    NA,"Sagittarius Cluster",NA,
                                    "Sagittarius Star Cloud",NA,NA,"Dumbbell Nebula",NA,NA,
                                    NA,"Andromeda Galaxy",NA,
                                    "Triangulum Galaxy",NA,NA,NA,NA,NA,NA,"Winnecke 4",NA,
                                    "Great Nebula in Orion","De Mairan's Nebula",
                                    "Beehive Cluster","Pleiades",NA,NA,NA,
                                    NA,NA,"Whirlpool Galaxy",NA,NA,NA,NA,NA,
                                    "Ring Nebula",NA,NA,NA,NA,NA,
                                    "Sunflower Galaxy","Black Eye Galaxy",NA,NA,NA,NA,
                                    NA,NA,NA,NA,NA,NA,NA,
                                    "Little Dumbbell Nebula",NA,NA,NA,NA,"Bode's Galaxy",
                                    "Cigar Galaxy","Southern Pinwheel",NA,NA,NA,NA,
                                    NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                    "Owl Nebula",NA,NA,NA,"Pinwheel Galaxy",NA,NA,
                                    "Sombrero Galaxy",NA,NA,NA,NA,NA,NA)
  )
  
  mess1 <- mess_raw[-1,]
  arcmin12 <- strsplit(mess1$arcmin1,split="x")
  arcmin1 <- as.numeric(sapply(arcmin12,"[",1))
  arcmin2 <- as.numeric(sapply(arcmin12,"[",2))
  mess1$arcmin1 <- arcmin1
  mess1$arcmin2 <- ifelse(is.na(arcmin2), arcmin1, arcmin2)
  
  mess1$area <- mess1$arcmin1*mess1$arcmin2
  
  dly <- strsplit(mess1$`Distance.(ly)`,split=" ")
  
  dly1 <- as.numeric(sapply(dly,"[",1))
  dly2 <- sapply(dly,"[",2)
  mess1$Distance..ly. <- ifelse(is.na(dly2), dly1, dly1*1000000)
  
  RAh <- as.numeric(sapply(strsplit(mess1$Right_ascension, split="h"), "[",1))
  RAm1 <- sapply(strsplit(mess1$Right_ascension, split="h "), "[",2)
  RAm2 <- strsplit(RAm1,split="")
  RAm3 <- as.numeric(sapply(RAm2, function(x) paste(x[-length(x)], collapse="")))
  
  mess1$RA_deg <- RAh*360/24 + RAm3*360/24/60
  
  DECd <- as.numeric(substr(mess1$Declination,2,3))
  DECm <- as.numeric(substr(mess1$Declination,6,7))
  DECp <- ifelse(substr(mess1$Declination,1,1)=="+",1,-1)
  
  mess1$Dec_deg <- DECp*(DECd+(DECm/60))
  
  # plot(mess1$RA_deg, mess1$Dec_deg, col=as.numeric(as.factor(mess1$Viewing.Season)))
  
  mess <- mess1
  
  
  
  # combine the two catalogs!
  cats <- data.frame(ID = c(mess$M, ngc$Primary.ID),
                     catalog = c(rep("Messier", nrow(mess)), rep("NGC", nrow(ngc))),
                     name = c(mess$Common.Name._______________, ngc$Alternate.ID),
                     type_raw = c(mess$Type, ngc$Cls),
                     con = c(mess$Con, ngc$Con),
                     season = c(mess$Viewing.Season, ngc$Season),
                     distance_LY = c(mess$`Distance.(ly)`, rep(NA, nrow(ngc))),  # could also use numeric version
                     ngc_rating = c(rep(NA, nrow(mess)), ngc$Rating),
                     RA_deg = c(mess$RA_deg, ngc$RA_deg),
                     Dec_deg = c(mess$Dec_deg, ngc$Dec_deg),
                     mag = c(mess$Mag., ngc$Mag),
                     arcmin1 = c(mess$arcmin1, ngc$arcmin1),
                     arcmin2 = c(mess$arcmin2, ngc$arcmin2))
  
  # fixing some mags
  cats$mag[cats$ID == "NGC 2359"] <- 11.45
  cats$mag[cats$ID == "NGC 2261"] <- 9
  cats$mag[cats$ID == "NGC 2024"] <- 10 # supposedly 2??
  cats$mag[cats$ID == "NGC 1788"] <- 9
  cats$mag[cats$ID == "NGC 1491"] <- 11.2
  
  
  # calculate surface brightness
  cats$bright <- 10^(-.4*cats$mag)
  cats$area <- cats$arcmin1*cats$arcmin2
  cats$surf_bright <- cats$bright/cats$area / min(cats$bright/cats$area)
  
  cats$log_surf_bright <- log10(cats$surf_bright)
  
  cats$season <- tolower(cats$season)
  
  
  # angular size in moons
  cats$moons <- cats$arcmin1/31
  
  # angular size in pixels at 135mm with crop sensor
  cats$pix_135 <- as.integer(cats$arcmin1*60/6.42)
  cats$pix_250 <- as.integer(cats$arcmin1*60/3.47)
  
  # converting angles to radians
  cats$RA_rad <- cats$RA_deg*pi/180
  cats$Dec_rad <- cats$Dec_deg*pi/180
  
  # condense type
  cats$type <- NA
  cats$type[cats$type_raw %in% c("Oc","As","Ds","OC")] <- "Open Clust"
  cats$type[cats$type_raw %in% c("Gc","GC")] <- "Glob Clust"
  cats$type[cats$type_raw %in% c("Pl","Sn","PN")] <- "Plan Neb"
  cats$type[cats$type_raw %in% c("Di","MW","EN","RN","SNR","E/RN")] <- "Neb"
  cats$type[cats$type_raw %in% c("Sp","Ba")] <- "Spiral Gal"
  cats$type[cats$type_raw %in% c("Ln","El","Ir")] <- "Ellip Gal"
  cats$type[cats$type_raw %in% c("Gal")] <- "NGC Gal"
  
  # "<a href='http://google.com' target='_blank'>GOOGLE</a>"
  # links <- c("https://en.wikipedia.org/wiki/Crab_Nebula", "https://en.wikipedia.org/wiki/Messier_2", "https://en.wikipedia.org/wiki/Messier_3", "https://en.wikipedia.org/wiki/Messier_4", "https://en.wikipedia.org/wiki/Messier_5", "https://en.wikipedia.org/wiki/Butterfly_Cluster", "https://en.wikipedia.org/wiki/Messier_7", "https://en.wikipedia.org/wiki/Lagoon_Nebula", "https://en.wikipedia.org/wiki/Messier_9", "https://en.wikipedia.org/wiki/Messier_10", "https://en.wikipedia.org/wiki/Wild_Duck_Cluster", "https://en.wikipedia.org/wiki/Messier_12", "https://en.wikipedia.org/wiki/Messier_13", "https://en.wikipedia.org/wiki/Messier_14", "https://en.wikipedia.org/wiki/Messier_15", "https://en.wikipedia.org/wiki/Eagle_Nebula", "https://en.wikipedia.org/wiki/Omega_Nebula", "https://en.wikipedia.org/wiki/Messier_18", "https://en.wikipedia.org/wiki/Messier_19", "https://en.wikipedia.org/wiki/Trifid_Nebula", "https://en.wikipedia.org/wiki/Messier_21", "https://en.wikipedia.org/wiki/Messier_22", "https://en.wikipedia.org/wiki/Messier_23", "https://en.wikipedia.org/wiki/Small_Sagittarius_Star_Cloud", "https://en.wikipedia.org/wiki/Messier_25", "https://en.wikipedia.org/wiki/Messier_26", "https://en.wikipedia.org/wiki/Dumbbell_Nebula", "https://en.wikipedia.org/wiki/Messier_28", "https://en.wikipedia.org/wiki/Messier_29", "https://en.wikipedia.org/wiki/Messier_30", "https://en.wikipedia.org/wiki/Andromeda_Galaxy", "https://en.wikipedia.org/wiki/Messier_32", "https://en.wikipedia.org/wiki/Triangulum_Galaxy", "https://en.wikipedia.org/wiki/Messier_34", "https://en.wikipedia.org/wiki/Messier_35", "https://en.wikipedia.org/wiki/Messier_36", "https://en.wikipedia.org/wiki/Messier_37", "https://en.wikipedia.org/wiki/Messier_38", "https://en.wikipedia.org/wiki/Messier_39", "https://en.wikipedia.org/wiki/Winnecke_4", "https://en.wikipedia.org/wiki/Messier_41", "https://en.wikipedia.org/wiki/Orion_Nebula", "https://en.wikipedia.org/wiki/Messier_43", "https://en.wikipedia.org/wiki/Beehive_Cluster", "https://en.wikipedia.org/wiki/Pleiades", "https://en.wikipedia.org/wiki/Messier_46", "https://en.wikipedia.org/wiki/Messier_47", "https://en.wikipedia.org/wiki/Messier_48", "https://en.wikipedia.org/wiki/Messier_49", "https://en.wikipedia.org/wiki/Messier_50", "https://en.wikipedia.org/wiki/Whirlpool_Galaxy", "https://en.wikipedia.org/wiki/Messier_52", "https://en.wikipedia.org/wiki/Messier_53", "https://en.wikipedia.org/wiki/Messier_54", "https://en.wikipedia.org/wiki/Messier_55", "https://en.wikipedia.org/wiki/Messier_56", "https://en.wikipedia.org/wiki/Ring_Nebula", "https://en.wikipedia.org/wiki/Messier_58", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  links <- paste0("https://en.wikipedia.org/wiki/",
                  cats$catalog, "_",
                  as.numeric(gsub(".*?([0-9]+).*", "\\1", cats$ID)))
  
  cats$IDD <- paste0("<a href='", links, "' target='_blank'>", cats$ID, "</a>")
  
  dotlaw <- function(d1, d2, a1, a2) {
    acos(sin(d1)*sin(d2) + cos(d1)*cos(d2)*cos(a1-a2))
  }
  cat_dists <- matrix(nrow=nrow(cats), ncol=nrow(cats))
  for(i in 1:nrow(cats)) {
    for(j in (1:nrow(cats))[-i]) {
      cat_dists[i,j] <- dotlaw(d1=cats$Dec_rad[i], d2=cats$Dec_rad[j],
                               a1=cats$RA_rad[i], a2=cats$RA_rad[j])*180/pi
    }
  }
  # cats$n_neighbors <- as.integer(rowSums(cat_dists <= 2.5, na.rm = TRUE))
  
  useforcolor <- cats$type
  # useforcolor <- cats$season
  
  thecolor <- adjustcolor(as.numeric(as.factor(useforcolor)), 
                          red.f=.85, green.f=.85, blue.f=.85) 
  thenames <- sort(unique(useforcolor))
  
  
  lat <- reactive({
    input$lat_deg*pi/180
    })
  
  alt_12am <- reactive({
    180/pi*asin((sin(lat())*sin(cats$Dec_rad))+
                                 (cos(lat())*cos(cats$Dec_rad)*cos(pi+date_adj()-cats$RA_rad)))
  })
  alt_time <- reactive({
    180/pi*asin((sin(lat())*sin(cats$Dec_rad))+
                                 (cos(lat())*cos(cats$Dec_rad)*cos(hour_adj()*pi+date_adj()-cats$RA_rad)))
  })
  az_time <- reactive({
    a <- 180/pi*acos((sin(cats$Dec_rad) - sin(alt_time()*pi/180)*sin(lat())) / 
                (cos(alt_time()*pi/180)*cos(lat())))
    ifelse(sin(hour_adj()*pi+date_adj()-cats$RA_rad) > 0, a, 360-a) 
  })
  n_neighbors <- reactive({
    as.integer(rowSums(cat_dists <= input$ndeg, na.rm = TRUE))
  })
  rankalt <- reactive({
    rank(alt_time())
  })
  ranksurf <- rank(cats$surf_bright)
  rankmag <- rank(-cats$mag)
  ranksize <- rank(cats$moons)
  # rankn <- rank(as.numeric(cats$n_neighbors))
  rankn <- reactive({
    rank(n_neighbors())
  })
  
  therank <- reactive({
    ifelse(alt_time() > input$minalt & cats$type %in% input$types,
           rank(rankalt()*input$usealt +
           ranksurf*input$usesurf +
           rankmag*input$usemag +
             ranksize*input$usesize +
             rankn()*input$usen), 1)
  })
  
  
  
  theday <- reactive({
    Sys.Date() + input$daysahead
  })
  date_adj <- reactive({
    as.numeric(theday() - as.Date("2023-03-21"))*2*pi/365.25
  })
  hour_adj <- reactive({
    (input$localtime + 12 + (input$long_deg/15 - (input$timezone + input$DST)))/12   
  })
  
  
  highlight <- reactive({
    rank(therank()) >= nrow(cats) - 5 #input$thismany
  })
  opacity <- reactive({
    (therank()/max(therank()))^7  # try transforms here??
  })
  
  
  thehighlightedone <- reactive({
    tbl1 <- cats[, c(1,3,5:8,11,22,23,17,18,19)]  # c(1,3,20,5:8,11,22,23,16,17,18,19)
    cbind(tbl1,alt_time(),alt_12am())[order(therank(), decreasing=T), ][input$whichhighlight,]
  })
  

    output$mainPlot <- renderPlot({
      
      par(mfrow=c(1,2))
      
      ### plot of altitude by time (at given date)
      
      time_rad <- seq(0,2*pi,length.out=366)[-366]
      timeslab <- times <- seq(0,24,length.out=366)[-366]+12
      timeslab[times>=24] <- times[times>=24]-24
      # dates <- as.Date("2022-03-21")+365*hour_rad/2/pi
      
      # time_adj <- hour_adj()*pi # time
      
      # position of the sun
      r <- function(x) x*pi/180  # quick function to convert degrees to radians
      
      # silly stuff from wikipedia
      # n <- as.numeric(theday - as.Date("2000-01-01"))   ## try this year and see if it makes more sense
      n <- as.numeric(theday() - as.Date("2023-01-01"))   ## try this year and see if it makes more sense
      L <- r(280.46 + 0.9856474*n)
      g <- r(357.528 + 0.9856003*n)
      lam <- L + r(1.915)*sin(g) + r(0.02)*sin(2*g)
      eps <- r(23.439 - 0.0000004*n)
      alph <- atan2(cos(eps)*sin(lam), cos(lam))
      del <- asin(sin(eps)*sin(lam))
      alt_sun <- 180/pi*asin((sin(lat())*sin(del))+(cos(lat())*cos(del)*cos(time_rad+date_adj()-alph)))
      # plot(alt_sun)
      
      alt_mat <- matrix(ncol=length(time_rad), nrow=nrow(cats))
      for(j in 1:ncol(alt_mat)) {
        alt_mat[,j] <- 180/pi*asin((sin(lat())*sin(cats$Dec_rad))+(cos(lat())*cos(cats$Dec_rad)*cos(time_rad[j]+date_adj()-cats$RA_rad)))
        ## I think this handles longitude wrong
      }  
      # plot(NA, xlim=range(dates), ylim=range(alt_mat), yaxt='n')
      plot(times,alt_mat[1,],col=0,ylim=range(alt_mat), xaxt='n', 
           ylab="Altitude", xlab="Time", main="At specified date")
      
      DST <- F   ##### fix this
      
      # axis(side=1,at=12:36,labels=c((13+DST):23,0:(13+DST)), las=2)
      axis(side=1, at=(8:40 - (input$timezone - input$long_deg/15 + input$DST)), labels=(8:40 %% 24), las=2)
      for(i in 1:nrow(alt_mat)) lines(times, alt_mat[i,],
                                      col=adjustcolor(thecolor[i], alpha.f=opacity()[i]),
                                      lwd=2+3*highlight()[i])
      for(i in (1:nrow(alt_mat))[highlight()]) text(times[alt_mat[i,]==max(alt_mat[i,])], max(alt_mat[i,]),
                                                  labels=cats$ID[i], pos=3)
      
      points(times, alt_mat[cats$ID==thehighlightedone()$ID,], col=adjustcolor(1,alpha.f=.1), cex=3)
      
      # col=as.numeric(as.factor(mess$Type))[i])
      # lines(times,alt_mat[42,],lwd=4)
      abline(h=c(0,30,60),lwd=c(2,1,1),lty=c(1,2,2))
      points(times,alt_sun)
      # lines(times,200*(alt_sun<(0))-100)
      # lines(times,200*(alt_sun<(-6))-100, lty=2)
      # lines(times,200*(alt_sun<(-12))-100, lty=3)
      # lines(times,200*(alt_sun<(-18))-100, lty=4)
      polygon(x=c(0,0,40, 40, rev(times)), y=c(100, rep(-100,2), 100, rev(200*(alt_sun>(0))-100)), 
              border=NA, col=adjustcolor(1, alpha.f=.07))
      polygon(x=c(0,0,40, 40, rev(times)), y=c(100, rep(-100,2), 100, rev(200*(alt_sun>(-6))-100)), 
              border=NA, col=adjustcolor(1, alpha.f=.07))
      polygon(x=c(0,0,40, 40, rev(times)), y=c(100, rep(-100,2), 100, rev(200*(alt_sun>(-12))-100)), 
              border=NA, col=adjustcolor(1, alpha.f=.07))
      polygon(x=c(0,0,40, 40, rev(times)), y=c(100, rep(-100,2), 100, rev(200*(alt_sun>(-18))-100)), 
              border=NA, col=adjustcolor(1, alpha.f=.07))
      legend("bottomright",col=adjustcolor(1:length(thenames), red.f=.85, blue.f=.85, green.f=.85),lwd=2,legend=thenames)      
      ### plot of altitude by date (at given time)
      
      date_rad <- seq(0,2*pi,length.out=366)[-366]
      # date0 <- as.Date("2023-03-21")
      date0 <- as.Date(paste0(format(theday() - 80, "%Y"), "-03-21"))
      dates <- date0+365*date_rad/2/pi
      
      time_adj <- hour_adj()*pi # time not including DST
      # time_adj <- 12/12*pi # 12pm     
      
      alt_mat <- matrix(ncol=length(date_rad), nrow=nrow(cats))
      for(j in 1:ncol(alt_mat)) {
        alt_mat[,j] <- 180/pi*asin((sin(lat())*sin(cats$Dec_rad))+(cos(lat())*cos(cats$Dec_rad)*cos(date_rad[j]+time_adj-cats$RA_rad)))
        ## I think this handles longitude wrong
      }  
      # plot(NA, xlim=range(dates), ylim=range(alt_mat), yaxt='n')
      plot(dates,alt_mat[1,],col=0,ylim=range(alt_mat), 
           ylab="Altitude", xlab="Date", main="At specified time")
      # axis(side=2,at=seq(-90,90,by=30))
      for(i in 1:nrow(alt_mat)) lines(dates, alt_mat[i,], 
                                      col=adjustcolor(thecolor[i], alpha.f=opacity()[i]),
                                      lwd=2+3*highlight()[i])
      for(i in (1:nrow(alt_mat))[highlight()]) text(dates[alt_mat[i,]==max(alt_mat[i,])], max(alt_mat[i,]),
                                                  labels=cats$ID[i], pos=3)
      
      points(dates, alt_mat[cats$ID==thehighlightedone()$ID,], col=adjustcolor(1,alpha.f=.1), cex=3)
      
      # col=as.numeric(as.factor(mess$Type))[i])
      # lines(dates,alt_mat[42,],lwd=4)
      abline(h=c(0,30,60),lwd=c(2,1,1),lty=c(1,2,2))
      abline(v=theday())
      text(x=theday(), y=par("usr")[4], labels=theday(), xpd=NA, pos=3)
      legend("bottomright",col=adjustcolor(1:length(thenames), red.f=.85, blue.f=.85, green.f=.85),lwd=2,legend=thenames)
      
      

    })
    
    output$subPlot <- renderPlot({
      # par(mfrow=c(2,2))
      # plot(density(-cats$mag))
      # points(x=-cats$mag, y=rep(0, nrow(cats)))
      # abline(v=-thehighlightedone()$mag)
      # 
      # plot(density(cats$moons))
      # points(x=cats$moons, y=rep(0, nrow(cats)))
      # abline(v=thehighlightedone()$moons)
      # 
      # plot(density(cats$surf_bright))
      # points(x=cats$surf_bright, y=rep(0, nrow(cats)))
      # abline(v=thehighlightedone()$surf_bright)
      # 
      # plot(density(alt_time()))
      # points(x=alt_time(), y=rep(0, nrow(cats)))
      # # abline(v=thehighlightedone()$surf_bright)
      
      par(mfrow=c(1,2))   ########################## add a second panel to this plot
      
      # plot(cats$moons, cats$surf_bright, log="xy", col=thecolor, pch=16, 
      #      xlab="Apparent size in Moons", ylab="Approx Surface Brightness (/min)")
      plot(cats$moons, cats$surf_bright, log="xy", col=0, pch=16, 
           xlab="Apparent size in Moons", ylab="Approx Surface Brightness (/min)")
      for(i in 1:nrow(cats)) {
        points(cats$moons[i], cats$surf_bright[i], pch=21,
               col=thecolor[i],
               bg=adjustcolor(thecolor[i], alpha.f=(.1+.9*opacity()[i])))
      }
      
      
      # abline(h=thehighlightedone()$surf_bright, col=adjustcolor(1,alpha.f=.5))
      abline(h=cats$surf_bright[order(therank(), decreasing=T)][input$whichhighlight], 
             col=adjustcolor(1,alpha.f=.5))
      abline(v=thehighlightedone()$moons, col=adjustcolor(1,alpha.f=.5))
      # labtext <- paste(thehighlightedone()$ID[1], thehighlightedone()$name[1])
      labtext <- ifelse(!is.na(thehighlightedone()$name[1]),
                        paste0("#", input$whichhighlight, ": ", thehighlightedone()$ID[1], " ", thehighlightedone()$name[1], " (", thehighlightedone()$type, ")"),
                        paste0("#", input$whichhighlight, ": ", thehighlightedone()$ID[1], " (", thehighlightedone()$type, ")"))
      mtext(text=labtext, at=thehighlightedone()$moons, side=3, line=1)
      
      xplot <- (90-alt_time())/90*sin(pi/180*az_time())
      yplot <- (90-alt_time())/90*cos(pi/180*az_time())
      plot(NA, xlim=c(-1,1), ylim=c(-1,1), xlab="", ylab="", main="", 
           bty="n", yaxt="n", xaxt="n")
      polygon(x=cos(seq(0,to=2*pi,length.out=100)), y=sin(seq(0,to=2*pi,length.out=100)),
              border="grey")
      # polygon(x=0.5*cos(seq(0,to=2*pi,length.out=100)), y=0.5*sin(seq(0,to=2*pi,length.out=100)),
      #         border="grey", lty=3)
      polygon(x=0.33*cos(seq(0,to=2*pi,length.out=100)), y=0.33*sin(seq(0,to=2*pi,length.out=100)),
              border="grey", lty=3)
      polygon(x=0.67*cos(seq(0,to=2*pi,length.out=100)), y=0.67*sin(seq(0,to=2*pi,length.out=100)),
              border="grey", lty=3)
      for(i in 1:length(xplot)) {
        points(xplot[i], yplot[i], pch=16, 
               col=adjustcolor(thecolor[i], alpha.f=opacity()[i]))
      }
      xhighlight <- xplot[order(therank(), decreasing=T)][input$whichhighlight]
      yhighlight <- yplot[order(therank(), decreasing=T)][input$whichhighlight]
      points(xhighlight, yhighlight, cex=2)
      
      text(y=c(1,-1,0,0), x=c(0,0,1,-1), labels=c("N","S","W","E"))
      
    })
    
    output$thetable <- renderTable({
      # tbl1 <- cats[, c(1,3,22,18,19,11,23,17,5:8)]  # c(1,3,20,5:8,11,22,23,17,18,19)
      tbl1 <- cats[, c(24,3,23,18,19,20,11,17,5:8)]  # c(1,3,20,5:8,11,22,23,17,18,19)
      cbind(tbl1,n_neighbors(),alt_time(),alt_12am())[order(therank(), decreasing=T), ]
    }, sanitize.text.function = function(x) x)
    
    output$theothertable <- renderTable({
      thehighlightedone()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
