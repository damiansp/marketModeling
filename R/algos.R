#---------#---------#---------#---------#---------#---------#---------#---------
load('~/Learning/marketModeling/data/myHistoric.RData')
head(df)
tail(df)

dailyChange <- function(x) {
  (x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)]
}

m <- dim(df)[1]
last <- m

wlsh.osc <- c(
  -77, -73, -80, -88, -97,-106,-110,-112,-122,-132,-151,-173,-199,-233,-261,-294,
 -325,-357,-375,-381,-381,-379,-388,-392,-397,-396,-386,-386,-394,-402,-401,-399,
 -407,-419,-434,-456,-468,-467,-466,-472,-468,-467,-470,-486,-497,-500,-508,-516,
 -528,-528,-516,-498,-473,-441,-424,-414,-376,-350,-321,-287,-259,-240,-219,-196,
 -181,-173,-164,-157,-157,-158,-152,-152,-153,-144,-137,-138,-152,-161,-183,-202,
 -208,-230,-246,-258,-256,-270,-287,-317,-343,-375,-402,-415,-429,-431,-450,-467,
 -482,-488,-497,-500,-491,-472,-455,-443,-421,-406)
SPActual <- c(df$SPActual,
  4797,4794,4701,4696,4677,4670,4713,4726,4659,4663,4577,4533,4483,4398,4410,
  4356,4350,4327,4432,4516,4547,4589,4477,4501,4484,4522,4587,4504,4419,4402,
  4471,4475,4380,4349,4305,4226,4289,4385,4374,4306,4387,4363,4329,4201,4171,
  4278,4260,4204,4173,4262,4358,4412,4463,4461,4512,4456,4520,4543,4576,4632,
  4602,4530,4546,4583,4525,4481,4500,4488,4413,4397,4447,4393,4392,4462,4459,
  4394,4272,4296,4175,4184,4288,4132,4155,4175,4300,4147,4123,3991,4001,3935,
  3930,4024,4008,4089,3924,3901,3901,3974,3941,3979,4058,4158,4132,4101,4177,
  4109)
SP <- SPActual / SPActual[last]
SPd <- dailyChange(SP)

FidIn <- c(df$FidIn,
   57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 61104,
   61104, 62434, 62434, 62434, 62434, 62434,160743,160743,160743,161785,161785,
  161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,
  161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,
  161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,
  161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,
  161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,
  161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,
  161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,161785,
  161785,161785,161785,161785,161785,161785,161785) #
FidVal <- c(df$FidVal,
   94096, 90621, 86085, 86306, 84787, 85416, 87205, 87106, 83943, 83649, 84545,
   84094, 85649, 82388, 83399, 81424, 80533,218550,222221,229970,232595,228121,
  221810,225872,225565,227194,232232,231501,226454,226222,230340,230174,224468,
  220354,218267,212341,223870,228841,232960,230158,230738,225290,220206,215209,
  214494,218804,217080,213226,207439,213845,227138,236682,250020,243490,251352,
  245734,249364,242369,247895,257702,251481,245407,249188,258258,248363,238575,
  237355,231025,228022,224991,231757,223658,220440,226673,222025,216685,213970,
  217134,211660,210306,213446,208981,212931,212020,216771,207224,202355,193896,
  192151,187333,191359,199792,195092,198413,193282,196613,195702,195714,189526,
  192622,196485,201013,199789,197811,204343,200244) #
Fid <- FidVal / FidIn
Fid <- Fid / Fid[last]
Fidd <- dailyChange(Fid)

f1kIn <- c(df$f1kIn,
   98297, 98297, 98297, 98297, 98297, 98297, 98297, 98297, 98297, 98297, 98297,
   98297, 98297, 98297, 98297, 98297, 98297,0.6935,0.6935,0.6935,0.6935,0.6935,
  0.6935,0.6935,0.6935,0.6935,0.6935,0.6935,0.6935,0.6935,0.6935,0.6935,0.6935,
     620,   620,   620,   620,   620,   620,   620,   620,   620,  1306,  1306,
    1306,  1306,  1306,  1306,  1306,  1306,  1306,  2023,  2023,  2023,  2023,
    2023,  2023,  2023,  2023,  2023,  2023,  2023,  2023,  2023,  2023,  2023,
    2690,  2690,  2690,  2690,  2690,  2690,  2690,  2690,  2690,  3375,  3375,
    3375,  3375,  3375,  3375,  3375,  3375,  3375,  3375,  4129,  4129,  4129,
    4129,  4129,  4129,  4129,  4129,  4129,  4129,  4936,  4936,  4936,  4936,
    4936,  4936,  4936,  4936,  4936,  4936,  5729) #
f1kVal <- c(df$f1kVal,
  157563,158404,157348,153323,153323,151838,151838,153394,153734,150997,151082,
  147813,146496,145095,141741,141741,141741,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     863,   855,   847,   824,   854,   867,   867,   852,   869,  1818,  1787,
    1712,  1707,  1772,  1764,  1731,  1702,  1752,  2769,  2808,  2865,  2856,
    2895,  2900,  2911,  2908,  2940,  2988,  2954,  2911,  2921,  2967,  2914,
    3818,  3833,  3791,  3711,  3696,  3767,  3706,  3699,  3774,  4717,  4597,
    4455,  4511,  4342,  4349,  4474,  4276,  4319,  4301,  5377,  5117,  5030,
    4790,  4837,  4728,  4712,  4887,  4820,  4922,  5657,  5652,  5641,  5744,
    5631,  5714,  5850,  6042,  6017,  5985,  7121) #
n <- length(f1kIn)
#payment * f1kIn[n] / f1kVal[n] + f1kIn[n]

f1k <- f1kVal / f1kIn
f1k <- f1k / f1k[last]
f1kd <- dailyChange(f1k)
f1k[is.na(f1k)] <- NA
f1kd[is.na(f1kd)] <- NA

OHIn <- c(df$OHIn,
    49269, 49269, 49269, 49269, 49269, 49269, 49269, 49269, 49269, 49269, 49269,
    49269, 49269, 49269, 49269, 49269, 49269, 49269, 49269, 49864, 49864, 49864,
    49864, 49864, 49864, 49864, 49864, 49864, 49864, 49864, 49864, 50799, 50799,
    51650, 51650, 51650, 52046, 52789, 53816, 53816, 55281, 55281, 55281, 55281,
    55621, 55621, 55621, 55621, 57649, 57649, 58646, 58978, 58978, 58978, 58978,
    58978, 58978, 58978, 58978, 58978, 58978, 58978, 59760, 59760, 59760, 59760,
    59760, 59760, 59760, 59760, 59760, 59760, 60579, 60579, 60579, 60579, 60643,
    60643, 60643, 61614, 61614, 61614, 62294, 62294, 62294, 62294, 62294, 62945,
    63568, 63568, 63568, 63568, 64053, 64053, 64053, 64053, 64053, 64539, 64539,
    65847, 65847, 65847, 66684, 66189, 66189, 66189) #
OHVal <-c(df$OHVal,    
   102557,100422, 97844, 98121, 97375, 97932, 98939, 99051, 97002, 96791, 95665,
    95391, 95163, 93596, 94389, 93136, 92819, 92495, 94639, 99447,100868, 98399,
    94494, 97379, 97364, 98090,100648,100016, 98296, 97198, 99552,100015, 96612,
    95617, 94996, 92698, 98555,101932,105235,104528,107470,105753,103912,101474,
   101801,103713,103204,102112,103435,107191,117790,121618,127554,123673,127957,
   126322,127162,124866,126801,131445,130528,127861,130695,135516,131259,127588,
   126916,124159,122811,121324,125142,121775,119628,123063,121167,118242,116101,
   117227,114478,116299,117700,115609,117580,116664,118435,114622,111952,108212,
   109388,106990,108934,114344,112948,114852,111417,113169,113265,113867,110468,
   114669,116925,119933,121272,119546,123545,121431) #
OH <- OHVal / OHIn
OH <- OH / OH[last]
OHd <- dailyChange(OH)
totalFunds <- FidVal + f1kVal + OHVal


# Mon
m1 <- (c(
  100000, 99734, 97021, 92607, 93071, 91736, 92132, 94069, 93531, 90683, 90091,
   87564, 87271, 86460, 84269, 85206, 82751, 81848, 80050, 82238, 86565, 87949,
   86042, 83068, 85330, 85522, 87001, 90263, 89454, 87144, 86604, 89342, 88849,
   85501, 82663, 81832, 79154, 83928, 85585, 87402, 86468, 86568, 83415, 80900,
   77588, 77022, 80586, 79208, 75363, 71932, 74564, 79665, 82683, 85919, 84263,
   87026, 85860, 87058, 85346, 86812, 90121, 88040, 86501, 87299, 90475, 87259,
   84010, 83687, 81901, 81058, 80459, 82912, 80389, 78921, 81832, 78883, 75269,
   73241, 75371, 71659, 71117, 72945, 70469, 72978, 72235, 75066, 69732, 66717,
   61297, 59691, 56166, 58338, 63260, 61025, 63151, 60198, 62075, 61473, 61632,
   58010, 59528, 61860, 64602, 63146, 61926, 65673, 63181)
  / 100000)
#qp.start <- 92
m1.name <- 'sim1'
  
# Tues
m2 <- (c(
  100000, 99519, 96234, 91473, 91946, 90337, 90702, 92795, 92113, 88699, 88134,
   85502, 84905, 84010, 81444, 82682, 80076, 79132, 77316, 79826, 84924, 86845,
   84916, 81811, 83915, 84091, 85475, 88480, 87430, 84757, 84215, 86909, 86224,
   82882, 80170, 79277, 76822, 81232, 82869, 84376, 83620, 83943, 80812, 78105,
   74317, 73857, 77530, 76241, 72815, 69899, 72093, 76498, 78989, 81897, 80630,
   83072, 81761, 82686, 81217, 82571, 85155, 83529, 82094, 82940, 85501, 82670,
   79775, 79483, 77784, 76954, 76282, 78337, 76131, 74997, 77613, 75151, 71995,
   70160, 71885, 68718, 68253, 70129, 67816, 69893, 68939, 70952, 66282, 63781,
   58921, 57771, 54938, 56562, 60454, 58552, 60800, 58448, 59895, 59358, 59731,
   56514, 57741, 60025, 62593, 61344, 60351, 63305, 61248)
  / 100000) 
m2.name <- 'sim2'

# Wed
m3.raw <- c(
  100000, 99890, 96310, 91049, 91683, 89443, 89246, 91155, 90781, 88037, 87453, 
   85588, 85124, 85345, 82818, 83265, 80641, 79763, 77546, 79590, 84694, 86997,
   84065, 80991, 82349, 81679, 83145, 87233, 85828, 83761, 83048, 85413, 85793,
   82981, 80101, 79476, 77510, 84759, 86351, 89583, 87106, 88107, 85490, 83370,
   80189, 79725, 82573, 81231, 77547, 74984, 77353, 81368, 84414, 87515, 86385,
   88753, 87410, 88721, 86792, 88646, 91518, 89937, 88551, 89545, 91913, 88982,
   86036, 85895, 83995, 82998, 82196, 84528, 82200, 80832, 83790, 81297, 77940,
   75824, 77783, 74449, 73896, 75729, 73478, 75238, 74645, 76035, 71760, 69444,
   65646, 64752, 62474, 63326, 67463, 66242, 69585, 67034, 68403, 67820, 67875,
   65479, 66684, 69321, 71693, 70319, 69583, 72723, 70255)
m3 <- m3.raw / 100000
m3.name <- 'sim3'

# Thur
m4.raw <- c(
  100000, 99289, 96063, 91314, 91644, 90074, 90327, 92360, 91830, 88380, 88025,
   85472, 84872, 84283, 81799, 82759, 80245, 79590, 78131, 80209, 83986, 85118,
   84282, 81703, 83586, 83495, 84327, 87216, 86221, 83907, 83463, 86119, 85921,
   82848, 80632, 79966, 78132, 82004, 83071, 84342, 83314, 83823, 80951, 78736,
   75363, 74865, 78112, 77060, 73747, 70732, 73045, 77623, 80265, 83692, 82327,
   84860, 83303, 84431, 83013, 84238, 86767, 85113, 84015, 84779, 86850, 84106,
   81387, 81126, 79584, 78809, 78162, 80299, 78306, 77228, 79702, 77511, 74441,
   72509, 74178, 70926, 70529, 72285, 69846, 71661, 70726, 72745, 68116, 65405,
   60320, 59258, 56414, 57910, 61900, 60036, 61948, 59727, 61018, 60880, 61074,
   57979, 59113, 61391, 64041, 62777, 61570, 64841, 62550)
m4 <- m4.raw / 100000
m4.name <- 'sim4'

# Fri
m5 <- (c(
  100000, 99775, 96651, 91870, 92422, 91307, 92182, 94394, 93768, 89786, 89177,
   86127, 85615, 84895, 81979, 83919, 81050, 80084, 77906, 80564, 86086, 88236,
   85575, 82009, 84571, 84519, 85750, 89164, 87765, 85391, 84953, 87773, 86529,
   83335, 80597, 79997, 77851, 82453, 84232, 85985, 85142, 85000, 82028, 79579,
   76314, 76131, 80113, 78878, 75174, 71810, 74769, 81328, 84511, 88583, 86653,
   89302, 88138, 88974, 87142, 88853, 92280, 90186, 88372, 89101, 92223, 89249,
   86336, 85805, 84045, 83406, 82627, 85007, 82435, 81087, 83991, 81153, 77696,
   75672, 77615, 73847, 73290, 75208, 72323, 74707, 73957, 76467, 70971, 68378,
   63066, 62220, 59209, 61155, 66022, 63790, 65607, 62780, 64470, 64079, 64225,
   60701, 62250, 64564, 67228, 65990, 64750, 68496, 66102)
  / 100000)
m5.name <- 'sim5'  

# Sat
m6 <- (c(
  100000,100012, 98204, 96007, 96400, 95686, 96971, 97980, 98605, 95083, 95146,
   93567, 93268, 93246, 90907, 92420, 90796, 90046, 88126, 91095, 95457, 97577,
   94775, 91892, 93896, 93335, 94248, 97118, 96630, 94595, 94040, 96516, 96062,
   92644, 89662, 89136, 86860, 91581, 94435, 96429, 94844, 95051, 91299, 87998,
   82856, 81936, 87122, 85525, 81217, 78300, 81197, 86988, 90084, 95329, 92914,
   95327, 93939, 95411, 93709, 96084,100081, 98029, 96120, 97397,100495, 96992,
   93601, 93350, 91191, 90400, 89442, 92042, 89235, 87697, 91168, 88173, 84425,
   82103, 84200, 80402, 79518, 81849, 78395, 81565, 80631, 83689, 77294, 74176,
   67817, 66609, 63340, 65512, 71594, 68836, 71399, 68010, 70177, 69702, 69719,
   65619, 67372, 70416, 73573, 72197, 71214, 74628, 72069)
  / 100000)
m6.name <- 'sim6'  

# Sun
m7 <- (c(
  100000, 99901, 98262, 96147, 96334, 95340, 95359, 96898, 96645, 95102, 94744,
   93378, 92032, 92532, 90635, 91392, 89184, 88026, 85549, 87371, 91761, 93129,
   91146, 88692, 89998, 90197, 90946, 93189, 92856, 91261, 90848, 92961, 93142,
   90538, 88038, 87669, 86034, 90318, 91664, 93607, 92079, 92963, 90210, 87478,
   83126, 82742, 86523, 85175, 81054, 77653, 80287, 85058, 88263, 91422, 89849,
   92723, 91477, 92380, 90661, 92244, 95161, 93805, 92365, 92990, 95293, 92139,
   89070, 89063, 87319, 86410, 85787, 88088, 85587, 84303, 87236, 84698, 81437,
   79338, 81615, 77817, 77332, 78610, 76235, 78196, 76737, 78814, 73402, 69301,
   64540, 63160, 60706, 62340, 65974, 63724, 66402, 63517, 65418, 64702, 64687,
   61186, 63451, 65966, 68653, 67303, 65926, 69089, 66608)
  / 100000)
m7.name <- 'sim7'  


totin <- FidIn + f1kIn + OHIn
totval <- FidVal + f1kVal + OHVal
tot <- totval / totin 
tot <- tot / tot[last]
minein <- FidIn + OHIn
mineval <- FidVal + OHVal
mine <- mineval / minein
mine <- mine / mine[last]
m1 <- m1 / m1[1]
m2 <- m2 / m2[1]
m3 <- m3 / m3[1]
m4 <- m4 / m4[1]
m5 <- m5 / m5[1]
m6 <- m6 / m6[1]
m7 <- m7 / m7[1]

# Calculate Sharpe Ratios
Sharpe <- function(returns, window=length(returns)) {
  if (length(returns) > window) {
    returns <- returns[(length(returns) - window):length(returns)]	  	
  }	
  returns <- returns[!is.na(returns)]
  (sqrt(252) * mean(returns)) / sd(returns)
}

get.daily.returns <- function(x) { 
  n <- length(x)
  x[2:n] / x[1:(n - 1)] - 1 
}

n <- length(m1) 
m <- length(SP)
this.year <- (m-n):m

SP.sh  <- round(Sharpe(get.daily.returns(SP[this.year])), 3)
tot.sh <- round(Sharpe(get.daily.returns(tot[this.year])), 3)
mine.sh <- round(Sharpe(get.daily.returns(mine[this.year])), 3)
m1.sh  <- round(Sharpe(get.daily.returns(m1)), 3)
m2.sh  <- round(Sharpe(get.daily.returns(m2)), 3)
m3.sh  <- round(Sharpe(get.daily.returns(m3)), 3)
m4.sh  <- round(Sharpe(get.daily.returns(m4)), 3)
m5.sh  <- round(Sharpe(get.daily.returns(m5)), 3)
m6.sh  <- round(Sharpe(get.daily.returns(m6)), 3)
m7.sh  <- round(Sharpe(get.daily.returns(m7)), 3)
prev <- m - n
days <- -prev:(n - 1)
new.days <- 0:(n - 1)
#plot(new.days,
#     m1, 
#     type='l', 
#     xlim=range(days),
#     ylim=range(c(SP,  tot, 1 + tot - SP, mine, m1,  m2, m3, m4, m5, m6, m7)), 
#     lwd=3, 
#     log='y')
#abline(h=seq(0, 8, 0.1), lty=2, col=rgb(0, 0, 0, 0.2))
#abline(h=seq(0, 8, 0.5), lty=4, col=rgb(0, 0, 0, 0.8))
#abline(v=0, lwd=2)
#lines(days, tot, col=2, lwd=3)
#lines(days, mine, col='cadetblue', lwd=3)
#lines(days, SP, col='yellow', lwd=3)
#lines(new.days, m2, lwd=3, col='coral')
#lines(new.days, m3, lwd=3, col='blue')
#lines(new.days, m4, lwd=3, col='purple')
#lines(new.days, m5, lwd=3, col='cyan')
#lines(new.days, m6lwd=3, col='grey')
#lines(new.days, m7 lwd=3, col='magenta')
#lines(days, 1 + tot - SP, col='#880000')
#legend(
#  'topleft', 
#  lty=1, 
#  lwd=c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1), 
#  col=c('black', 'red ', 'cadetblue', 'yellow',  'coral', 'blue', 'purple', 
#        'cyan', 'grey', 'magenta', '#880000'),
#  legend=c(paste(m1.sh, m1.name), 
#           paste(tot.sh, 'Actual'), 
#           paste(mine.sh, 'Mine'),
#           paste(SP.sh, 'S&P'), 
#           paste(m2.sh, m2.name), 
#           paste(m3.sh, m3.name), 
#           paste(m4.sh, m4.name), 
#           paste(m5.sh, m5.name), 
#           paste(m6.sh, m6.name), 
#           paste(m7.sh, m7.name), 
#           'Actual Diff'),
#  bty='n')
  
#quartz()  
plot(new.days,
     m1, 
     type='l', 
     xlim=range(days[days >= 0]),
     ylim=c(0.4,
            max(c(SP,  tot, 1 + tot - SP, mine, m1,  m2, m3, m4, m5, m6, m7))), 
     lwd=3, 
     log='y')
abline(h=seq(0, 8, 0.1), lty=2, col=rgb(0, 0, 0, 0.2))
abline(h=seq(0, 8, 0.5), lty=4, col=rgb(0, 0, 0, 0.8))
abline(v=0, lwd=2)
lines(days, tot, col=2, lwd=3)
lines(days, mine, col='cadetblue', lwd=3)
lines(days, SP, col='yellow', lwd=3)
lines(new.days, m2, lwd=3, col='coral')
lines(new.days, m3, lwd=3, col='blue')
lines(new.days, m4, lwd=3, col='purple')
lines(new.days, m5, lwd=3, col='cyan')
lines(new.days, m6, lwd=3, col='grey')
lines(new.days, m7, lwd=3, col='magenta')
lines(days, 1 + tot - SP, col='#880000')
legend(
  'topleft', 
  lty=1, 
  lwd=c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1), 
  col=c('black', 'red ', 'cadetblue', 'yellow',  'coral', 'blue', 'purple', 
        'cyan', 'grey', 'magenta', '#880000'),
  legend=c(paste(m1.sh, m1.name), 
           paste(tot.sh, 'Actual'), 
           paste(mine.sh, 'Mine'),
           paste(SP.sh, 'S&P'), 
           paste(m2.sh, m2.name), 
           paste(m3.sh, m3.name), 
           paste(m4.sh, m4.name), 
           paste(m5.sh, m5.name), 
           paste(m6.sh, m6.name), 
           paste(m7.sh, m7.name), 
           'Actual Diff'),
  bty='n')
abline(v=qp.start)
abline(h=1, lty=4)
  
# Downward vals only
sp.start <- which(SP == 1)
n.sp <- length(SP)
sp.rec <- SP[sp.start:n.sp]
n.sp <- length(sp.rec)
sp.ch <- sp.rec[2:n.sp] / sp.rec[1:(n.sp-1)]
m1.ch <- m1[2:n.sp] / m1[1:(n.sp-1)]
m2.ch <- m2[2:n.sp] / m2[1:(n.sp-1)]
m3.ch <- m3[2:n.sp] / m3[1:(n.sp-1)]
m4.ch <- m4[2:n.sp] / m4[1:(n.sp-1)]
m5.ch <- m5[2:n.sp] / m5[1:(n.sp-1)]
m6.ch <- m6[2:n.sp] / m6[1:(n.sp-1)]
m7.ch <- m7[2:n.sp] / m7[1:(n.sp-1)]
down.days <- which(sp.ch <= 1)
up.days <- which(sp.ch > 1)
quartz()
plot(cumprod(c(1, sp.ch[down.days])), 
     type='l', 
     col='yellow', 
     ylim=range(cumprod(c(1, m1.ch[down.days])), 
                cumprod(c(1, m2.ch[down.days])), 
                cumprod(c(1, m3.ch[down.days])), 
                cumprod(c(1, m4.ch[down.days])), 
                cumprod(c(1, m5.ch[down.days])), 
                cumprod(c(1, m6.ch[down.days])), 
                cumprod(c(1, m7.ch[down.days]))), 
     log='y',
     lwd=2,
     main='Downward Days Only',
     ylab='Change')
lines(cumprod(c(1, m1.ch[down.days])), col='black', lwd=2)  
lines(cumprod(c(1, m2.ch[down.days])), col='coral', lwd=2)  
lines(cumprod(c(1, m3.ch[down.days])), col='blue', lwd=2)  
lines(cumprod(c(1, m4.ch[down.days])), col='purple', lwd=2)  
lines(cumprod(c(1, m5.ch[down.days])), col='cyan', lwd=2)  
lines(cumprod(c(1, m6.ch[down.days])), col='grey', lwd=2)  
lines(cumprod(c(1, m7.ch[down.days])), col='magenta', lwd=2)  
legend('topright', 
       lty=1,
       lwd=2, 
       col=c('yellow', 'black', 'coral', 'blue', 'purple', 'cyan', 'grey', 
             'magenta'),
       legend=c('SP', m1.name, m2.name, m3.name, m4.name, m5.name, m6.name, 
                m7.name))
abline(h=1, lty=4)                
quartz()
plot(cumprod(c(1, sp.ch[up.days])), 
     type='l', 
     col='yellow', 
     ylim=range(cumprod(c(1, sp.ch[up.days])),
                cumprod(c(1, m1.ch[up.days])), 
                cumprod(c(1, m2.ch[up.days])), 
                cumprod(c(1, m3.ch[up.days])), 
                cumprod(c(1, m4.ch[up.days])), 
                cumprod(c(1, m5.ch[up.days])), 
                cumprod(c(1, m6.ch[up.days])), 
                cumprod(c(1, m7.ch[up.days]))), 

     log='y',
     lwd=2,
     main='Upward Days Only',
     ylab='Change')
lines(cumprod(c(1, m1.ch[up.days])), col='black', lwd=2)  
lines(cumprod(c(1, m2.ch[up.days])), col='coral', lwd=2)  
lines(cumprod(c(1, m3.ch[up.days])), col='blue', lwd=2)  
lines(cumprod(c(1, m4.ch[up.days])), col='purple', lwd=2)  
lines(cumprod(c(1, m5.ch[up.days])), col='cyan', lwd=2)  
lines(cumprod(c(1, m6.ch[up.days])), col='grey', lwd=2)  
lines(cumprod(c(1, m7.ch[up.days])), col='magenta', lwd=2)  
legend('topleft', 
       lty=1,
       lwd=2, 
       col=c('yellow', 'black', 'coral', 'blue', 'purple', 'cyan', 'grey', 
             'magenta'),
       legend=c('SP', m1.name, m2.name, m3.name, m4.name, m5.name, m6.name, 
                m7.name))
abline(h=1, lty=4)

#______________________________________________________________________________


quartz()
x <- 1:length(totalFunds)
plot(x, 
     totalFunds, 
     type = 'l', 
     ylim=c(0, max(totalFunds)), 
     main='1 = 01 Jan 2018')#, log='y')
#savings.mod <- lm(totalFunds ~ x)

exp.mod <- function(funds, x) {
  savings.exp.mod <- lm(log(funds) ~ x)
  log.y <- coef(savings.exp.mod)[1] + x*coef(savings.exp.mod)[2]
  exp(log.y)  
}


#abline(savings.mod, col=rgb(0, 0, 0, 0.25))
start <- totalFunds[last]
now <- totalFunds[length(totalFunds)]
low <- min(totalFunds[(last):(length(totalFunds))])
high <- max(totalFunds[(last):(length(totalFunds))])
earned <- high - start
down.from.high <- high - now
amt.down <- round(-100 *  down.from.high / earned, 2)
lines(x, exp.mod(totalFunds, x), col=rgb(0, 0, 0, 1))
abline(h=start, col='grey')
abline(h=now, col='grey')
abline(h=low, col=2)
abline(h=high, col=3)

lines(FidVal + f1kVal, col=4)
lines(x, exp.mod(FidVal + f1kVal, x), col=rgb(0, 0, 1, 1))

lines(f1kVal[790:length(f1kVal)], col=5)#
lines(
  x[790:length(f1kVal)], 
  exp.mod(f1kVal[790:length(f1kVal)], x[790:length(f1kVal)]), 
  col=rgb(0, 1, 1, 1))

lines(OHVal, col=6)
lines(x, exp.mod(OHVal, x), col=rgb(1, 0, 1, 1))

# FIX HERE
#amt.down <- round(100 * pct, 2)
amt.up <- round(100 * ((totalFunds[length(totalFunds)] / start) - 1), 2)
legend(
  'topleft', 
  text.col=c('green', 'black', 'red'), 
  legend=c(sprintf('$%d', max(totalFunds)),
  sprintf('$%d\n(%.2f%% down\n  %.2f%% up)',
          totalFunds[length(totalFunds)],
          amt.down,
          amt.up),
          sprintf('$%d', low)), 
  bg=rgb(1, 1, 1, 0.75))
legend('bottomright',
       lty=1,
       col=c(4, 6, 1),
       legend=c('Fidelity (+rollover)', 'E*Trade', 'Total'),
       bg=rgb(1, 1, 1, 0.75))

day <- length(f1kIn) - 1 # length(<some new algo>) - 1
# Pad with leading NAs to make all vectors of equal length
tLength <- length(SP)
#Fid     <- c(rep(NA, tLength - length(Fid)),     Fid)
#OH      <- c(rep(NA, tLength - length(OH)),      OH)
#f1k     <- c(rep(NA, tLength - length(f1k)),     f1k)
days <- seq(to=day, by=1, length=tLength)

d <- data.frame(SP, Fid, OH, f1k)
realWts <- matrix(data=c(FidVal, OHVal, f1kVal), ncol=3)
realWts <- realWts / rowSums(realWts, na.rm = T)
realWts[is.na(realWts)] <- 0 
nWts <- length(realWts[, 1])
meanReal <- (Fid[(length(Fid) - nWts + 1):length(Fid)] * realWts[, 1] +
             OH[(length(OH)   - nWts + 1):length(OH)]  * realWts[, 2] +
             f1k[(length(OH)  - nWts + 1):length(f1k)] * realWts[, 3])
#meanReal <- c(rep(NA, 249), meanReal)
colors <- c(SP='yellow', Fid='red', OH='green', f1k='#cc0088', meanReal='grey')			   
quartz()
matplot(days, d, type = 'l', lty = 1, col = colors, 
		lwd = 2, main = 'Performance of Funds (2018)',
	  	xlab = 'Market Days Elapsed', ylab = 'Times Original Value', 
	  	log = 'y', 
	  	xlim = c(0, day + 5), 
	  	ylim = c(min(d[days >= 0, ], meanReal - SP + 1, na.rm=T), 
	  			 max(d[days >= 0, ], meanReal - SP + 1, na.rm=T)))
lines(days, (meanReal - SP) + 1, lwd=1)
lines(days, d[, 'SP'], col = 'yellow', lwd=3)
lines(days, meanReal, col = colors['meanReal'], lwd=1)

abline(h=1, lwd = 2)
abline(v=0, lwd = 2)
abline(v=last - 1)
#abline(v=c(30, 75), lty=4, col=rgb(0, 0, 0, 0.4))
abline(h=seq(0.1, 5.0, 0.1), col=rgb(0, 0, 0, 0.4), lty=2:1)

SP.Sharpe   <- round(Sharpe(SPd),  3)
Fid.Sharpe  <- round(Sharpe(Fidd), 3)
OH.Sharpe   <- round(Sharpe(OHd),  3)
f1k.Sharpe  <- round(Sharpe(f1kd), 3)
real.Sharpe <- round(Sharpe(dailyChange(meanReal)), 3)   

all.n <- length(SPd)
SP.Sharpe1yr   <- round(Sharpe(SPd[last:all.n],  250), 3)
Fid.Sharpe1yr  <- round(Sharpe(Fidd[last:all.n], 250), 3)
OH.Sharpe1yr   <- round(Sharpe(OHd[last:all.n],  250), 3)
f1k.Sharpe1yr  <- round(Sharpe(f1kd[last:all.n], 250), 3)
real.Sharpe1yr <- round(Sharpe(dailyChange(meanReal)[last:all.n], 250), 3)

legend(
  'topleft', 
  title='All data',
  legend=c(
    sprintf('%11s: %+.3f', 'SP500',  SP.Sharpe), 
    sprintf('%15s: %+.3f', 'Fid',    Fid.Sharpe), 
    sprintf('%13s: %+.3f', 'OH',     OH.Sharpe), 
    sprintf('%15s: %+.3f', 'f1k',    f1k.Sharpe), 
    sprintf('%s: %+.3f', 'meanReal', real.Sharpe),
    sprintf('%s', 'diff')),
  lty=1, 
  col=c(colors, 'black'), 
  lwd=c(2, 2, 2, 2, 1, 1), 
  bg=rgb(1, 1, 1, 0.8), 
  cex=0.6)
legend(
  'bottomleft', 
  title='Last year only',
  legend = c(
    sprintf('%11s: %+.3f', 'SP500',  SP.Sharpe1yr), 
    sprintf('%15s: %+.3f', 'Fid',    Fid.Sharpe1yr), 
    sprintf('%13s: %+.3f', 'OH',     OH.Sharpe1yr), 
    sprintf('%15s: %+.3f', 'f1k',    f1k.Sharpe1yr), 
    sprintf('%s: %+.3f', 'meanReal', real.Sharpe1yr),
    sprintf('%s', 'diff')),
  lty=1, 
  col=c(colors, 'black'), 
  lwd=c(2, 2, 2, 2, 2, 1, 1), 
  bg=rgb(1, 1, 1, 0.8), 
  cex=0.6)




#-------------------------------------------------------------
x <- 1:length(totalFunds)
savings.exp.mod <- lm(log(totalFunds) ~ x)
x <- last + seq(250, 3750, by=250)[1:10]
log.y <- coef(savings.exp.mod)[1] + x*coef(savings.exp.mod)[2]
y <- round(exp(log.y))
for (p in y) { cat(p, '\n') }
#-------------------------------------------------------------



#-------------------------------------------------------------


#deposits.in

crypto.frac <- 0.003
(target.crypo.amt <- crypto.frac * totval[length(totval)]) # in: 1100

(fid.amt <- FidVal[length(FidVal)])   # [80533, 258258]
(oh.amt <- OHVal[length(OHVal)])      # [92495, 135516] # E*Trade
(f1k.total <- f1kVal[length(f1kVal)]) # [  847,   7121] [2-param]





#--------------------------------------------------------
# To Save New `df` at the end of each year:
#df.test <- data.frame(
#  SPActual=SPActual, FidIn=FidIn, FidVal=FidVal, f1kIn=f1kIn, f1kVal=f1kVal, 
#  OHIn=OHIn, OHVal=OHVal)
#matplot(df.test, type='l', lty=1, col=1:7)
#legend('topleft', lty=1, col=1:7, legend=names(df.test))
# Backed up previous?
#save(df.test, file='~/Learning/marketModeling/data/myHistoricTest.RData')
#rm(list=ls())
#load('~/Learning/marketModeling/data/myHistoricTest.RData')
#ls()
#matplot(df.test, type='l', lty=1, col=1:7)
#legend('topleft', lty=1, col=1:7, legend=names(df.test))
# OK?
#df <- df.test
#save(df, file='~/Learning/marketModeling/data/myHistoric.RData')
# Delete test and backups