 #---------#---------#---------#---------#---------#---------#---------#---------
load('~//Learning/marketModeling/data/myHistoric.RData')
head(df)
tail(df)

dailyChange <- function(x) {
  (x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)]
}

m <- dim(df)[1]
last <- m

# Buy when low:
# AAPL, AMZN, CRM, DIS, GOOG, INTG, MA, ROL, TSLA, MSFT, ZM

wlsh.osc <- c(
  202,198,196,201,207,209,211,213,213,209,208,211,213,214,215,214,201,192,175,
  166,162,159,161,163,169,174,177,180,184,185,185,181,177,169,159,154,137,120,
  113,102, 84, 61, 45, 28, 19, 12, 12, 11, 13, 13, 13,  4, -3, -8,-19,-33,-44,
  -48,-54,-60,-64,-62,-55,-49,-45,-38,-30,-23,-16,-11, -2,  7, 12, 11, 15, 15,
   19, 25, 29, 32, 37, 36, 36, 31, 26, 22, 22, 15,  5,-15,-30,-36,-43,-53,-65,
  -71,-77,-78,-82,-83,-83,-84,-84,-84,-87,-85,-84,-82,-82,-80,-77,-75,-74,-76,
  -78,-87,-89,-88,-88,-85,-82,-78,-74.661)

SPActual <- c(df$SPActual,
    3701,  3727,  3748,  3804,  3825,  3800,  3801,  3810,  3796,  3768,  3799,
    3852,  3853,  3841,  3855,  3850,  3751,  3787,  3714,  3774,  3826,  3830,
    3872,  3887,  3916,  3911,  3910,  3916,  3935,  3933,  3931,  3914,  3907,
    3877,  3881,  3925,  3829,  3811,  3902,  3870,  3820,  3768,  3842,  3821,
    3875,  3899,  3939,  3943,  3969,  3963,  3974,  3915,  3913,  3941,  3911,
    3889,  3910,  3975,  3971,  3959,  3973,  4020,  4078,  4074,  4080,  4097,
    4129,  4128,  4142,  4125,  4170,  4185,  4163,  4135,  4173,  4135,  4180,
    4188,  4187,  4183,  4211,  4181,  4193,  4165,  4168,  4202,  4233,  4188,
    4152,  4063,  4113,  4174,  4163,  4128,  4116,  4159,  4156,  4197,  4188,
    4196,  4201,  4204,  4202,  4208,  4193,  4230,  4227,  4227,  4220,  4239,
    4247,  4255,  4247,  4224,  4222,  4166,  4225,  4246,  4242,  4266,  4281,
    4291,  4292)
SP <- SPActual / SPActual[last]
SPd <- dailyChange(SP)

FidIn <- c(df$FidIn,
   53922, 53922, 53922, 53922, 53922, 53922, 53922, 53922, 53922, 53922, 54237,
   54237, 54237, 54237, 54237, 54237, 54237, 54237, 54237, 54237, 54374, 54374,
   54374, 54374, 54374, 54374, 54374, 54374, 54374, 54374, 54374, 54374, 54374,
   54374, 54374, 54374, 54374, 54374, 54686, 54686, 54686, 54686, 54686, 54686,
   54686, 54686, 54686, 54686, 55120, 55120, 55120, 55120, 55120, 55120, 55120,
   55120, 55120, 55120, 55120, 55120, 55120, 57134, 57134, 57134, 57134, 57134,
   57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134,
   57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134,
   57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134,
   57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134,
   57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134,
   57134, 57134)
  
FidVal <- c(df$FidVal,
  100775,101806,100719,104282,106134,105570,107538,107355,107940,106940,109459,
  110997,110526,111271,111543,110262,107484,109536,108855,111502,113873,113818,
  115499,117322,117875,118172,117692,118556,119639,117678,115695,114613,116176,
  111259,109783,109466,104910,106892,110443,108295,103893,100791,100385, 97933,
  101389,101050,103550,102892,104611,103381,103419,100435,101666,102424,101883,
   98921, 98434, 98951, 97796, 98423,100298,105545,105223,106905,106282,107822,
  107772,107608,109458,108528,109640,107777,106413,105013,106624,106684,107668,
  109233,108900,108318,106900,105662,104119,102569,101520, 98999,100292, 97938,
   99562, 97231, 96192, 98256, 97922, 98484, 98465,100544,100172,101235,101235,
  102711,102831,103083,102590,102795,100900,101963,103374,103821,103530,104707,
  105446,106854,104991,105145,107587,108313,108203,109752,110781,112337,112586,
  114655,115227)
Fid <- FidVal / FidIn
Fid <- Fid / Fid[last]
Fidd <- dailyChange(Fid)

f1kIn <- c(df$f1kIn,
   81977, 81977, 81977, 81977, 81977, 81977, 81977, 81977, 81977, 81977, 81977,
   82670, 82670, 82670, 82670, 82670, 82670, 82670, 82670, 82670, 83373, 83373,
   83373, 83373, 83373, 83373, 83373, 83373, 83373, 84026, 84026, 84026, 84026,
   84026, 84026, 84026, 84026, 84026, 84754, 84754, 84754, 84754, 84754, 84754,
   84754, 84754, 84754, 84754, 84754, 85475, 85475, 85475, 85475, 85475, 85475,
   85475, 85475, 85475, 85475, 85475, 85475, 86214, 86214, 86214, 86214, 86214,
   86214, 86214, 86214, 86214, 86214, 86929, 86929, 86929, 86929, 86929, 86929,
   86929, 86929, 86929, 86929, 86929, 86929, 87634, 87634, 87634, 87634, 87634,
   87634, 87634, 87634, 88364, 88364, 88364, 88364, 88364, 88364, 88364, 88364,
   88364, 88364, 88364, 89079, 89079, 89079, 89079, 89079, 89079, 89079, 89079,
   89079, 89079, 89079, 89789, 89789, 89789, 89789, 89789, 89789, 89789, 89789,
   89789, 89789) #
f1kVal <- c(df$f1kVal,
  117777,116744,117728,117731,120057,121168,120278,120935,120334,120334,120554,
  121979,123642,123696,123631,123631,122528,118811,120519,119737,122325,124491,
  123724,125251,126357,127527,127656,127631,128367,130090,128945,128945,128166,
  128674,126348,126059,126840,123391,125056,127839,126344,123241,120600,121952,
  120098,123207,123199,125720,125586,127255,127019,127019,124933,125398,125724,
  122721,122721,123290,124469,123297,123677,125808,125808,127974,127974,127519,
  128428,128743,128549,128853,128815,130977,130105,130105,128440,129968,128929,
  130954,131643,131678,131899,131900,131727,131936,130893,130893,131328,132546,
  130523,127212,128260,131152,131152,131062,129712,130546,130623,130623,131498,
  130964,131783,132143,133927,133629,133117,133728,133728,134049,134330,133827,
  133788,134324,134133,135073,134071,134072,132912,134418,135166,136019,136019,
  135863,135993)
payment <- 1018.67
n <- length(f1kIn)
payment * f1kIn[n] / f1kVal[n] + f1kIn[n]

f1k <- f1kVal / f1kIn
f1k <- f1k / f1k[last]
f1kd <- dailyChange(f1k)

OHIn <- c(df$OHIn,
   46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021,
   46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021,
   46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021,
   46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021,
   46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 46021, 39307, 39307,
   39307, 39307, 39307, 39307, 39307, 39307, 39443, 39443, 39443, 39443, 39443,
   39443, 39443, 39443, 39443, 39443, 39694, 39694, 39694, 39694, 39694, 39694,
   39694, 39694, 39694, 39694, 39694, 39936, 39936, 39936, 39936, 39936, 39936,
   39936, 39936, 40200, 40200, 40531, 40531, 40531, 40531, 40531, 40531, 40531,
   40531, 40531, 40881, 40881, 40881, 40881, 40881, 40881, 40881, 40881, 40881,
   40881, 40881, 40881, 41225, 41225, 41225, 41225, 41225, 41225, 41225, 41225,
   41225, 41225) #
OHVal <- c(df$OHVal, 
   99561,100876, 99287,103014,104849,104471,107010,107093,107445,106404,108339,
  109868,109448,110351,110633,109193,106228,107902,107523,109218,111376,111362,
  112837,114507,115070,115961,115690,116960,117369,116242,114644,113656,115007,
  110730,109404,109333,105306,107034,109264,107765,103819,101054,100806, 99134,
  101622,101246,103242,102694,103325,102648,102961,100355,101452, 87037, 86693,
   84611, 84231, 84590, 83743, 84206, 85650, 87094, 87041, 88052, 87532, 88805,
   88826, 89042, 90562, 89641, 90900, 90612, 89286, 88477, 89308, 89204, 90453,
   91960, 91652, 91518, 90168, 88880, 87832, 86221, 85114, 82896, 84726, 82617,
   83738, 82515, 81775, 83143, 83630, 84050, 84053, 85876, 85262, 85957, 86149,
   87060, 87227, 88219, 87824, 87963, 86472, 87322, 88436, 88765, 88638, 89436,
   89911, 90798, 89601, 89870, 92060, 92609, 92486, 93600, 94295, 95402, 95659,
   96973, 97244) # 
OH <- OHVal / OHIn
OH <- OH / OH[last]
OHd <- dailyChange(OH)
totalFunds <- FidVal + f1kVal + OHVal


# Actual-Stratego 
m1 <- (c(
  100000, 97573, 99368, 97344,102160,104572,104303,107511,107529,107726,106392,
  108987,110861,110273,111413,111413,107686,103945,106512,105521,108375,111209,
  111242,113331,115778,116661,117908,117253,118611,119394,118349,116172,115151,
  117336,110919,108642,108221,101922,104631,106850,105531, 98774, 94278, 93620,
   89596, 95521, 94567, 99464, 98224, 99462, 98211, 98299, 93394, 95287, 96724,
   95632, 90890, 90136, 90963, 89031, 90012, 92921, 95563, 94804, 97205, 96249,
   99093, 99263, 99461,102175,100359,102524,100728, 98299, 96499, 97955, 97635,
   99445,101557,101175,100755, 98934, 97247, 94660, 92309, 90825, 86991, 89296,
   85938, 87621, 84396, 83089, 85864, 85184, 85641, 85658, 87213, 87167, 87968,
   88133, 89063, 89234, 89379, 88908, 88987, 87999, 89498, 90228, 90561, 90437,
   91512, 92425, 93229, 91846, 92090, 91871, 92167, 94523, 95550, 95770, 96989,
   97078, 98109, 98347)
  / 100000)
qp.start <- 92
m1.name <- 'BuyNHold'
  
# Nug2021 (actual)
m2 <- (c(
  100000, 97571, 99367, 97325,102154,104574,104162,107106,107325,107519,106447,
  108792,110831,110256,111484,111420,109139,105790,108364,107381,110151,113367,
  113334,115738,118125,118974,120009,119433,121103,121666,120001,117765,116816,
  118627,112556,110721,110641,104571,106850,110682,107334,100703, 95943, 95136,
   91019, 97128, 96284,101381, 99949,101442, 99859,100041, 95224, 97342, 98745,
   97643, 92530, 91641, 92551, 90620, 91709, 94903, 97590, 96998, 99279, 98182,
  101005,101035,101109,103532,102062,103865,102277,100076, 98193, 99834, 99686,
  101767,103997,103709,103182,101141, 99340, 96811, 94493, 92931, 89492, 91370,
   85938, 89987, 86595, 85053, 88241, 87825, 88785, 88814, 91728, 91133, 92532,
   92843, 94430, 94644, 95034, 94449, 94725, 92004, 93856, 95517, 96166, 95824,
   97190, 97977, 99757, 97579, 97778, 98052, 98987,101811,103625,104726,106536,
  107042,109327,109750)
  / 100000) 
m2.name <- 'Ideal (Nug2021)' # when up -> Cash in low; when down: even balance

# buy-lowball 
# Weekly: 20% from each of the top 3 to the bottom 3
# Top 3 to bottom 3
m3.raw <- c(
  100000, 97712, 99493, 97488,102309,104673,104338,107501,107577,107851,106448,
  108994,110922,110438,111685,111678,108828,105126,107645,106697,109456,112506,
  112449,114619,117096,118002,119308,118543,120265,121028,119900,117723,116771,
  118782,112309,110228,109876,103755,106481,110395,107175,100458, 96088, 95589,
   91908, 97826, 96850,101630,100189,101468,100212,100335, 95297, 97287, 98863,
   97873, 93262, 92433, 93173, 91066, 91893, 94954, 97518, 96961, 99007, 97853,
  100445,100434,100861,103783,102170,104265,102353, 99812, 98146, 99764, 99675,
  101165,103740,103193,102634,100459, 99043, 96480, 93979, 92666, 89398, 91632,
   87835, 89926, 86949, 85223, 88073, 87336, 88406, 88261, 91166, 90658, 92046,
   92339, 94255, 94367, 94643, 93826, 94075, 91585, 92654, 94781, 95616, 95273,
   96714, 97565, 99400, 96860, 97216, 95817, 96543,100920,102725,103828,105867,
  106606,108870,109482)
m3 <- m3.raw / 100000
m3.name <- 'CashInLow'
#len <- length(m3)
#(last.week.gains <- m3.raw[len] - m3.raw[len - 7])
#(per <- last.week.gains / 3)

# buy-highball
# Weekly: 20% from each of the bottom 3 to the top 3
# Bottom 3 to top 3
m4.raw <- c(
  100000, 97671, 99458, 97401,102290,104670,104323,107402,107403,107416,106014,
  108674,110578,110035,111144,111313,107662,103882,106295,105412,108130,110785,
  110876,112882,115455,116323,117607,117014,118612,119406,118295,116177,115030,
  117039,110632,108534,108166,101915,104560,108682,105631, 98890, 94259, 93589,
   89422, 95160, 94351, 99197, 98034, 99237, 97990, 97975, 93224, 95098, 96383,
   95257, 90614, 89940, 90889, 89058, 90090, 92924, 95628, 94949, 97379, 96423,
   99224, 99445, 99579,102102,100347,102514,100835, 98601, 96772, 98215, 97999,
   99886,101809,101472,100901, 99378, 97640, 95233, 92807, 91358, 87567, 89822,
   86754, 88149, 84779, 83689, 86471, 85928, 86772, 86644, 89309, 88736, 89963,
   90145, 91012, 91026, 91244, 90699, 90538, 88676, 90008, 91447, 92136, 91754,
   92975, 93670, 95118, 93345, 93542, 93627, 94420, 97047, 98636, 99325,100331,
  100601,102336,102610)
m4 <- m4.raw / 100000
m4.name <- 'CashInHigh'
(last.week.gains <- m4.raw[len] - m4.raw[len - 7])
(per <- last.week.gains / 3)
  
# 30/70 
# Whenever a stock moves above 70, take out 20% to reserve
# Whenever a stock falls below 30 add 10% of reserves 
# (or whatever fraction if > 4)
m5 <- (c(
  100000, 97738, 99501, 97535,102285,104624,104400,107473,107542,107573,106249,
  108679,110549,110105,111212,111309,108494,105085,107334,106608,108936,111588,
  111659,113560,115826,116578,117670,117018,118445,119126,118142,116229,115322,
  117209,111542,109753,109497,104072,106548,110232,107514,101300, 97189, 96600,
   92523, 98668, 97430,102655,101196,102557,101293,101376, 96342, 98483, 99357,
   99076, 93960, 93045, 93951, 91929, 92860, 96009, 98772, 97827,100188, 98981,
  101860,101915,102244,105898,104108,106308,104317,101684, 99957,101404,101333,
  103144,105419,105011,104243,102287,100635, 97849, 95417, 93884, 89730, 92105,
   88915, 90830, 87571, 86124, 89145, 88408, 89526, 89502, 92519, 92003, 93326,
   93616, 95221, 95323, 95454, 94630, 94842, 92428, 93530, 95659, 96598, 96127,
   97430, 98306,100035, 97804, 98123, 97627, 98389,101616,103610,104598,106285,
  106723,108987,109407)
  / 100000)
m5.name <- '30/70'  


m6 <- (c(
  100000,100000,100000,100000,100000,100000,100000,100000,100000,100000,100000,
  100000,100000,100000,100000,100000,100000,100000,100000,100000,100039,100061,
  100456,101872,103664,104319,104605,104362,104698,104954,103219,101540,100857,
  102589, 98833, 98671, 98425, 94655, 95673, 96701, 96698, 96682, 96674, 96684,
   96685, 96697, 96702, 96708, 96708, 96718, 96712, 96707, 96691, 96690, 96687,
   96682, 96682, 96688, 96693, 96296, 95328, 96348, 97386, 98030, 98029, 98009,
   98138, 98260, 99153,101256, 99424,101820,100008, 97867, 96671, 97724, 97539,
   98858,100537,100197, 99992, 98022, 96837, 95064, 92529, 91829, 88989, 90672,
   88732, 89022, 87537, 87190, 88100, 86900, 87315, 87133, 88650, 88171, 88606,
   88495, 90782, 90313, 90375, 89937, 90003, 88585, 89913, 91512, 92156, 91902,
   93374, 94331, 95935, 93848, 94285, 94097, 95050, 98092, 99988,101308,103159,
  103654,105851,106145)
  / 100000)
m6.name <- 'rsi each'  

m7 <- (c(
  100000,100000,100000,100000,100000,100000,100000,100000,100000,100000,100000,
  100000,100000,100000,100000,100000,100000,100000,100000,100000,100396,101084,
  101156,101669,102140,102243,102939,102629,103844,104154,103338,101998,101447,
  102663, 99844, 99141, 99059, 96672, 97645, 98587, 98532, 98508, 98443, 98437,
   98364, 98470, 97449, 98546, 98541, 98546, 98529, 98522, 98394, 98462, 98474,
   98464, 98370, 98390, 98406, 98380, 98410, 98394, 98500, 97993, 98750, 98434,
   99343, 99392, 99696,101501,100210,101701,100440, 98863, 97496, 98685, 98666,
  100199,101844,101554,101112, 99668, 98235, 96408, 94499, 93300, 90796, 92302,
   90406, 91001, 89893, 89336, 90399, 90210, 90454, 90457, 91235, 91100, 91602,
   91716, 92267, 92315, 92440, 91788, 91843, 89947, 91103, 92435, 92916, 92640,
   93742, 94538, 95871, 94232, 94394, 94499, 95294, 97430, 98888, 99862,101191,
  101495,103226,103557)
  / 100000)
m7.name <- 'rsi overall'  


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
plot(new.days,
     m1, 
     type='l', 
     xlim=range(days),
     ylim=range(c(SP,  tot, 1 + tot - SP, mine, m1,  m2, m3, m4, m5, m6, m7)), 
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
lines(new.days, m6 + 0.07, lwd=3, col='grey')
lines(new.days, m7 + 0.07, lwd=3, col='magenta')
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
  
quartz()  
plot(new.days,
     m1, 
     type='l', 
     xlim=range(days[days >= 0]),
     ylim=c(0.8,
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
lines(new.days, m6 + 0.07, lwd=3, col='grey')
lines(new.days, m7 + 0.07, lwd=3, col='magenta')
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
lines(cumprod(c(1, m6.ch[down.days])) - 0.09, col='grey', lwd=2)  
lines(cumprod(c(1, m7.ch[down.days])) - 0.09, col='magenta', lwd=2)  
legend('topright', 
       lty=1,
       lwd=2, 
       col=c('yellow', 'black', 'coral', 'blue', 'purple', 'cyan', 'grey', 
             'magenta'),
       legend=c('SP', m1.name, m2.name, m3.name, m4.name, m5.name, m6.name, 
                m7.name))
quartz()
plot(cumprod(c(1, sp.ch[up.days])), 
     type='l', 
     col='yellow', 
     ylim=range(cumprod(c(1, m1.ch[up.days])), 
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
lines(cumprod(c(1, m6.ch[up.days])) + 0.17, col='grey', lwd=2)  
lines(cumprod(c(1, m7.ch[up.days])) + 0.17, col='magenta', lwd=2)  
legend('topleft', 
       lty=1,
       lwd=2, 
       col=c('yellow', 'black', 'coral', 'blue', 'purple', 'cyan', 'grey', 
             'magenta'),
       legend=c('SP', m1.name, m2.name, m3.name, m4.name, m5.name, m6.name, 
                m7.name))

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

lines(FidVal, col=4)
lines(x, exp.mod(FidVal, x), col=rgb(0, 0, 1, 1))

lines(f1kVal, col=5)
lines(x, exp.mod(f1kVal, x), col=rgb(0, 1, 1, 1))

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
       col=c(4, 5, 6, 1),
       legend=c('Fidelity', '401(k)', 'E*Trade', 'Total'),
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
n <- length(wlsh.osc)
ds <- abs(diff(wlsh.osc))
(thresh <- quantile(ds, p=0.05))
dif <- wlsh.osc[n] - wlsh.osc[n - 1]
if (dif == 0) { 
  cat('No change. Handle this case') 
} else { change <- ifelse(dif > 0, 'up', 'down') }
powET  <- ifelse(change == 'up', 2, 0)
powFid <- ifelse(change == 'up', 1, -0.1)
below.thresh <- abs(dif) <= thresh
	
	
cat(
  sprintf(
    'Direction is: %s %s\n  Set ET exp to %.2f and Fid exp to %.2f', 
    change,
    ifelse(below.thresh, sprintf('(but below threshold: %.4f)', thresh), ''),
    powET, 
    powFid))

CASH_OUT
CASH_IN

(cash.out.fid <- CASH_OUT * FidVal[length(FidVal)])
cash.out.fid <- 27842 # Update ONLY when new threshold crossed
(1 - CASH_IN) * cash.out.fid 

(cash.out.et <- CASH_OUT * OHVal[length(OHVal)])
cash.out.et <- 23193
(1 - CASH_IN) * cash.out.et

# weight by history length
f <- sqrt
w <- c(f(50), # nya 50
       f(35), # nya 35
       f(20), #     20...
       f(10), 
       f(5),
       f(3),  # nya 3
       f(50), # sp 50
       f(35), # sp 35
       f(20), #    20...
       f(10), 
       f(5),
       f(3))  # sp 3
(w <- w / sum(w))

# 0.0001 >= 0.00005 = 5e-5
# Mon
wil3 <- c(1, 0.9994, 1)
nas3 <- c(1, 1, 1)

# Tue
wil5 <- c(0.9812, 1, 0.9879)
nas5 <- c(1, 1, 1)

# Wed    
wil10 <- c(0.9718, 0.8530, 0.0185)
nas10 <- c(1, 1, 0.9907)

# Thu
wil20 <- c(0.0251, 0.0001, 0)
nas20 <- c(1, 1, 0.9953)

# Fri
nya35 <- c(1, 0.9881, 0)
nas35 <- c(0.9988, 0.9905, 1)

# Sat
nya50 <- c(1, 1, 0.9912)
nas50 <- c(0.9999, 1, 0.0107)



# Change must be ≥ 10%
# UPDATE: Fridays or when a percent-in value has changed
# Fid (buy and hold form Partnership Portfolio) [4-param]
(fid.amt <- FidVal[length(FidVal)]) # [96192, 119639]
(fid.pct.in <- w %*% c(nya50[1], nya35[1], wil20[1], wil10[1], wil5[1], wil3[1],
                       nas50[1], nas35[1], nas20[1], nas10[1], nas5[1], nas3[1]))
(0.9085 - 0.9076) # 0.09
fid.amt.in <- fid.amt * fid.pct.in
(fid.res <- fid.amt - fid.amt.in)

# OH (e*trade) - traditional strategy [3-param]
(oh.amt <- OHVal[length(OHVal)]) # [81775, 117369]
(oh.pct.in <- w %*% c(nya50[2], nya35[2],  wil20[2], wil10[2], wil5[2], wil3[2],
                      nas50[2], nas35[2],  nas20[2], nas10[2], nas5[2], nas3[2])) 
(0.8970 - 0.8955) # 0.15
oh.amt.in <- oh.amt * oh.pct.in
(oh.res <- oh.amt - oh.amt.in)

# For 401(k): # Next: 12 Jul (10% Change Required)
(f1k.total <- f1kVal[length(f1kVal)]) # [118811, 136019] [2-param]
(f1k.pct.in <- w %*% c(nya50[3], nya35[3], wil20[3], wil10[3], wil5[3], wil3[3],
                       nas50[3], nas35[3], nas20[3], nas10[3], nas5[3], nas3[3])) 
(0.5806 - 0.6434) # 6.28
(f1k.res <- 1 - f1k.pct.in)
#--------------------------------------------------------

x <- c(1, 2, 4)
x <- x / sum(x)
x * c(f1k.pct.in)

#--------------------------------------------------------
# To Save New `df` at the end of each year:
df.test <- data.frame(
  SPActual=SPActual, FidIn=FidIn, FidVal=FidVal, f1kIn=f1kIn, f1kVal=f1kVal, 
  OHIn=OHIn, OHVal=OHVal)
matplot(df.test, type='l', lty=1, col=1:7)
legend('topleft', lty=1, col=1:7, legend=names(df.test))
# Backed up previous?
save(df.test, file='~/Desktop/marketStudies/data/myHistoricTest.RData')
rm(list=ls())
load('~/Desktop/marketStudies/data/myHistoricTest.RData')
ls()
matplot(df.test, type='l', lty=1, col=1:7)
legend('topleft', lty=1, col=1:7, legend=names(df.test))
# OK?
df <- df.test
save(df, file='~/Desktop/marketStudies/data/myHistoric.RData')
# Delete test and backups