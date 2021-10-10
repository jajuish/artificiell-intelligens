#' learn
#' 
#' Creates a Bayesian network based on the network provided in the question
#' Contains binomial and continuous nodes
learn = function (hist) {
  #### CREATE THE CAUSAL NETWORK ############
  #### (ACCORDING TO PROVIDED STRUCTURE) ####

	# P(Pn)
	#
	# indices:
	# P(VTB = 0) | P(VTB = 1)
	# -----------|----------
	#    [1]     |   [2]
	#
	# P(Pn) = pn[Pn +1]
	pn = c()
	pn[1] = length(hist[hist$Pn == 0, "Pn"]) / 10000
	pn[2] = length(hist[hist$Pn == 1, "Pn"]) / 10000

	# P(VTB)
	#
	# indices:
	# P(VTB = 0) | P(VTB = 1)
	# -----------|----------
	#    [1]     |   [2]
	#
	# P(VTB) = vtb[VTB +1]
	vtb = c()
	vtb[1] = length(hist[hist$VTB == 0, "VTB"]) / 10000
	vtb[2] = length(hist[hist$VTB == 1, "VTB"]) / 10000

	# P(Sm)
	#
	# indices:
	# P(Sm = 0) | P(Sm = 1)
	# ----------|----------
	#    [1]    |   [2]
	#
	# P(Sm) = sm[Sm +1]
	sm = c()
	sm[1] = length(hist[hist$Sm == 0, "Sm"]) / 10000
	sm[2] = length(hist[hist$Sm == 1, "Sm"]) / 10000

	# P(TB | VTB)
	# 
	# indices:
	# VTB | P(TB = 0) | P(TB = 1)
	# ----|-----------|----------
	#  0  | [1,1]     | [2,1]
	#  1  | [1,2]     | [2,2]
	#
	# P(TB | VTB) = tb[TB +1, VTB +1]
	tb = matrix(nrow = 2, ncol = 2)
	tb[1,1] = length(hist[hist$TB == 0 & hist$VTB == 0, "TB"]) / 10000
	tb[1,2] = length(hist[hist$TB == 0 & hist$VTB == 1, "TB"]) / 10000
	tb[2,1] = length(hist[hist$TB == 1 & hist$VTB == 0, "TB"]) / 10000
	tb[2,2] = length(hist[hist$TB == 1 & hist$VTB == 1, "TB"]) / 10000

	# P(LC | Sm)
	#
	# indices:
	# Sm | P(LC = 0) | P(LC = 1)
	#  0 | [1,1]     | [2,1]
	#  1 | [1,2]     | [2,2]
	#
	# P(LC | Sm) = lc[LC +1, Sm +1]
	lc = matrix(nrow = 2, ncol = 2)
	lc[1,1] = length(hist[hist$LC == 0 & hist$Sm == 0, "LC"]) / 10000
	lc[1,2] = length(hist[hist$LC == 0 & hist$Sm == 1, "LC"]) / 10000
	lc[2,1] = length(hist[hist$LC == 1 & hist$Sm == 0, "LC"]) / 10000
	lc[2,2] = length(hist[hist$LC == 1 & hist$Sm == 1, "LC"]) / 10000

	# P(Br | Sm)
	#
	# indices:
	# Sm | P(Br = 0) | P(Br = 1)
	#  0 | [1,1]     | [2,1]
	#  1 | [1,2]     | [2,2]
	#
	# P(Br | Sm) = br[Br +1, Sm +1]
	br = matrix(nrow = 2, ncol = 2)
	br[1,1] = length(hist[hist$Br == 0 & hist$Sm == 0, "Br"]) / 10000
	br[1,2] = length(hist[hist$Br == 0 & hist$Sm == 1, "Br"]) / 10000
	br[2,1] = length(hist[hist$Br == 1 & hist$Sm == 0, "Br"]) / 10000
	br[2,2] = length(hist[hist$Br == 1 & hist$Sm == 1, "Br"]) / 10000

	# P(Te | Pn)
	#
	# indices:
	# Pn | mean(Te) | sd(Te)
	# ---|----------|-------
	#  0 | [1,1]    | [1,2]
	#  2 | [2,1]    | [2,2]
	#
	# P(Te | Pn) = dnorm(VAL, te[Pn +1, 1], te[Pn +1, 2])
	te = matrix(nrow = 2, ncol = 2)
	te[1,1] = mean(hist[hist$Pn == 0, "Te"])
	te[1,2] = sd(hist[hist$Pn == 0, "Te"])
	te[2,1] = mean(hist[hist$Pn == 1, "Te"])
	te[2,2] = sd(hist[hist$Pn == 1, "Te"])

	# P(XR | Pn, TB, LC)
	# 
	# indices:
	# Pn | TB | LC | P(XR = 0) | P(XR = 1)
	# ---|----|----|-----------|-----------
	#  0 | 0  | 0  | [1,1,1,1] | [2,1,1,1]
	#  0 | 0  | 1  | [1,1,1,2] | [2,1,1,2]
	#  0 | 1  | 0  | [1,1,2,1] | [2,1,2,1]
	#  0 | 1  | 1  | [1,1,2,2] | [2,1,2,2]
	#  1 | 0  | 0  | [1,2,1,1] | [2,2,1,1]
	#  1 | 0  | 1  | [1,2,1,2] | [2,2,1,2]
	#  1 | 1  | 0  | [1,2,2,1] | [2,2,2,1]
	#  1 | 1  | 1  | [1,2,2,2] | [2,2,2,2]
	#
	# P(XR | Pn, TB, LC) = xr[XR +1, Pn +1, TB +1, LC +1]
	xr = array(dim = c(2,2,2,2))
	xr[1,1,1,1] = length(hist[hist$XR == 0 & hist$Pn == 0 & hist$TB == 0 & hist$LC == 0, "XR"]) / 10000
	xr[1,1,1,2] = length(hist[hist$XR == 0 & hist$Pn == 0 & hist$TB == 0 & hist$LC == 1, "XR"]) / 10000
	xr[1,1,2,1] = length(hist[hist$XR == 0 & hist$Pn == 0 & hist$TB == 1 & hist$LC == 0, "XR"]) / 10000
	xr[1,1,2,2] = length(hist[hist$XR == 0 & hist$Pn == 0 & hist$TB == 1 & hist$LC == 1, "XR"]) / 10000
	xr[1,2,1,1] = length(hist[hist$XR == 0 & hist$Pn == 1 & hist$TB == 0 & hist$LC == 0, "XR"]) / 10000
	xr[1,2,1,2] = length(hist[hist$XR == 0 & hist$Pn == 1 & hist$TB == 0 & hist$LC == 1, "XR"]) / 10000
	xr[1,2,2,1] = length(hist[hist$XR == 0 & hist$Pn == 1 & hist$TB == 1 & hist$LC == 0, "XR"]) / 10000
	xr[1,2,2,2] = length(hist[hist$XR == 0 & hist$Pn == 1 & hist$TB == 1 & hist$LC == 1, "XR"]) / 10000
	xr[2,1,1,1] = length(hist[hist$XR == 1 & hist$Pn == 0 & hist$TB == 0 & hist$LC == 0, "XR"]) / 10000
	xr[2,1,1,2] = length(hist[hist$XR == 1 & hist$Pn == 0 & hist$TB == 0 & hist$LC == 1, "XR"]) / 10000
	xr[2,1,2,1] = length(hist[hist$XR == 1 & hist$Pn == 0 & hist$TB == 1 & hist$LC == 0, "XR"]) / 10000
	xr[2,1,2,2] = length(hist[hist$XR == 1 & hist$Pn == 0 & hist$TB == 1 & hist$LC == 1, "XR"]) / 10000
	xr[2,2,1,1] = length(hist[hist$XR == 1 & hist$Pn == 1 & hist$TB == 0 & hist$LC == 0, "XR"]) / 10000
	xr[2,2,1,2] = length(hist[hist$XR == 1 & hist$Pn == 1 & hist$TB == 0 & hist$LC == 1, "XR"]) / 10000
	xr[2,2,2,1] = length(hist[hist$XR == 1 & hist$Pn == 1 & hist$TB == 1 & hist$LC == 0, "XR"]) / 10000
	xr[2,2,2,2] = length(hist[hist$XR == 1 & hist$Pn == 1 & hist$TB == 1 & hist$LC == 1, "XR"]) / 10000

	# P(Dy | LC, Br)
	# 
	# indices:
	#  LC | Br | P(Dy = 0) | P(Dy = 1)
	# ----|----|-----------|----------
	#  0  | 0  | [1,1,1]   | [2,1,1]
	#  0  | 1  | [1,1,2]   | [2,1,2]
	#  1  | 0  | [1,2,1]   | [2,2,1]
	#  1  | 1  | [1,2,2]   | [2,2,2]
	# 
	# P(Dy | LC, Br) = dy[Dy +1, LC +1, Br +1]
	dy = array(dim = c(2,2,2))
	dy[1,1,1] = length(hist[hist$Dy == 0 & hist$LC == 0 & hist$Br == 0, "Dy"]) / 10000
	dy[1,1,2] = length(hist[hist$Dy == 0 & hist$LC == 0 & hist$Br == 1, "Dy"]) / 10000
	dy[1,2,1] = length(hist[hist$Dy == 0 & hist$LC == 1 & hist$Br == 0, "Dy"]) / 10000
	dy[1,2,2] = length(hist[hist$Dy == 0 & hist$LC == 1 & hist$Br == 1, "Dy"]) / 10000
	dy[2,1,1] = length(hist[hist$Dy == 1 & hist$LC == 0 & hist$Br == 0, "Dy"]) / 10000
	dy[2,1,2] = length(hist[hist$Dy == 1 & hist$LC == 0 & hist$Br == 1, "Dy"]) / 10000
	dy[2,2,1] = length(hist[hist$Dy == 1 & hist$LC == 1 & hist$Br == 0, "Dy"]) / 10000
	dy[2,2,2] = length(hist[hist$Dy == 1 & hist$LC == 1 & hist$Br == 1, "Dy"]) / 10000

	network = list(
		Pn = pn,
		Te = te,
		VTB = vtb,
		TB = tb,
		Sm = sm,
		LC = lc,
		Br = br,
		XR = xr,
		Dy = dy
	)

	return (network)
}

#    Pn       Te VTB TB Sm LC Br XR Dy
# 1  NA 38.73447   0 NA  1 NA NA  1  0
# 2  NA 41.18788   0 NA  0 NA NA  1  0
# 3  NA 39.23329   1 NA  1 NA NA  1  0
# 4  NA 39.16066   1 NA  1 NA NA  0  0
# 5  NA 39.13593   0 NA  0 NA NA  1  0
# 6  NA 38.64055   0 NA  1 NA NA  1  1
# 7  NA 40.61668   0 NA  0 NA NA  0  0
# 8  NA 38.95874   0 NA  0 NA NA  0  0
# 9  NA 38.65676   0 NA  1 NA NA  0  0
# 10 NA 40.70658   1 NA  1 NA NA  1  1

diagnose = function (network, cases) {
	print(network)
	print(cases)
	randomValues = round(runif(10, 0, 1))
	currentIndex = 1

	for (i in 1:length(cases)) {
		currentCase = cases[i]

		#### ASSIGNED VALUES
		#### assign random values to Pn, TB, LC, Br
		pn = randomValues[currentIndex]
		currentIndex = currentIndex + 1
		tb = randomValues[currentIndex]
		currentIndex = currentIndex + 1
		lc = randomValues[currentIndex]
		currentIndex = currentIndex + 1
		br = randomValues[currentIndex]
		currentIndex = currentIndex + 1

		#### PROPOSED VALUES
		#### propose inverse values for the above
		pn_ = 1-pn
		tb_ = 1-tb
		lc_ = 1-lc
		br_ = 1-br

		#### CALCULATE p_old
		#### = P(Pn) * P(VTB) * P(Sm) * P(TB | VTB) * P(LC | Sm) * P(Br | Sm) * P(Te | Pn) * P(XR | Pn, TB, LC) * P(Dy | LC, Br)
		# p_old =
		# 	network$Pn[pn +1] * # P(Pn)
		# 	network$VTB[currentCase$VTB +1] * # P(VTB)
		# 	network$Sm[currentCase$Sm +1] * # P(Sm)
		# 	network$TB[currentCase$VTB +1, tb +1] * # P(TB | VTB)
		# 	network$LC[currentCase$Sm +1, lc +1] * # P(LC | Sm)
		# 	network$Br[currentCase$Sm +1, br +1] * # P(Br | Sm)
		# 	 * # P(Te | Pn)
		# 	 * # P(XR | Pn, TB, LC)
		# 	 * # P(Dy | LC, Br)

	}
}