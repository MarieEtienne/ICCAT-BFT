## ------------------------------------------------------------------------- ##
## CONTROL FILE TEMPLATE                                                     ##
## ------------------------------------------------------------------------- ##
##
##
## ------------------------------------------------------------------------- ##
## CONTROLS FOR LEADING PARAMETERS                                           ##
##  Prior descriptions:                                                      ##
##                      -0 uniform      (0,0)                                ##
##                      -1 normal       (p1=mu,p2=sig)                       ##
##                      -2 lognormal    (p1=log(mu),p2=sig)                  ##
##                      -3 beta         (p1=alpha,p2=beta)                   ##
##                      -4 gamma        (p1=alpha,p2=beta)                   ##
## ------------------------------------------------------------------------- ##
## npar
7
## ival         lb      ub      phz     prior   p1      p2      #parameter   ##
3	-5	15	4	0	-5	15 	#log_ro/msy 
0.8	0.2	1	4	3	3	2 	#steepness h 
-2	-5	0	-1	1	-1.469	0.05 	#log_m, natural mortality
6	-5	30	1	0	-5	30 	#log_Rbar, average recruitment
8	-5	30	1	0	-5	30 	#log_Rinit, initial recruitment
0.3	0.001	0.999	3	3	12	52.8 	#proportion of variance for observation process
0.2	0.001	10	3	4	1e-04	1e-04 	# total variance
## SELECTIVITY PARAMETERS Columns for gear                                   ##
## OPTIONS FOR SELECTIVITY (isel_type):                                      ##
##      1) logistic selectivity parameters                                   ##
##      2) selectivity coefficients                                          ##
##      3) a constant cubic spline with age-nodes                            ##
##      4) a time varying cubic spline with age-nodes                        ##
##      5) a time varying bicubic spline with age & year nodes.              ##
##      6) fixed logistic (set isel_type=6, and estimation phase to -1)      ##
##      7) logistic function of body weight.                                 ##
##      8) logistic with weight deviations (3 parameters)                    ##
##      11) logistic selectivity with 2 parameters based on mean length      ##
##      12) length-based selectivity coefficients with spline interpolation  ##
##      sig=0.05 0.10 0.15 0.20 0.30 0.40 0.50                               ##
##      wt =200. 50.0 22.2 12.5 5.56 3.12 2.00                               ##
## ------------------------------------------------------------------------- ##
3	1 	# 1  -selectivity type ivector(isel_type) for gear
6	6 	# 2  -Age/length at 50% selectivity (logistic)
1	1 	# 3  -STD at 50% selectivity (logistic)
5	5 	# 4  -No. of age nodes for each gear (0=ignore)
0	0 	# 5  -No. of year nodes for 2d spline(0=ignore)
2	2 	# 6  -Phase of estimation (-1 for fixed)
15	15 	# 7  -Penalty wt for 2nd differences w=1/(2*sig^2)
0	0 	# 8  -Penalty wt for dome-shaped w=1/(2*sig^2)
1	1 	# 9  -Penalty wt for time-varying selectivity
1	1 	# 10 -n_sel_blocks (number of selex blocks)
## ------------------------------------------------------------------------- ##
1950
1950
##
##
##
## ------------------------------------------------------------------------- ##
## PRIORS FOR SURVEY Q                                                       ##
## Prior type:                                                               ##
##			0 - uninformative prior                                         ##
##			1 - normal prior density for log(q)                             ##
##			2 - random walk in q                                            ##
## ------------------------------------------------------------------------- ##
1 	# -number of surveys (nits)
0 	# -prior type (see legend above)
0 	# -prior log(mean)
0 	# -prior sd
## ------------------------------------------------------------------------- ##
##
## ------------------------------------------------------------------------- ##
## OTHER MISCELANEOUS CONTROLS                     ##
##
## ------------------------------------------------------------------------- ##
	 0 	# 1  -verbose ADMB output (0=off, 1=on)
	 1 	# 2  -recruitment model (1=beverton-holt, 2=ricker)
	155.866419354839	# 3  -std in observed catches in first phase.
	77.9332096774194	# 4  -std in observed catches in last phase.
	0	# 5  -Assume unfished in first year (0=FALSE, 1=TRUE)
	0.00	# 6  -Minimum proportion to consider in age-proportions for dmvlogistic
	0.30	# 7  -Mean fishing mortality for regularizing the estimates of Ft
	0.3	# 8  -std in mean fishing mortality in first phase
	2.00	# 9  -std in mean fishing mortality in last phase
	-1	# 10 -phase for estimating m_deviations (use -1 to turn off mdevs)
	0.1	# 11 -std in deviations for natural mortality
	12	# 12 -number of estimated nodes for deviations in natural mortality
	0.50	# 13 -fraction of total mortality that takes place prior to spawning
	1	# 14 -switch for age-composition likelihood (1=dmvlogistic,2=dmultinom)
	0	# 15 -switch for IFD distribution in selectivity simulations			 ##
##
## ------------------------------------------------------------------------- ##
## MARKER FOR END OF CONTROL FILE (eofc)                                     ##
## ------------------------------------------------------------------------- ##
999
