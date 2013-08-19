## Makefile for running iscam
## Targets: 
##		all:   -copy executable and run the model with DAT & ARG
##		run:   -copy executable and force a run
##		mcmc:  -copy executable and run in mcmc mode and mceval
##		retro: -copy executable and run  retrospective analysis
EXEC=iscam
REXEC=Rscript
prefix=$(ISCAM_HOME)
DAT=
CTL=ICCAT
ARG='bfte/2012/vpa/simple/low/'
ARGSIM=0
MCFLAG=-mcmc 10000 -mcsave 100 -nosdmcmc
NR=4

ifdef DEBUG
  DIST=$(prefix)/debug/iscam
else
  DIST=$(prefix)/release/iscam
endif




all: $(EXEC)

$(EXEC): $(REXEC) $(DIST)  
	cd $(ARG) && $(MAKE)

$(DIST):
	cp $(DIST) $(ARG)/$@

$(REXEC): $(DIST)
	$(REXEC) sources/setISCAMFiles.R --args Inputs/$(ARG)
 
mcmc:  $(EXEC) 
	cd $(ARG) && $(MAKE) mcmc

sim:  $(EXEC) 
	cd $(ARG) && $(MAKE) sim $(ARGSIM)




clean: 
