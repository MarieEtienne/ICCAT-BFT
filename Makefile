## Makefile for running iscam
## Author: Steve Martell modified by MP Etienne
## Makefile for running iscam on bfte
## Targets: 
##		all:   -copy executable and run the model with DAT & ARG
##		run:   -copy executable and force a run
##		mcmc:  -copy executable and run in mcmc mode and mceval
##		retro: -copy executable and run  retrospective analysis
##		sim:   -copy executable and run simulation analysis

# |------------------------------------------------------------------------------------|
# | MACROS
# |------------------------------------------------------------------------------------|
# |

EXEC=iscam
ARG=bfte/2012/vpa/reported/low
TARGET=$(ARG)/$(EXEC)
prefix=$(ISCAM_HOME)
DAT=RUN.dat
CTL=$(ARG)/ICCAT
ARGSIM=1
MCFLAG=-mcmc 10000 -mcsave 100 -nosdmcmc
NR=4
NOSIM  = 1
NSTART  = 1



# |------------------------------------------------------------------------------------|
# | DEBUG FLAG
# |------------------------------------------------------------------------------------|
# |
ifdef DEBUG
  DIST=$(prefix)/debug/iscam
else
  DIST=$(prefix)/release/iscam
endif


# |------------------------------------------------------------------------------------|
# | COPY EXEC AND RUN MODEL
# |------------------------------------------------------------------------------------|
# |
all:  $(TARGET).par

$(TARGET): $(DIST)
	cp $(DIST) $@

$(TARGET).par:  $(CTL).ctl $(TARGET)
	cp $(DIST) $@
	cd $(ARG); ./$(EXEC) -ind $(DAT) $(ARG)

$(CTL).ctl: sources/setISCAMFiles.R 
	Rscript sources/setISCAMFiles.R  Inputs/$(ARG) 

# |---------------------------------------------------------------------------------- |
# | MCMC and MCEVAL
# |------------------------------------------------------------------------------------|
# |
mcmc:  $(TARGET).psv

$(TARGET).psv: $(CTL).ctl  $(DIST) $(TARGET)
	cd $(ARG); ./$(EXEC) -ind $(DAT) $(ARG) $(MCFLAG) 
	cd $(ARG); ./$(EXEC) -ind $(DAT) $(ARG) -mceval

cleanmcmc: 
	rm $(TARGET).psv

# |------------------------------------------------------------------------------------|
# | RETROSPECTIVE
# |------------------------------------------------------------------------------------|
# |
retro: $(TARGET) $(TARGET).ret1

$(TARGET).ret1:
	@echo $(RUNRETRO) | R --vanilla --slave

RUNRETRO = 'args = paste("-retro",c(1:$(NR),0),"-nox"); \
            sapply(args,\
            function(a){ cmd=paste("cd $(ARG) & ./$(EXEC)","-ind $(DAT) $(ARG)",a);\
                        system(cmd)})'

# |------------------------------------------------------------------------------------|
# | REMOVE TEMPORARY ADMB FILES
# |------------------------------------------------------------------------------------|
# |
dust:
	cd $(ARG) & rm -f *.log *.rpt *.htp admodel.* variance *.bar *.mcm

clean:
	cd $(ARG) & rm -rf iscam.* admodel.* variance eigv.rpt fmin.log $(EXEC) variance *.ret[0-9]*


# |------------------------------------------------------------------------------------|
# | SIMULATIONS TO BE RUN IN PARALLEL IN NUMERIC DIRECTORIES
# |------------------------------------------------------------------------------------|
# | NOSIM determines the number of simulations.
# | simdirs     : is the list of simulation directories to copy material to.
# | datadone: is a loop for looping over directories

simdirs := $(shell echo 'cat(formatC($(NSTART):($(NSTART)+$(NOSIM)-1), digits=4, flag="0"))' | R --vanilla --slave)
createdir:=$(foreach dir,simulation/$(simdirs),$(dir)/createdir)
datadone:= $(foreach dir,$(simdirs),$(dir)/datadone)
runsims := $(foreach dir,$(simdirs),$(dir)/runsims)
# |------------------------------------------------------------------------------------|
# | BUILD DIRECTORIES, COPY FILES AND RUN SIMULATIONS INTO EACH DIRECTORY (target = data)
# |------------------------------------------------------------------------------------|
# |


$(datadone):
	Rscript sources/setISCAMFiles.R  Inputs/$(ARG) $(@D)
	cp  $(ARG)/$(CTL).[cp]*[!v]   $(EXEC) simulation/$(@D)
	cp $(DIST) simulation/$(@D)
	-cd simulation/$(@D);./$(EXEC) -ind $(DAT)

data: $(datadone)

cleansims: 
	rm -r simulation/0* 

sim: data 

