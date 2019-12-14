run-day-x:
	ghc --make ./$(DAY)/$(DAY) && cd $(DAY) && ./$(DAY)

clean-day-x:
	$(RM) $(DAY)/$(DAY).hi && $(RM) $(DAY)/$(DAY).o $$ $(RM) $(DAY)/$(DAY)