run:
	ghc --make ./$(DAY)/$(DAY) && cd $(DAY) && ./$(DAY)

clean:
	$(RM) $(DAY)/$(DAY).hi && $(RM) $(DAY)/$(DAY).o $$ $(RM) $(DAY)/$(DAY)