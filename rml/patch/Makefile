
all:

diff: FORCE
	(for file in `cat ./files`; \
	 do ./diff.sh $$file; \
	 done)


patch: FORCE
	(for file in `cat ./files`; \
	 do ./patch.sh $$file; \
	 done)



public-distrib: diff
	(for file in `cat ./files`; \
	 do rm -f ../$$file; \
	 done)

clean:


realclean: clean
	rm -f *~

cleanall: realclean

FORCE: