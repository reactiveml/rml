#!/usr/bin/gnuplot -persist
#
#    
#    	G N U P L O T
#    	Version 4.0 patchlevel 0
#    	last modified Thu Apr 15 14:44:22 CEST 2004
#    	System: Linux 2.6.6
#    
#    	Copyright (C) 1986 - 1993, 1998, 2004
#    	Thomas Williams, Colin Kelley and many others
#    
#    	This is gnuplot version 4.0.  Please refer to the documentation
#    	for command syntax changes.  The old syntax will be accepted
#    	throughout the 4.0 series, but all save files use the new syntax.
#    
#    	Type `help` to access the on-line reference manual.
#    	The gnuplot FAQ is available from
#    		http://www.gnuplot.info/faq/
#    
#    	Send comments and requests for help to
#    		<gnuplot-info@lists.sourceforge.net>
#    	Send bugs, suggestions and mods to
#    		<gnuplot-bugs@lists.sourceforge.net>
#    
# set terminal x11 
# set output

# set terminal postscript eps


set xlabel "area size"
set ylabel "time (s)"
plot [0:1000][2:11]\
  "range_area_temps.data" using 2:3 every 4 title "range = 10" with lines,\
  "range_area_temps.data" using 2:3 every 4::1 title "range = 50" with lines,\
  "range_area_temps.data" using 2:3 every 4::2 title "range = 100" with lines\
#  "range_area_temps.data" using 2:3 every 4::3 title "range = 200" with lines
	

	
#    EOF
