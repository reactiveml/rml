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


set xlabel "nb nodes"
set ylabel "time (s)"

#set yrange [0:2000]

#plot \
#  "n_temps.data" using 1:6 every 3 title "D = 10" with lines,\
#  "n_temps.data" using 1:6 every 3::1 title "D = 20" with lines,\
#  "n_temps.data" using 1:6 every 3::2 title "D = 30" with lines\

plot \
  "n_temps.data" using 1:8 every 2 title "D = 20" with lines\
#  "n_temps.data" using 1:7 every 2::1 title "D = 30" with lines\


	
#    EOF
