#!/usr/bin/awk -f

BEGIN {
        # Keywords
        split("assert,include,open",directive,",");
        split("class,constraint,external,functor,inherit,method,module,parser,private,val,virtual,and,as,exception,fun,function,in,let,mutable,of,rec,type,process,await,signal,default,gather",definition,",");
	split("lazy,new,when,do,done,downto,else,for,if,match,or,then,to,try,while,with,present,control,until,|",control,",");
	split("sig,struct,object,begin,end,loop",blocking,",");
	split("emit,pause,run,nothing",builtin,",");

	# Header
	print "<html>";
	print "<head>";
	print "<title>" FILENAME "</title>";
	print "<meta name=\"GENERATOR\" content=\"rml2html\">";
	print "</head>";
	print "<body>";
	print "\n";
	print "<pre>"
      }

      {
	field = 1;
        # Special character
	gsub(">","\&gt;");
	gsub("<","\&lt;");
        # Comments
	gsub("\(\*","<font color=\"990000\">(*");
	gsub("\*\)","*)</font>");
	while(  field <= NF ) {
          # Modules and Builders
	  if ( match($field,/^[A-Z][_a-zA-Z0-9\']*$/) ) {
	    $field = "<font color=\"0033cc\">" $field "</font>";	
	  }
	  # directive
	  for (i in directive) {
	    if ( $field == directive[i]) {
	      $field = "<font color=\"cc9900\">" directive[i] "</font>";
	    }
	  };
	  # definition
	  for (i in definition) {
	    if ( $field == definition[i]) {
	      $field = "<font color=\"green\">" definition[i] "</font>";
	    }
	  };
          # control
	  for (i in control) {
	    if ( $field == control[i]) {
	      $field = "<font color=\"77aaaa\">" control[i] "</font>";
	    }
	  };
	  # blocking
	  for (i in blocking) {
	    if ( $field == blocking[i]) {
	      $field = "<font color=\"990099\">" blocking[i] "</font>";
	    }
	  };
	  # builtin
	  for (i in builtin) {
	    if ( $field == builtin[i]) {
	      $field = "<font color=\"magenta\">" builtin[i] "</font>";
	    }
	  };
	  field++;
	};
	print $0
      }


END   {
        print "</pre>";
	print "</body>";
	print "</html>";
      }
