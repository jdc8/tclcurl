
# Basic support for CGI/SCGI/FastCGI programs, based on tcllib's ncgi
#
# Copyright (c) 2000 Ajuba Solutions.
# Copyright (c) 2012 Richard Hipp, Andreas Kupries
# Copyright (c) 2013 Jos Decoster
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


# Please note that Don Libes' has a "cgi.tcl" that implements version 1.0
# of the cgi package.  That implementation provides a bunch of cgi_ procedures
# (it doesn't use the ::cgi:: namespace) and has a wealth of procedures for
# generating HTML.  In contrast, the package provided here is primarly
# concerned with processing input to CGI programs.  I have tried to mirror his
# API's where possible.  So, ncgi::input is equivalent to cgi_input, and so
# on.  There are also some different APIs for accessing values (ncgi::list,
# ncgi::parse and ncgi::value come to mind)

# Note, I use the term "query data" to refer to the data that is passed in
# to a CGI program.  Typically this comes from a Form in an HTML browser.
# The query data is composed of names and values, and the names can be
# repeated.  The names and values are encoded, and this module takes care
# of decoding them.

package require Tcl 8.6
package require fileutil ; # Required by importFile.

package provide nscgi 2.0.0

oo::class create nscgi {

    variable query contenttype value varlist urlStub listRestrict cookieOutput map _tmpfiles headers body init_cgi

    constructor {args} {
	set listRestrict 0
	set init_cgi 0
	for {set i 1} {$i < 128} {incr i} {
	    set c [format %c $i]
	    if {![string match \[a-zA-Z0-9\] $c]} {
		set map($c) %[format %.2X $i]
	    }
	}
	array set map {
	    " " +   \n %0D%0A
	}
	array set _tmpfiles {}
	my configure {*}$args
	if {$init_cgi} {
	    my initCGI
	}
    }

    destructor {}

    method configure {args} {
	if {[llength $args] == 1} {
	    return [set [lindex $args 0]]
	} elseif {([llength $args] % 2) == 0} {
	    foreach {k v} $args {
		set $k $v
	    }
	}
    }

    # "query" holds the raw query (i.e., form) data
    # This is treated as a cache, too, so you can call ncgi::query more than
    # once

#    variable query

    # This is the content-type which affects how the query is parsed

#    variable contenttype

    # value is an array of parsed query data.  Each array element is a list
    # of values, and the array index is the form element name.
    # See the differences among ncgi::parse, ncgi::input, ncgi::value
    # and ncgi::valuelist for the various approaches to handling these values.

#    variable value

    # This lists the names that appear in the query data

#    variable varlist

    # This holds the URL coresponding to the current request
    # This does not include the server name.

#    variable urlStub

    # This flags compatibility with Don Libes cgi.tcl when dealing with
    # form values that appear more than once.  This bit gets flipped when
    # you use the ncgi::input procedure to parse inputs.

#    variable listRestrict 0

    # This is the set of cookies that are pending for output

#    variable cookieOutput

    # Support for x-www-urlencoded character mapping
    # The spec says: "non-alphanumeric characters are replaced by '%HH'"

#    variable i
#    variable c
#    variable map

    # for {set i 1} {$i <= 256} {incr i} {
    # 	set c [format %c $i]
    # 	if {![string match \[a-zA-Z0-9\] $c]} {
    # 	    set map($c) %[format %.2X $i]
    # 	}
    # }

    # These are handled specially
    # array set map {
    # 	" " +   \n %0D%0A
    # }

    # Map of transient files

#    variable  _tmpfiles
#    array set _tmpfiles {}

# ::ncgi::reset
#
#	This resets the state of the CGI input processor.  This is primarily
#	used for tests, although it is also designed so that TclHttpd can
#	call this with the current query data
#	so the ncgi package can be shared among TclHttpd and CGI scripts.
#
#	DO NOT CALL this in a standard cgi environment if you have not
#	yet processed the query data, which will not be used after a
#	call to ncgi::reset is made.  Instead, just call ncgi::parse
#
# Arguments:
#	newquery	The query data to be used instead of external CGI.
#	newtype		The raw content type.
#
# Side Effects:
#	Resets the cached query data and wipes any environment variables
#	associated with CGI inputs (like QUERY_STRING)

method reset {args} {

    # array unset _tmpfiles -- Not a Tcl 8.2 idiom
    unset _tmpfiles ; array set _tmpfiles {}

    set cookieOutput {}
    if {[llength $args] == 0} {

	# We use and test args here so we can detect the
	# difference between empty query data and a full reset.

	if {[info exists query]} {
	    unset query
	}
	if {[info exists contenttype]} {
	    unset contenttype
	}
    } else {
	set query [lindex $args 0]
	set contenttype [lindex $args 1]
    }
}

# ::ncgi::urlStub
#
#	Set or return the URL associated with the current page.
#	This is for use by TclHttpd to override the default value
#	that otherwise comes from the CGI environment
#
# Arguments:
#	url	(option) The url of the page, not counting the server name.
#		If not specified, the current urlStub is returned
#
# Side Effects:
#	May affects future calls to ncgi::urlStub

method urlStub {{url {}}} {
    if {[string length $url]} {
	set urlStub $url
	return ""
    } elseif {[info exists urlStub]} {
	return $urlStub
    } elseif {[dict exists $headers SCRIPT_NAME]} {
	set urlStub [dict get $headers SCRIPT_NAME]
	return $urlStub
    } else {
	return ""
    }
}

# ::ncgi::query
#
#	This reads the query data from the appropriate location, which depends
#	on if it is a POST or GET request.
#
# Arguments:
#	none
#
# Results:
#	The raw query data.

method query {} {

    if {[info exists query]} {
	# This ensures you can call ncgi::query more than once,
	# and that you can use it with ncgi::reset
	return $query
    }

    set query ""
    if {[dict exists $headers REQUEST_METHOD]} {
	if {[dict get $headers REQUEST_METHOD] == "GET"} {
	    if {[dict exists $headers QUERY_STRING]} {
		set query [dict get $headers QUERY_STRING]
	    }
	} elseif {[dict get $headers REQUEST_METHOD] == "POST"} {
	    if {[dict exists $headers CONTENT_LENGTH] &&
		[string length [dict get $headers CONTENT_LENGTH]] != 0} {
		set query $body
	    }
	}
    }
    return $query
}

# ::ncgi::type
#
#	This returns the content type of the query data.
#
# Arguments:
#	none
#
# Results:
#	The content type of the query data.

method type {} {
    if {![info exists contenttype]} {
	if {[dict exists $headers CONTENT_TYPE]} {
	    set contenttype [dict get $headers CONTENT_TYPE]
	} else {
	    return ""
	}
    }
    return $contenttype
}

# ::ncgi::decode
#
#	This decodes data in www-url-encoded format.
#
# Arguments:
#	An encoded value
#
# Results:
#	The decoded value

method DecodeHex {hex} {
    return [binary decode hex $hex]
}

method decode {str} {
    # rewrite "+" back to space
    # protect \ from quoting another '\'
    set str [string map [list + { } "\\" "\\\\" \[ \\\[ \] \\\]] $str]

    # prepare to process all %-escapes
    regsub -all -- {%([Ee][A-Fa-f0-9])%([89ABab][A-Fa-f0-9])%([89ABab][A-Fa-f0-9])} \
	$str {[encoding convertfrom utf-8 [DecodeHex \1\2\3]]} str
    regsub -all -- {%([CDcd][A-Fa-f0-9])%([89ABab][A-Fa-f0-9])}                     \
	$str {[encoding convertfrom utf-8 [DecodeHex \1\2]]} str
    regsub -all -- {%([0-7][A-Fa-f0-9])} $str {\\u00\1} str

    # process \u unicode mapped chars
    return [subst -novar $str]
}

# ::ncgi::encode
#
#	This encodes data in www-url-encoded format.
#
# Arguments:
#	A string
#
# Results:
#	The encoded value

method encode {str} {
    set r ""
    foreach c [split $str ""] {
	if {[string match {[a-zA-Z0-9]} $c]} {
	    append r $c
	} elseif {[info exists map($c)]} {
	    append r $map($c)
	} else {
	    set c [encoding convertto utf-8 $c]
	    foreach b [split $c ""] {
		append r %[string toupper [binary encode hex $b]]
	    }
	}
    }
    return $r
}

# ::ncgi::names
#
#	This parses the query data and returns a list of the names found therein.
#
# 	Note: If you use ncgi::setValue or ncgi::setDefaultValue, this
#	names procedure doesn't see the effect of that.
#
# Arguments:
#	none
#
# Results:
#	A list of names

method names {} {
    array set names {}
    foreach {name val} [my nvlist] {
        if {![string equal $name "anonymous"]} {
            set names($name) 1
        }
    }
    return [array names names]
}

# ::ncgi::nvlist
#
#	This parses the query data and returns it as a name, value list
#
# 	Note: If you use ncgi::setValue or ncgi::setDefaultValue, this
#	nvlist procedure doesn't see the effect of that.
#
# Arguments:
#	none
#
# Results:
#	An alternating list of names and values

method nvlist {} {
    set _query [my query]
    set _type  [my type]
    switch -glob -- $_type {
	"" -
	text/xml* -
	application/x-www-form-urlencoded* -
	application/x-www-urlencoded* {
	    set result {}

	    # Any whitespace at the beginning or end of urlencoded data is not
	    # considered to be part of that data, so we trim it off.  One special
	    # case in which post data is preceded by a \n occurs when posting
	    # with HTTPS in Netscape.

	    foreach {x} [split [string trim $_query] &] {
		# Turns out you might not get an = sign,
		# especially with <isindex> forms.

		set pos [string first = $x]
		set len [string length $x]

		if { $pos>=0 } {
		    if { $pos == 0 } { # if the = is at the beginning ...
		        if { $len>1 } {
                            # ... and there is something to the right ...
		            set varname anonymous
		            set val [string range $x 1 end]
		        } else {
                            # ... otherwise, all we have is an =
		            set varname anonymous
		            set val ""
		        }
		    } elseif { $pos==[expr {$len-1}] } {
                        # if the = is at the end ...
		        set varname [string range $x 0 [expr {$pos-1}]]
			set val ""
		    } else {
		        set varname [string range $x 0 [expr {$pos-1}]]
		        set val [string range $x [expr {$pos+1}] end]
		    }
		} else { # no = was found ...
		    set varname anonymous
		    set val $x
		}
		lappend result [my decode $varname] [my decode $val]
	    }
	    return $result
	}
	multipart/* {
	    return [my multipart $_type $_query]
	}
	default {
	    return -code error "Unknown Content-Type: $_type"
	}
    }
}

# ::ncgi::parse
#
#	The parses the query data and stores it into an array for later retrieval.
#	You should use the ncgi::value or ncgi::valueList procedures to get those
#	values, or you are allowed to access the ncgi::value array directly.
#
#	Note - all values have a level of list structure associated with them
#	to allow for multiple values for a given form element (e.g., a checkbox)
#
# Arguments:
#	none
#
# Results:
#	A list of names of the query values

method parse {} {
    set listRestrict 0
    set varlist {}
    if {[info exists value]} {
	unset value
    }
    foreach {name val} [my nvlist] {
	if {![info exists value($name)]} {
	    lappend varlist $name
	}
	lappend value($name) $val
    }
    return $varlist
}

# ::ncgi::input
#
#	Like ncgi::parse, but with Don Libes cgi.tcl semantics.
#	Form elements must have a trailing "List" in their name to be
#	listified, otherwise this raises errors if an element appears twice.
#
# Arguments:
#	fakeinput	See ncgi::reset
#	fakecookie	The raw cookie string to use when testing.
#
# Results:
#	The list of element names in the form

method input {{fakeinput {}} {fakecookie {}}} {
    set varlist {}
    set listRestrict 1
    if {[info exists value]} {
	unset value
    }
    if {[string length $fakeinput]} {
	my reset $fakeinput
    }
    foreach {name val} [my nvlist] {
	set exists [info exists value($name)]
	if {!$exists} {
	    lappend varlist $name
	}
	if {[string match "*List" $name]} {
	    # Accumulate a list of values for this name
	    lappend value($name) $val
	} elseif {$exists} {
	    error "Multiple definitions of $name encountered in input.\
	    If you're trying to do this intentionally (such as with select),\
	    the variable must have a \"List\" suffix."
	} else {
	    # Capture value with no list structure
	    set value($name) $val
	}
    }
    return $varlist
}

# ::ncgi::value
#
#	Return the value of a named query element, or the empty string if
#	it was not not specified.  This only returns the first value of
#	associated with the name.  If you want them all (like all values
#	of a checkbox), use ncgi::valueList
#
# Arguments:
#	key	The name of the query element
#	default	The value to return if the value is not present
#
# Results:
#	The first value of the named element, or the default

method value {key {default {}}} {
    if {[info exists value($key)]} {
	if {$listRestrict} {

	    # ::ncgi::input was called, and it already figured out if the
	    # user wants list structure or not.

	    set val $value($key)
	} else {

	    # Undo the level of list structure done by ncgi::parse

	    set val [lindex $value($key) 0]
	}
	if {[string match multipart/* [my type]]} {

	    # Drop the meta-data information associated with each part

	    set val [lindex $val 1]
	}
	return $val
    } else {
	return $default
    }
}

# ::ncgi::valueList
#
#	Return all the values of a named query element as a list, or
#	the empty list if it was not not specified.  This always returns
#	lists - if you do not want the extra level of listification, use
#	ncgi::value instead.
#
# Arguments:
#	key	The name of the query element
#
# Results:
#	The first value of the named element, or ""

method valueList {key {default {}}} {
    if {[info exists value($key)]} {
	return $value($key)
    } else {
	return $default
    }
}

# ::ncgi::setValue
#
#	Jam a new value into the CGI environment.  This is handy for preliminary
#	processing that does data validation and cleanup.
#
# Arguments:
#	key	The name of the query element
#	value	This is a single value, and this procedure wraps it up in a list
#		for compatibility with the ncgi::value array usage.  If you
#		want a list of values, use ngci::setValueList
#
#
# Side Effects:
#	Alters the ncgi::value and possibly the ncgi::valueList variables

method setValue {key val} {
    if {$listRestrict} {
	my setValueList $key $val
    } else {
	my setValueList $key [list $val]
    }
}

# ::ncgi::setValueList
#
#	Jam a list of new values into the CGI environment.
#
# Arguments:
#	key		The name of the query element
#	valuelist	This is a list of values, e.g., for checkbox or multiple
#			selections sets.
#
# Side Effects:
#	Alters the ncgi::value and possibly the ncgi::valueList variables

method setValueList {key valuelist} {
    if {![info exists value($key)]} {
	lappend varlist $key
    }

    # This if statement is a workaround for another hack in
    # ::ncgi::value that treats multipart form data
    # differently.
    if {[string match multipart/* [my type]]} {
	set value($key) [list [list {} [join $valuelist]]]
    } else {
	set value($key) $valuelist
    }
    return ""
}

# ::ncgi::setDefaultValue
#
#	Set a new value into the CGI environment if there is not already one there.
#
# Arguments:
#	key	The name of the query element
#	value	This is a single value, and this procedure wraps it up in a list
#		for compatibility with the ncgi::value array usage.
#
#
# Side Effects:
#	Alters the ncgi::value and possibly the ncgi::valueList variables

method setDefaultValue {key val} {
    my setDefaultValueList $key [list $val]
}

# ::ncgi::setDefaultValueList
#
#	Jam a list of new values into the CGI environment if the CGI value
#	is not already defined.
#
# Arguments:
#	key		The name of the query element
#	valuelist	This is a list of values, e.g., for checkbox or multiple
#			selections sets.
#
# Side Effects:
#	Alters the ncgi::value and possibly the ncgi::valueList variables

method setDefaultValueList {key valuelist} {
    if {![info exists value($key)]} {
	my setValueList $key $valuelist
	return ""
    } else {
	return ""
    }
}

# ::ncgi::exists --
#
#	Return false if the CGI variable doesn't exist.
#
# Arguments:
#	name	Name of the CGI variable
#
# Results:
#	0 if the variable doesn't exist

method exists {var} {
    return [info exists value($var)]
}

# ::ncgi::empty --
#
#	Return true if the CGI variable doesn't exist or is an empty string
#
# Arguments:
#	name	Name of the CGI variable
#
# Results:
#	1 if the variable doesn't exist or has the empty value

method empty {name} {
    return [expr {[string length [string trim [my value $name]]] == 0}]
}

# ::ncgi::import
#
#	Map a CGI input into a Tcl variable.  This creates a Tcl variable in
#	the callers scope that has the value of the CGI input.  An alternate
#	name for the Tcl variable can be specified.
#
# Arguments:
#	cginame		The name of the form element
#	tclname		If present, an alternate name for the Tcl variable,
#			otherwise it is the same as the form element name

method import {cginame {tclname {}}} {
    if {[string length $tclname]} {
	upvar 1 $tclname var
    } else {
	upvar 1 $cginame var
    }
    set var [my value $cginame]
}

# ::ncgi::importAll
#
#	Map a CGI input into a Tcl variable.  This creates a Tcl variable in
#	the callers scope for every CGI value, or just for those named values.
#
# Arguments:
#	args	A list of form element names.  If this is empty,
#		then all form value are imported.

method importAll {args} {
    if {[llength $args] == 0} {
	set args $varlist
    }
    foreach cginame $args {
	upvar 1 $cginame var
	set var [my value $cginame]
    }
}

# ::ncgi::redirect
#
#	Generate a redirect by returning a header that has a Location: field.
#	If the URL is not absolute, this automatically qualifies it to
#	the current server
#
# Arguments:
#	url		The url to which to redirect
#
# Side Effects:
#	Outputs a redirect header

method redirect {url} {

    if {![regexp -- {^[^:]+://} $url]} {

	# The url is relative (no protocol/server spec in it), so
	# here we create a canonical URL.

	# request_uri	The current URL used when dealing with relative URLs.
	# proto		http or https
	# server 	The server, which we are careful to match with the
	#		current one in base Basic Authentication is being used.
	# port		This is set if it is not the default port.

	if {[dict exists $headers REQUEST_URI]} {
	    # Not all servers have the leading protocol spec
	    regsub -- {^https?://[^/]*/} [dict get $headers REQUEST_URI] / request_uri
	} elseif {[dict exists $headers SCRIPT_NAME]} {
	    set request_uri [dict get $headers SCRIPT_NAME]
	} else {
	    set request_uri /
	}

	set port ""
	if {[dict exists $headers HTTPS] && [dict get $headers HTTPS] == "on"} {
	    set proto https
	    if {[dict get $headers SERVER_PORT] != 443} {
		set port :[dict get $headers SERVER_PORT]
	    }
	} else {
	    set proto http
	    if {[dict get $headers SERVER_PORT] != 80} {
		set port :[dict get $headers SERVER_PORT]
	    }
	}
	# Pick the server from REQUEST_URI so it matches the current
	# URL.  Otherwise use SERVER_NAME.  These could be different, e.g.,
	# "pop.scriptics.com" vs. "pop"

	if {[dict exists $headers REQUEST_URI]} {
	    # Not all servers have the leading protocol spec
	    if {![regexp -- {^https?://([^/:]*)} [dict get $headers REQUEST_URI] x server]} {
		set server [dict get $headers SERVER_NAME]
	    }
	} else {
	    set server [dict get $headers SERVER_NAME]
	}
	if {[string match /* $url]} {
	    set url $proto://$server$port$url
	} else {
	    regexp -- {^(.*/)[^/]*$} $request_uri match dirname
	    set url $proto://$server$port$dirname$url
	}
    }
    set C [my header text/html Location $url]
    append C "Please go to <a href=\"$url\">$url</a>\n"
    return $C
}

# ncgi:header
#
#	Output the Content-Type header.
#
# Arguments:
#	type	The MIME content type
#	args	Additional name, value pairs to specifiy output headers
#
# Side Effects:
#	Outputs a normal header

method header {{type text/html} args} {
    variable cookieOutput
    set C ""
    append C "Content-Type: $type\n"
    foreach {n v} $args {
	append C "$n: $v\n"
    }
    if {[info exists cookieOutput]} {
	foreach line $cookieOutput {
	    append C "Set-Cookie: $line\n"
	}
    }
    append C "\n"
    return $C
}

# ::ncgi::parseMimeValue
#
#	Parse a MIME header value, which has the form
#	value; param=value; param2="value2"; param3='value3'
#
# Arguments:
#	value	The mime header value.  This does not include the mime
#		header field name, but everything after it.
#
# Results:
#	A two-element list, the first is the primary value,
#	the second is in turn a name-value list corresponding to the
#	parameters.  Given the above example, the return value is
#	{
#		value
#		{param value param2 value param3 value3}
#	}

method parseMimeValue {val} {
    set parts [split $val \;]
    set results [list [string trim [lindex $parts 0]]]
    set paramList [list]
    foreach sub [lrange $parts 1 end] {
	if {[regexp -- {([^=]+)=(.+)} $sub match key val]} {
            set key [string trim [string tolower $key]]
            set val [string trim $val]
            # Allow single as well as double quotes
            if {[regexp -- {^["']} $val quote]} { ;# need a " for balance
                if {[regexp -- ^${quote}(\[^$quote\]*)$quote $val x val2]} {
                    # Trim quotes and any extra crap after close quote
                    set val $val2
                }
            }
            lappend paramList $key $val
	}
    }
    if {[llength $paramList]} {
	lappend results $paramList
    }
    return $results
}

# ::ncgi::multipart
#
#	This parses multipart form data.
#	Based on work by Steve Ball for TclHttpd, but re-written to use
#	string first with an offset to iterate through the data instead
#	of using a regsub/subst combo.
#
# Arguments:
#	type	The Content-Type, because we need boundary options
#	query	The raw multipart query data
#
# Results:
#	An alternating list of names and values
#	In this case, the value is a two element list:
#		headers, which in turn is a list names and values
#		content, which is the main value of the element
#	The header name/value pairs come primarily from the MIME headers
#	like Content-Type that appear in each part.  However, the
#	Content-Disposition header is handled specially.  It has several
#	parameters like "name" and "filename" that are important, so they
#	are promoted to to the same level as Content-Type.  Otherwise,
#	if a header like Content-Type has parameters, they appear as a list
#	after the primary value of the header.  For example, if the
#	part has these two headers:
#
#	Content-Disposition: form-data; name="Foo"; filename="/a/b/C.txt"
#	Content-Type: text/html; charset="iso-8859-1"; mumble='extra'
#
#	Then the header list will have this structure:
#	{
#		content-disposition form-data
#		name Foo
#		filename /a/b/C.txt
#		content-type {text/html {charset iso-8859-1 mumble extra}}
#	}
#	Note that the header names are mapped to all lowercase.  You can
#	use "array set" on the header list to easily find things like the
#	filename or content-type.  You should always use [lindex $value 0]
#	to account for values that have parameters, like the content-type
#	example above.  Finally, not that if the value has a second element,
#	which are the parameters, you can "array set" that as well.
#
method multipart {type query} {

    set parsedType [my parseMimeValue $type]
    if {![string match multipart/* [lindex $parsedType 0]]} {
	return -code error "Not a multipart Content-Type: [lindex $parsedType 0]"
    }
    array set options [lindex $parsedType 1]
    if {![info exists options(boundary)]} {
	return -code error "No boundary given for multipart document"
    }
    set boundary $options(boundary)

    # The query data is typically read in binary mode, which preserves
    # the \r\n sequence from a Windows-based browser.
    # Also, binary data may contain \r\n sequences.

    if {[string match "*$boundary\r\n*" $query]} {
        set lineDelim "\r\n"
	#	puts "DELIM"
    } else {
        set lineDelim "\n"
	#	puts "NO"
    }

    # Iterate over the boundary string and chop into parts

    set len [string length $query]
    # [string length $lineDelim]+2 is for "$lineDelim--"
    set blen [expr {[string length $lineDelim] + 2 + \
            [string length $boundary]}]
    set first 1
    set results [list]
    set offset 0

    # Ensuring the query data starts
    # with a newline makes the string first test simpler
    if {[string first $lineDelim $query 0]!=0} {
        set query $lineDelim$query
    }
    while {[set offset [string first $lineDelim--$boundary $query $offset]] \
            >= 0} {
	if {!$first} {
	    lappend results $formName [list $mpheaders \
		[string range $query $off2 [expr {$offset -1}]]]
	} else {
	    set first 0
	}
	incr offset $blen

	# Check for the ending boundary, which is signaled by --$boundary--

	if {[string equal "--" \
		[string range $query $offset [expr {$offset + 1}]]]} {
	    break
	}

	# Split headers out from content
	# The headers become a nested list structure:
	#	{header-name {
	#		value {
	#			paramname paramvalue ... }
	#		}
	#	}

        set off2 [string first "$lineDelim$lineDelim" $query $offset]
	set mpheaders [list]
	set formName ""
        foreach line [split [string range $query $offset $off2] $lineDelim] {
	    if {[regexp -- {([^:	 ]+):(.*)$} $line x hdrname val]} {
		set hdrname [string tolower $hdrname]
		set valueList [my parseMimeValue $val]
		if {[string equal $hdrname "content-disposition"]} {

		    # Promote Conent-Disposition parameters up to headers,
		    # and look for the "name" that identifies the form element

		    lappend mpheaders $hdrname [lindex $valueList 0]
		    foreach {n v} [lindex $valueList 1] {
			lappend mpheaders $n $v
			if {[string equal $n "name"]} {
			    set formName $v
			}
		    }
		} else {
		    lappend mpheaders $hdrname $valueList
		}
	    }
	}

	if {$off2 > 0} {
            # +[string length "$lineDelim$lineDelim"] for the
            # $lineDelim$lineDelim
            incr off2 [string length "$lineDelim$lineDelim"]
	    set offset $off2
	} else {
	    break
	}
    }
    return $results
}

# ::ncgi::importFile --
#
#   get information about a file upload field
#
# Arguments:
#   cmd         one of '-server' '-client' '-type' '-data'
#   var         cgi variable name for the file field
#   filename    filename to write to for -server
# Results:
#   -server returns the name of the file on the server: side effect
#      is that the file gets stored on the server and the
#      script is responsible for deleting/moving the file
#   -client returns the name of the file sent from the client
#   -type   returns the mime type of the file
#   -data   returns the contents of the file

method importFile {cmd var {filename {}}} {

    set vlist [my valueList $var]

    array set fileinfo [lindex $vlist 0]
    set contents [lindex $vlist 1]

    switch -exact -- $cmd {
	-server {
	    ## take care not to write it out more than once
	    if {![info exists _tmpfiles($var)]} {
		if {$filename != {}} {
		    ## use supplied filename
		    set _tmpfiles($var) $filename
		} else {
		    ## create a tmp file
		    set _tmpfiles($var) [::fileutil::tempfile ncgi]
		}

		# write out the data only if it's not been done already
		if {[catch {open $_tmpfiles($var) w} h]} {
		    error "Can't open temporary file in ncgi::importFile ($h)"
		}

		fconfigure $h -translation binary -encoding binary
		puts -nonewline $h $contents
		close $h
	    }
	    return $_tmpfiles($var)
	}
	-client {
	    if {![info exists fileinfo(filename)]} {return {}}
	    return $fileinfo(filename)
	}
	-type {
	    if {![info exists fileinfo(content-type)]} {return {}}
	    return $fileinfo(content-type)
	}
	-data {
	    return $contents
	}
	default {
	    error "Unknown subcommand to ncgi::import_file: $cmd"
	}
    }
}


# ::ncgi::cookie
#
#	Return a *list* of cookie values, if present, else ""
#	It is possible for multiple cookies with the same key
#	to be present, so we return a list.
#
# Arguments:
#	cookie	The name of the cookie (the key)
#
# Results:
#	A list of values for the cookie

method cookie {cookie} {
    set result ""
    if {[dict exists $headers HTTP_COOKIE]} {
	foreach pair [split [dict get $headers HTTP_COOKIE] \;] {
	    foreach {key val} [split [string trim $pair] =] { break ;# lassign }
	    if {[string compare $cookie $key] == 0} {
		lappend result $val
	    }
	}
    }
    return $result
}

# ::ncgi::setCookie
#
#	Set a return cookie.  You must call this before you call
#	ncgi::header or ncgi::redirect
#
# Arguments:
#	args	Name value pairs, where the names are:
#		-name	Cookie name
#		-value	Cookie value
#		-path	Path restriction
#		-domain	domain restriction
#		-expires	Time restriction
#
# Side Effects:
#	Formats and stores the Set-Cookie header for the reply.

method setCookie {args} {
    array set opt $args
    set line "$opt(-name)=$opt(-value) ;"
    foreach extra {path domain} {
	if {[info exists opt(-$extra)]} {
	    append line " $extra=$opt(-$extra) ;"
	}
    }
    if {[info exists opt(-expires)]} {
	switch -glob -- $opt(-expires) {
	    *GMT {
		set expires $opt(-expires)
	    }
	    default {
		set expires [clock format [clock scan $opt(-expires)] \
			-format "%A, %d-%b-%Y %H:%M:%S GMT" -gmt 1]
	    }
	}
	append line " expires=$expires ;"
    }
    if {[info exists opt(-secure)]} {
	append line " secure "
    }
    lappend cookieOutput $line
}

method initCGI {} {
    set headers [array get ::env]
    set body ""
    if {[dict exists $headers REQUEST_METHOD] && [dict get $headers REQUEST_METHOD] eq "POST"} {
	if {[dict exists $headers CONTENT_LENGTH] && [string is integer -strict [dict get $headers CONTENT_LENGTH]]} {
	    fconfigure stdin -translation binary -encoding binary
	    set body [read stdin [dict get $headers CONTENT_LENGTH]]
	}
    }
}

method getRequestParam {nm} {
    return [dict get $headers $nm]
}

method existsRequestParam {nm} {
    return [dict exists $headers $nm]
}

}
