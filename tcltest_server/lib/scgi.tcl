# Taken from wiki.tcl.tk/19760

package provide scgi 1.0.0

namespace eval scgi {
    proc listen {port} {
        socket -server [namespace code connect] $port
    }

    proc connect {sock ip port} {
        fconfigure $sock -blocking 0 -translation {binary crlf}
        fileevent $sock readable [namespace code [list read_length $sock {}]]
    }

    proc read_length {sock data} {
        append data [read $sock]
        if {[eof $sock]} {
            close $sock
            return
        }
        set colonIdx [string first : $data]
        if {$colonIdx == -1} {
            # we don't have the headers length yet
            fileevent $sock readable [namespace code [list read_length $sock $data]]
            return
        } else {
            set length [string range $data 0 $colonIdx-1]
            set data [string range $data $colonIdx+1 end]
            read_headers $sock $length $data
        }
    }

    proc read_headers {sock length data} {
        append data [read $sock]

        if {[string length $data] < $length+1} {
            # we don't have the complete headers yet, wait for more
            fileevent $sock readable [namespace code [list read_headers $sock $length $data]]
            return
        } else {
            set headers [string range $data 0 $length-1]
            set headers [lrange [split $headers \0] 0 end-1]
            set body [string range $data $length+1 end]
            set content_length [dict get $headers CONTENT_LENGTH]
            read_body $sock $headers $content_length $body
        }
    }

    proc read_body {sock headers content_length body} {
        append body [read $sock]

        if {[string length $body] < $content_length} {
            # we don't have the complete body yet, wait for more
            fileevent $sock readable [namespace code [list read_body $sock $headers $content_length $body]]
            return
        } else {
            handle_request $sock $headers $body
        }
    }
}
