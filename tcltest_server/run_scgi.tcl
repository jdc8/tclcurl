lappend auto_path lib

package require scgi
package require nscgi

proc handle_request {sock headers body} {

    puts "REQUEST: $headers"

    set cgi [nscgi new headers $headers body $body init_cgi 0]
    $cgi input
    fconfigure $sock -translation binary

    set C ""
    append C "From SCGI"

    set H [$cgi header {test/plain; charset="utf-8"}]
    set buffer [encoding convertto utf-8 "Status: 200 OK\n$H\n$C"]

    puts $sock $buffer
    close $sock
}

scgi::listen 9999
vwait forever
