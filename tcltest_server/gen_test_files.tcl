set f [open docroot/large.bin w]
fconfigure $f -translation binary

for {set i 0} {$i < 1000000} {incr i} {
    puts -nonewline $f [binary format c [expr {$i & 0xff}]]
}
close $f
