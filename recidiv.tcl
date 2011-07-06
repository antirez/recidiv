#!/usr/bin/env tclsh8.5

package require html

################################################################################
# Configuration
################################################################################

# Create the procedures to set the configuration parameters.
foreach directive {
    notify.from notify.to history.len datafile
    webroot whitelist template
} {
    proc $directive val "set ::$directive \$val"
}

# The 'test' procedure is used in order to setup a new CI test.
proc test {name steps} {
    lappend ::tests $name $steps
}

################################################################################
# Running the tests.
################################################################################

source ci.conf
set ::test_id 0

proc ci_error {stage msg} {
    set ::err [list $stage $msg]
    puts "### ERROR in stage '$stage': $msg"
}

proc ci_exec_command cmd {
    if {$::err ne {}} return {}
    puts "++ $cmd"
    if {[catch {
        set output [exec -ignorestderr {*}$cmd 2>@1]
    } e]} {
        ci_error $cmd $e
        set output {}
    }
    return $output
}

################################################################################
# Data persistence. We just use the fact that in Tcl everything is a string...
################################################################################

proc save_data {} {
    set data {}
    foreach {name commands} $::tests {
        if {[info exists ::history_$name]} {
            lappend data ::history_$name [set ::history_$name]
        }
    }
    lappend data ::test_id [set ::test_id]
    set fd [open $::datafile w]
    puts -nonewline $fd $data
    close $fd
}

proc load_data {} {
    if {![file readable $::datafile]} return
    set fd [open $::datafile]
    set data [read $fd]
    close $fd
    foreach {var val} $data {
        set $var $val
    }
}

################################################################################
# HTML generation
################################################################################

# Alias for html entitites.
proc e s {html::html_entities $s}

# Write an HTML file using the global HTML template.
proc write_html {file body} {
    set html [string map [list %content% $body] $::template]
    set fd [open $file w]
    puts -nonewline $fd $html
    close $fd
}

# Create the detail page, the one of a specific test run.
proc update_detail_page item {
    set content {}
    foreach {status id time branch tag err fulloutput} $item break
    append content "<div class=\"rundetails\">"
    append content "<h2>Details for run #$id (<span class=\"status_$status\">$status</span>)</h2>\n"
    append content "<h3>$branch ($tag)</h3>\n"
    append content "<h4>[clock format $time]</h4>\n"
    if {$status eq {ok}} {
        append content "<pre class=\"okpre\">[e $fulloutput]</pre>\n"
    } else {
        append content "<h2>@[lindex $err 0]</h2><pre class=\"okpre\">[e $fulloutput]</pre>\n<pre class=\"errpre\">[e [lindex $err 1]]</pre>\n"
    }
    append content "</div>"
    write_html [file join $::webroot run_$id.html] $content
}

# Create the index.html file in the web site, and triggers the generation
# of all the details pages.
proc update_site {} {
    puts "Updating web site..."
    set content {}
    foreach {name commands} $::tests {
        if {[info exists ::history_$name]} {
            append content "<h2>$name</h2>\n<ul>\n"
            for {set j 0} {$j < 10} {incr j} {
                set item [lindex [set ::history_$name] end-$j]
                if {$item eq {}} break
                foreach {status id time name tag} $item break
                append content "<li><a href=\"run_$id.html\">#$id</a> \[<span class=\"status_$status\">$status</span>\] $name ($tag) -- [clock format $time]</li>\n"
                update_detail_page $item
            }
            append content "</ul>\n"
        }
    }
    write_html [file join $::webroot index.html] $content
}

# Print some history information when the CI is executed.
# Just as a way to say "Welcome, you are running the right thing".
proc print_history_info {} {
    foreach {name commands} $::tests {
        if {[info exists ::history_$name]} {
            puts "$name:"
            flush stdout
            foreach item [set ::history_$name] {
                foreach {status id time branch tag} $item break
                puts "#$id \[$status\] $branch ($tag) -- [clock format $time]"
            }
        }
        puts {}
    }
}

################################################################################
# Main!
################################################################################

# Start the CI. Load data, Update the site (just in case you removed it),
# Print the history information, and run tests forever.
load_data
update_site
print_history_info
while 1 {
    foreach {name commands} $::tests {
        set ::err {}
        set tag {???}
        set fulloutput {}
        puts "======== Testing '$name'"
        foreach cmd $commands {
            if {[lindex $cmd 0] eq {cd}} {
                cd [lindex $cmd 1]
                append fulloutput "++ Working dir is now '[lindex $cmd 1]'\n\n"
            } else {
                append fulloutput "\n@$cmd\n\n" [ci_exec_command $cmd]
            }
        }

        # If it is a git repository we try to extract the SHA of the version
        # we are testing in order to provide more information.
        catch {
            if {[file exists .git]} {
                set gitlog [exec -ignorestderr git log --oneline $branch 2>@1]
                set tag [lindex [split $gitlog] 0]
            }
        }

        # Save this run in history.
        incr ::test_id
        if {$::err ne {}} {
            puts "!!! Error for '$name'"
            lappend ::history_$name [list err $::test_id [clock seconds] $name $tag $::err $fulloutput]
        } else {
            puts "Test successful for '$b'"
            lappend ::history_$name [list ok $::test_id [clock seconds] $name $tag {} $fulloutput]
        }
        puts {}
        save_data
        update_site
    }
}
