#!/usr/bin/env tclsh8.5

package require html
package require smtp
package require mime

################################################################################
# Configuration
################################################################################

if {[llength $argv]} {
    set ::configfile [lindex $argv 0]
} else {
    puts stderr "Usage: recidiv <config-file>"
    exit 1
}

# Create the procedures to set the configuration parameters.
foreach {directive defval} {
    notify.from ci@test.org
    notify.to {}
    history.len 1024
    datafile /tmp/recidiv.data
    webroot /tmp/
    whitelist {}
    template {<html><body>%content</body></html>}
    notify.every.failure 0
    smtp.server 127.0.0.1
    wait.after.every.test 0
    web.index.show.latest 15
    web.index.show.latest.err 5
} {
    proc $directive val "set ::$directive \$val"
    set $directive $defval
}

# The 'test' procedure is used in order to setup a new CI test.
proc test {name steps} {
    lappend ::tests $name $steps
}

################################################################################
# Running the tests.
################################################################################

source $::configfile
set ::test_id 0

proc ci_error {stage msg} {
    set ::err [list $stage $msg]
    puts "### ERROR in stage '$stage': $msg"
}

proc ci_exec_command cmd {
    puts "++ $cmd"
    if {[catch {
        if {[lindex $cmd 0] eq {cd}} {
            cd [lindex $cmd 1]
            set output "Working dir is now '[lindex $cmd 1]'\n"
        } else {
            set output [exec -ignorestderr {*}$cmd 2>@1]
        }
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

# Render commands output to HTML.
proc output-to-html o {
    set e [e $o]
    regsub -all "\n(@\[^\n\]+)\n" $o "\n</pre><h3>\\1</h3><pre>\n"
}

# Create the detail page, the one of a specific test run.
proc update_detail_page item {
    set content {}
    foreach {status id time branch tag err fulloutput} $item break
    if {[file exists [file join $::webroot run_$id.html]]} return
    append content "<div class=\"rundetails\">"
    append content "<h2>Details for run #$id (<span class=\"status_$status\">$status</span>)</h2>\n"
    append content "<h3>$branch ($tag)</h3>\n"
    append content "<h4>[clock format $time]</h4>\n"
    if {$status eq {ok}} {
        append content "<pre class=\"okpre\">[output-to-html $fulloutput]</pre>\n"
    } else {
        append content "<div class=\"errorcmd\">Error in: [lindex $err 0]</div>\n<pre class=\"okpre\">[output-to-html $fulloutput]</pre>\n<pre class=\"errpre\">[output-to-html [lindex $err 1]]</pre>\n"
    }
    append content "</div>"
    write_html [file join $::webroot run_$id.html] $content
}

proc latest_runs_to_html {name count status_pattern} {
    set h [set ::history_$name]
    set c 0
    set content {}

    for {set j 0} {$j < [llength $h]} {incr j} {
        if {$c == $count} break
        set item [lindex $h end-$j]
        if {$item eq {}} break
        foreach {status id time name tag} $item break
        if {[string match -nocase $status_pattern $status]} {
            append content "<li><a href=\"run_$id.html\">#$id</a> \[<span class=\"status_$status\">$status</span>\] $name ($tag) -- [clock format $time]</li>\n"
            update_detail_page $item
            incr c
        }
    }
    if {$c == 0} {
        append content "No items."
    }
    return $content
}

# Create the index.html file in the web site, and triggers the generation
# of all the details pages.
proc update_site {} {
    set content {}
    foreach {name commands} $::tests {
        if {[info exists ::history_$name]} {
            append content "<h2>$name</h2>\n<ul>\n"
            append content [latest_runs_to_html $name [set ::web.index.show.latest] *]
            append content "</ul>\n"
            set latesterr [latest_runs_to_html $name [set ::web.index.show.latest.err] err]
            if {$latesterr ne {}} {
                append content "<h3>latest errors</h3>\n<ul>\n"
                append content $latesterr
                append content "</ul>\n"
            }
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
# Email notifications
################################################################################
proc send_email_message {from recipient email_server subject body} {
    set token [mime::initialize -canonical text/plain -string $body]
    mime::setheader $token From $from
    mime::setheader $token To $recipient
    mime::setheader $token Subject $subject
    smtp::sendmessage $token \
          -originator $from -recipients $recipient -servers $email_server
    mime::finalize $token
}

proc handle_notifications name {
    set notify 0
    set h [set ::history_$name]
    set curr [lindex $h end]
    set prev [lindex $h end-1]
    foreach {status id time name tag err output} $curr break
    if {$status eq {err}} {
        if {[set ::notify.every.failure] || $prev eq {} ||
                                       [lindex $prev 0] ne {err}} {
            set notify 1
        }
    } else {
        if {$prev ne {} && [lindex $prev 0] eq {err}} {
            set notify 1
        }
    }

    # Send email notifications
    if {$notify} {
        foreach to [set ::notify.to] {
            puts "\[\\/\] send notification to $to"
            set subject "\[recidiv notification\] Test '$name' new state is: $status"
            set body "Details below:\n$output\nIn [lindex $err 0]:\n\n[lindex $err 1]"
            if {[catch {
                send_email_message [set ::notify.from] $to [set ::smtp.server] $subject $body
            } e]} {
                puts "Warning: problems sending email: $e"
            }
        }
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
            append fulloutput "\n@$cmd\n" [ci_exec_command $cmd]
            if {$::err ne {}} break
        }

        # If it is a git repository we try to extract the SHA of the version
        # we are testing in order to provide more information.
        catch {
            if {[file exists .git]} {
                set gitlog [exec -ignorestderr git log --oneline 2>@1]
                set tag [lindex [split $gitlog] 0]
            }
        }

        # Save this run in history.
        incr ::test_id
        if {$::err ne {}} {
            puts "!!! Error for '$name'"
            lappend ::history_$name [list err $::test_id [clock seconds] $name $tag $::err $fulloutput]
        } else {
            puts "Test successful for 'name'"
            lappend ::history_$name [list ok $::test_id [clock seconds] $name $tag {} $fulloutput]
        }
        handle_notifications $name
        save_data
        puts -nonewline "Updating web site... "
        flush stdout
        update_site
        puts "done\n"
        after [expr {[set ::wait.after.every.test]*1000}]
    }
}
