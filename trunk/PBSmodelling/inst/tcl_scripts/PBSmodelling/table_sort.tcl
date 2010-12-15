# Register the package
package provide PBSmodelling 1.0
package require Tcl      8.5

# Create the namespace
namespace eval ::PBSmodelling {
    # Export commands
    namespace export create
}

proc ::PBSmodelling::createLabel { w tt name var var_dim col_i } {
	set arrow_x 16
	set arrow_y 16
	frame $tt -relief raised -borderwidth 2
	grid [ttk::label $tt.lab -text "$name"] -column 0 -row 0
	#$tt.lab configure -background red
	grid [canvas $tt.can -borderwidth 0 -highlightthickness 0 -relief flat -takefocus 0 -width $arrow_x -height $arrow_y] -column 1 -row 0
	#$tt.can create line 0 0 3 3
	image create photo box -width $arrow_x -height $arrow_y -palette 256/256/256
	image create photo box_up -width $arrow_x -height $arrow_y -palette 256/256/256
	image create photo box_down -width $arrow_x -height $arrow_y -palette 256/256/256
	$tt.can create image 0 0 -image box -anchor nw -tags arrow

	#make some really shitty arrows 
	#TODO (should really do this as bitmap data, and import the image)
	for { set y 0 } { $y < 5 } { incr y } { #num of lines
		for { set x 0 } { $x <= $y } { incr x } { #width of arrow
			box_down put black -to [expr 8+$x] [expr $y+6]
			box_down put black -to [expr 8-$x] [expr $y+6]

			box_up put black -to [expr 8+$x] [expr 16-$y-6]
			box_up put black -to [expr 8-$x] [expr 16-$y-6]
		}
	}

	#TODO re-enable the relief changing
	#eval "bind $tt.lab <Button-1> {$tt configure -relief sunken}"
	##TODO not quite the right event -> if the mouse moves off the label without releasing, then we shouldn't use 
	#eval "bind $tt.lab <ButtonRelease-1> {$tt configure -relief raised}"
	bind $tt.lab <1> "columnclick $w %W $var \"$var_dim\" $col_i"
	
	return $tt
}

# proc ::PBSmodelling::mouseMove { 

proc redisplay { w var var_dim } {
	set nrow [lindex $var_dim 0]
	set ncol [lindex $var_dim 1]
	for { set j 1 } { $j <= $nrow } { incr j } {
		for { set i 1 } { $i <= $ncol } { incr i } {
			$w.lab$j-$i configure -text "[set [set var]($j,$i)]"
		}
	}
}

# Create a new stack
proc ::PBSmodelling::create { tt var var_dim col_names } {

	set nrow [lindex $var_dim 0]
	set ncol [lindex $var_dim 1]

	#inspired from http://wiki.tcl.tk/10923
	frame $tt
	scrollbar $tt.h -command "$tt.c xview" -orient horiz
	scrollbar $tt.v -command "$tt.c yview"
	canvas $tt.c -xscrollcommand "$tt.h set" -yscrollcommand "$tt.v set" -yscrollincrement 1
	#pack $tt -expand 1 -fill both
	grid rowconfig $tt 0 -weight 1 -minsize 0
	grid columnconfig $tt 0 -weight 1 -minsize 0
	grid $tt.c -row 0 -column 0 -sticky news
	grid $tt.v -row 0 -column 1 -sticky news
	grid $tt.h -row 1 -column 0 -sticky news
	bind $tt <Configure> "gconf $tt"
	frame $tt.c.f
	$tt.c create window 0 0 -window $tt.c.f -anchor nw

	for { set i 1 } { $i <= $ncol } { incr i } {
		set w [createLabel $tt.c.f $tt.c.f.lab$i [lindex $col_names [expr $i-1]] $var $var_dim $i]
		grid $w -column $i -row 0 -sticky we
	}

	for { set j 1 } { $j <= $nrow } { incr j } {
		for { set i 1 } { $i <= $ncol } { incr i } {
			grid [ttk::label $tt.c.f.lab$j-$i -text "[set [set var]($j,$i)]"] -column $i -row $j -sticky we
			bindtags $tt.c.f.lab$j-$i [lreplace [bindtags $tt.c.f.lab$j-$i] 0 0 $tt.c.f]
		}
	}

	redisplay $tt.c.f $var "$var_dim"
	#canvas $tt.c.f.can-21 -height 1 -width 1 -bg red
	#grid $tt.c.f.can-21 -column "1" -columnspan 20 -sticky we

	#tk_messageBox -message "[grid bbox $tt.c.f 3 3]" -type ok

	canvas $tt.c.f.hover -height 1 -width 100 -bg red
	#don't place this -> it's placed by the hovering mouse motion call
	
	#bind $tt.c.f <Motion> "mousemove $tt.c.f %x %y %W"
	bind $tt.c.f <B1-Motion> "mouseover $tt.c.f %x %y %W"

	bind $tt.c.f <1> "click $tt.c.f %W"
	bind $tt.c.f <ButtonRelease-1> "unclick $tt.c.f %W $var \"$var_dim\""

	return $tt
	#return $tt.c.f
}

proc displayColumnLabelSortDirection { w num_cols } {
	variable last_sort
	if { [info exists last_sort] == 0 } {
		set last_sort 0
	}
	for { set i 1 } { $i <= $num_cols } { incr i } {
		$w.lab$i.can delete arrow
		if { [lindex $last_sort 0] == $i } {
			if { [lindex $last_sort 1] == 1 } {
				$w.lab$i.can create image 0 0 -image box_up -anchor nw -tags arrow
			} else {
				$w.lab$i.can create image 0 0 -image box_down -anchor nw -tags arrow
			}
		} else {
			$w.lab$i.can create image 0 0 -image box -anchor nw -tags arrow
		}
	}
}

proc columnclick {w clicked var var_dim col} {
	set rows [lindex $var_dim 0]
	set cols [lindex $var_dim 1]

	variable last_sort
	if { [info exists last_sort] == 0 } {
		set last_sort 0
	}
	if { [lindex $last_sort 0] == $col } {
		if { [lindex $last_sort 1] == 1 } { set direction -1 } else { set direction 1 }
	} else {
		set direction 1
	}
	set last_sort "$col $direction"

	#lamely sort in o(n^2)
	for { set i 1 } { $i <= $rows } { incr i } {
		set min_i $i
		for { set j $i } { $j <= $rows } { incr j } {
			#when direction is -1, search for max
			if { [expr $direction * [set [set var]($j,$col)]] < [expr $direction * [set [set var]($min_i,$col)]] } {
				set min_i $j
			}
		}

		#copy all columns
		swaprows $var $var_dim $i $min_i
	}
	redisplay $w $var $var_dim
	displayColumnLabelSortDirection $w $cols

}

proc swaprows {var var_dim row1 row2} {
	if { $row1 == $row2 } { return }
	set cols [lindex $var_dim 1]

	for { set i 1 } { $i <= $cols } { incr i } {
		set saved [set [set var]($row1,$i)]
		set [set var]($row1,$i) [set [set var]($row2,$i)]
		set [set var]($row2,$i) $saved
	}
}

proc click {w clicked} {
	array set gridinfo [grid info $clicked]
	set row $gridinfo(-row)
	#tk_messageBox -message "[$w.lab$row-1 cget -background]" -type ok

	#highlight clicked row
	set col 1
	while { [winfo exists $w.lab$row-$col] } {
		$w.lab$row-$col configure -background red
		incr col
	}
}

proc moverows {var var_dim from to} {
	if { $from == $to || $from == [expr $to -1] || $to < 1 } {
		return
	}
	set ncol [lindex $var_dim 1]
	#save row to be moved
	for { set x 1 } { $x <= $ncol } { incr x } {
		set saved($x) [set [set var]($from,$x)]
	}

	if { $from < $to } {
		set to [expr $to - 1]
		for { set y $from } { $y < $to } { incr y } {
			for { set x 1 } { $x <= $ncol } { incr x } {
				set [set var]($y,$x) [set [set var]([expr $y+1],$x)]
			}
		}
	} else {
		for { set y $from } { $y > $to } { incr y -1 } {
			for { set x 1 } { $x <= $ncol } { incr x } {
				set [set var]($y,$x) [set [set var]([expr $y-1],$x)]
			}
		}
	}

	#copy saved row to final position
	for { set x 1 } { $x <= $ncol } { incr x } {
		set [set var]($to,$x) $saved($x) 
	}
	#tk_messageBox -message "move $from to $to" -type ok

	#set [set var](10,1) "row 10"
}

proc unclick {w clicked var var_dim} {
	array set gridinfo [grid info $clicked]
	set row $gridinfo(-row)
	#unset background of clicked row
	set col 1
	while { [winfo exists $w.lab$row-$col] } {
		$w.lab$row-$col configure -background ""
		incr col
	}

	#stop autoscrolling
	cancelautoscroll $w

	#get position of moved to row
	array set placeinfo [place info $w.hover]
	if {[array size placeinfo] == 0} {
		#nothink was dragged
		return
	}
	set gridloco [grid location $w $placeinfo(-x) $placeinfo(-y)]
	set moveto [expr [lindex $gridloco 1] + 1]

	moverows $var $var_dim $row $moveto
	redisplay $w $var "$var_dim"

	#remove direction sorting (since user changed rows)
	variable last_sort
	set last_sort 0
	displayColumnLabelSortDirection $w [lindex $var_dim 1]

	#remove the highlighed move to line
	place forget $w.hover
}

proc autoscroll {w} {
	variable $w.autoscroll_started
	variable $w.autoscroll_units

	#check widget exists (hasn't been closed)
	if { [winfo exists $w] == 0 } {
		set $w.autoscroll_started 0
		return
	}

	if { [set $w.autoscroll_units] == 0 } {
		set $w.autoscroll_started 0
	} else {
		$w yview scroll [set $w.autoscroll_units] units
		after 50 autoscroll $w
	}
	#variable enable_autoscroll
	#if { $enable_autoscroll == 0 } {
		#tk_messageBox -message "stopping" -type ok
	#}
}

#proc mousemove {w x y clicked} {
#	array set gridinfo [grid info $clicked]
#	set row $gridinfo(-row)
#	set col $gridinfo(-column)
#	set bbox [grid bbox $w $col $row]
#	set real_x [expr $x + [lindex $bbox 0] ]
#	set real_y [expr $y + [lindex $bbox 1] ]
#
#	detectautoscroll $w $real_x $real_y 
#}

proc cancelautoscroll {w} {
	set canvas_w [join [lrange [split $w "."] 0 end-2] "."].c
	variable $canvas_w.autoscroll_units 0
}
proc detectautoscroll {w real_x real_y } {
	set canvas_w [join [lrange [split $w "."] 0 end-2] "."].c
	set height [lindex [grid bbox $w] 3]
	set y_percent [expr double($real_y) / $height]
	set yview [$canvas_w yview]

	set dist_top [expr $y_percent - [lindex $yview 0]]
	set dist_bottom [expr [lindex $yview 1] - $y_percent]
	set scroll_threshold [expr ([lindex $yview 1] - [lindex $yview 0]) / 20]


	variable $canvas_w.autoscroll_started
	variable $canvas_w.autoscroll_units
	#init autoscroll_started to 0
	if { [info exists $canvas_w.autoscroll_started] == 0 } {
		set $canvas_w.autoscroll_started 0
	}
	if { $dist_top < $dist_bottom } {
		set tmp [expr $dist_top - $scroll_threshold]
		if { $tmp < 0 } {
			#negative scroll -> up
			set tmp [expr abs($tmp/$scroll_threshold)]
			set tmp [expr int( $tmp * $tmp ) ]
			if { $tmp == 0 } { set tmp 1 }
			set $canvas_w.autoscroll_units [expr -1 * $tmp]
		} else {
			set $canvas_w.autoscroll_units 0
		}
	} else {
		set tmp [expr $dist_bottom - $scroll_threshold]
		if { $tmp < 0 } {
			#tk_messageBox -message "$tmp" -type ok
			#positive scroll -> down
			set tmp [expr abs($tmp/$scroll_threshold)]
			set tmp [expr int( $tmp * $tmp ) ]
			if { $tmp == 0 } { set tmp 1 }
			set $canvas_w.autoscroll_units [expr $tmp]
		} else {
			set $canvas_w.autoscroll_units 0
		}
	}
	if { [set $canvas_w.autoscroll_started] == 0 && [set $canvas_w.autoscroll_units] != 0 } {
		set $canvas_w.autoscroll_started 1
		autoscroll $canvas_w
		#tk_messageBox -message "starting" -type ok
	}
}

proc mouseover {w x y clicked} {
	array set gridinfo [grid info $clicked]
	set row $gridinfo(-row)
	set col $gridinfo(-column)
	set bbox [grid bbox $w $col $row]
	set real_x [expr $x + [lindex $bbox 0] ]
	set real_y [expr $y + [lindex $bbox 1] ]

	set gridloco [grid location $w $real_x $real_y]
	set hover_grid_col [lindex $gridloco 0]
	set hover_grid_row [lindex $gridloco 1]
	set snap_row $hover_grid_col
	set snap_bbox [grid bbox $w $hover_grid_col $hover_grid_row]
	set snap_y [lindex $snap_bbox 1]
	set height [lindex $snap_bbox 3]
	if { [expr $real_y - $snap_y] > [expr $height / 2] } {
		set snap_y [expr $snap_y + $height]
		incr snap_row
	}

	#scroll on click and drag
	detectautoscroll $w $real_x $real_y 
	

	#highlight row where it will be moved to
	#if {$row == $hover_grid_row || $row == [expr $hover_grid_row -1] } {
	#	place forget $w.hover
	#} else {
		place $w.hover -x 0 -y [expr $snap_y -4] -relwidth 1.0 -width 5
	#}
	#tk_messageBox -message "$row $hover_grid_row" -type ok
}

proc gconf {w} {
	$w.c configure -scrollregion [grid bbox $w.c.f]
}

