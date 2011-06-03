proc sqlplus { } {

	set buf   $::vim::current(buffer)

	if { [file tail [$buf name]] != "afiedt.buf" } {
		puts "Must use file named afiedt.buf"
		return
	} 

	set Query [$buf get $::vim::range(start) $::vim::range(end)]

	$buf command "w! ~/.sqlplus.hack"
	$buf delete first last

	foreach line $Query {
		$buf append end $line
	}

	# Save state. 
	set fh [open ~/.sqlplus.hackrc w]
	puts $fh "set ::__SQLPLUS_ROW $::vim::range(start)"
	close $fh

	set ::__SQLPLUS_LOADED 0

	$buf command "set noai"
	$buf command "/;"
	$buf command "normal $"
	$buf command "normal xj0"
	$buf command "normal o/"
	$buf command "w! [$buf name]"
	$buf command "wq!"
}

proc sqlplus_force_save { } {
	set buf   $::vim::current(buffer)

	# Only force save if they typed :wq
	if [info exists ::__SQLPLUS_LOADED] {
		if { $::__SQLPLUS_LOADED == 1 } {
			set fh [open ~/.sqlplus.hackrc w]
			puts $fh "set ::__SQLPLUS_ROW $::vim::range(start)"
			close $fh
			$buf command "set writeany"
			$buf command "w! ~/.sqlplus.hack"
			$buf command "wq!"
		}
	}

}

proc sqlplus_reload { } {

	set buf   $::vim::current(buffer)
	set win   $::vim::current(window)

	if { [file tail [$buf name]] != "afiedt.buf" } {
		puts "Must use file named afiedt.buf"
		return
	} 

	$buf delete first last
	$buf command "read ~/.sqlplus.hack"
	$buf delete first	

	# Load a saved state, and set cursor back to original position.
	catch { source ~/.sqlplus.hackrc } error
	if [info exists ::__SQLPLUS_ROW] {
		$win cursor $::__SQLPLUS_ROW 0	
	}

	set ::__SQLPLUS_LOADED 1
	$win delcmd [list sqlplus_force_save]

}
