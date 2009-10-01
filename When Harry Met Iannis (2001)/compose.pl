#! /usr/bin/perl -w
# compose.pl
# compose SASL score for "When Harry Met Iannis"
# M. Edward Borasky, Borasky Research, 28 October 2001
# M. Edward Borasky, Borasky Research, 21 May 2002 -- upgrade to
# 13-limit
# GPL license applies

# The piece, "When Harry Met Iannis", is a random walk in a space of
# chords from Harry Partch's 13-limit Tonality Diamond ("Genesis of a
# Music", page 454).

# A chord is coded as a list. The first element of the list is +1 for
# Otonality and -1 for Utonality. The second element is the octave
# multiplier. The third element is the Numerary Nexus (denominator for
# an Otonality, numerator for a Utonality) and the remaining elements
# are the Identities (Odentities for an Otonality and Udentities for a
# Utonality).

# so

#@chord = (1, 1, 1, 1, 3, 5, 7, 9, 11, 13); # initial chord

# is an Otonality in octave 1 with Numerary Nexus (denominator) 1 and 
# Odentities 1, 3, 5, 7, 9, 11, and 13.

# The piece starts with the full Otonality chord on 1/1 = G 392 Hz.
# Then, each successive chord is derived by a randomly-chosen
# transformation. The possible transformations are:

# 1. Toggle the Otonality / Utonality selector, leaving all else fixed.

# 2. Go up or down one octave, with reflecting boundaries.

# 3. Change the Numerary Nexus to one of the others.

# 4. Add or delete an Identity, with at least three and at most seven
# Identities in all chords.

# initialization
use Config; # used later when we play the output "wav" file 
&openLog; # open the log output file
&openScore; # open the score output file
&openDurations; # open the durations input file
@identlist = (1, 3, 5, 7, 9, 11, 13); # list of identities
$nidents = $#identlist + 1; # number of identities
@chord = (1, 1, 1, 1, 3, 5, 7, 9, 11, 13); # initial chord
$clock = 0; # current time
$piecelength = 300; # how long is the piece?
$hioctave = 4; # upper octave boundary
$looctave = 2; # lower octave boundary
$hichord = $nidents; # maximum notes in a chord
$lochord = 3; # minimum notes in a chord

# main loop
while (1) { # exit with "last" at end of piece		
	if (-e $durationfile) { # is there a file?
		$duration = <DURATIONS>; # grab a line	
		$duration += 0; # force numeric
	}	
	else { # generate a duration at random
		$duration = 1/4 + rand (1 + 3/4);
	}	
	print "${clock} ${duration} @{chord}\n"; # debug output
	print LOG "${clock} ${duration} @{chord}\n"; # debug output
	&generateScore (@chord); # generate the SASL for the chord
	$clock += $duration; # advance the clock
	last if $clock > $piecelength; # stop the music!
	@chord = &transform (@chord); # transform the chord
}
&closeDurations; # close the duration file
&closeScore; # close the score file
&closeLog; # close the log file
&render; # play the piece
exit; # we're outta here!

sub openLog { # open the duration file
	open (LOG, "> HarryIannis.log");
}

sub openDurations { # open the duration file
	$durationfile = shift (@ARGV); # pick up file name
	open (DURATIONS, $durationfile) if -e $durationfile; # open it
}

sub openScore { # open the score file
	open (SCORE, "> HarryIannis.sasl");
	print SCORE "0 tempo 60\n";	# 1 beat = 1 second; makes stuff easy
}

sub closeDurations { # close the duration file
	close (DURATIONS) if -e $durationfile;
}

sub closeLog { # close the log file
	close (LOG);
}

sub closeScore { # close the score file
	print SCORE "${clock} end\n"; # SASL end command
	close (SCORE);
}

sub transform { # make a random transformation

	# first, pick up inputs
	$OUSwitch = shift (@_); # Otonality / Utonality switch
	$octave = shift (@_); # octave
	$nexus = shift (@_); # Numerary Nexus
	@idents = @_; # the Identities

	$majorcase = int (rand (4)); # major case number
	&OUSwitch if $majorcase == 0; # switch from O to U or vice versa
	&octave if $majorcase == 1; # up or down an octave
	&nexus if $majorcase == 2; # pick a new Numerary Nexus
	&idents if $majorcase == 3; # add or drop an Identity
	return ($OUSwitch, $octave, $nexus, @idents); # return transformed chord
}

sub generateScore { # write the control codes for this chord
	my ($OUSwitch) = shift (@_); # Otonality / Utonality switch
	my ($octave) = shift (@_); # octave
	my ($nexus) = shift (@_); # Numerary Nexus
	my (@idents) = @_; # the Identities
	my ($over, $under, $ident, $lolim, $hilim); # working variables
	my ($count) = $#idents + 1; # number of identities
	my ($ix) = rand ($count); # index to the one we're using as the "root"

	# now generate the pitches
	if ($OUSwitch > 1) { # Otonality?

		# put the "root" between 1/1 and 2/1
		($over, $under) = &between ($idents[$ix], $nexus, 1, 2);
		$lolim = $over/$under; $hilim = $lolim*2; # compute limits

		# now place the whole chord between the limits
		foreach $ident (@idents) {
			($over, $under)	= &between ($ident, $nexus, $lolim, $hilim);
			&generatePitch ($over, $under, $octave, $ident); # send to score
		}
	}
	else { # Utonality

		# put the "root" between 1/2 and 1/1
		($over, $under) = &between ($nexus, $idents[$ix], 1/2, 1);
		$hilim = $over/$under; $lolim = $hilim/2; # compute limits

		# now place the whole chord between the limits
		foreach $ident (@idents) {
			($over, $under)	= &between ($nexus, $ident, $lolim, $hilim);
			&generatePitch ($over, $under, $octave, $ident); # send to score
		}
	}
}

sub generatePitch { # send a pitch out to the score
	my ($over) = shift (@_);
	my ($under) = shift (@_);
	my ($octave) = shift (@_);
	my ($ident) = shift (@_);
	my ($pitch) = 392*$octave*$over/$under;
	my ($vowel) = int (rand (4)) + 1; # vowel number

	my ($inst) = "N${ident}"; # instrument name is Identity with N in front :-)
	print SCORE "${clock} voice1 ${duration} ${pitch} ${vowel} 1.0\n";
	my ($balance) = rand (1); # random position
	print SCORE "${clock} control bal ${balance}\n"; # send it out
}

sub OUSwitch { # switch from Otonality to Utonality or vice versa
	print "\nOUSwitch\n";
	print LOG "\nOUSwitch\n";
	$OUSwitch *= -1;
}

sub octave { # make an octave jump
	print "\noctave ";
	print LOG "\noctave ";
	if (int (rand (2))) { # down an octave?
		if ($octave > $looctave) { # room below?
			print "down\n";
			print LOG "down\n";
			$octave /= 2; # descend
		}
		else { # no room: go up instead
			print "up\n";
			print LOG "up\n";
			$octave *= 2; # ascend
		}
	}
	else { # up an octave
		if ($octave < $hioctave) { # room above?
			print "up\n";
			print LOG "up\n";
			$octave *= 2; # ascend
		}
		else { # no room: go down instead
			print "down\n";
			print LOG "down\n";
			$octave /= 2; # descend
		}
	}
}

sub nexus { # change Numerary Nexus
	my (@nexi); my ($ix); # allocate working variables
	foreach $ix (@identlist) { # collect list of possibles
		push (@nexi, $ix) if $ix != $nexus;
	}
	$ix = int (rand ($nidents - 1)); # pick one at random
	$nexus = $nexi[$ix]; # pick new Numerary Nexus
	print "\nnexus ${nexus}\n";
	print LOG "\nnexus ${nexus}\n";
}

sub idents { # change Identities
	print "\nidents ";
	print LOG "\nidents ";
	my ($count, $ix); # working variables
	$count = 1 + $#idents; # number of Identities
	if (int (rand (2))) { # are we dropping one?
		if ($count > $lochord) { # enough to drop one?	
			$ix = int (rand ($count)); # index of dropped Identity
			print "drop ${idents[$ix]}\n";
			print LOG "drop ${idents[$ix]}\n";
			splice (@idents, $ix, 1); # yank out the Identity
		}	
		else { # add one
			while (1) { # we'll exit with "last" when we get a hit
				$ix = 2*int (rand ($nidents)) + 1; # pick an Identity at random
				$count = grep (/^${ix}$/, @idents); # search for it
				last if $count == 0; # not found -- we're done
			}
			print "add ${ix}\n";
			print LOG "add ${ix}\n";
			push (@idents, $ix); # tack new Identity on the end
		}	
	}
	else { # adding one
		if ($count < $hichord) { # room for one more?	
			while (1) { # we'll exit with "last" when we get a hit
				$ix = 2*int (rand ($nidents)) + 1; # pick an Identity at random
				$count = grep (/^${ix}$/, @idents); # search for it
				last if $count == 0; # not found -- we're done
			}
			print "add ${ix}\n";
			print LOG "add ${ix}\n";
			push (@idents, $ix); # tack new Identity on the end
		}	
		else { # drop one
			$ix = int (rand ($count)); # index of dropped Identity
			print "drop ${idents[$ix]}\n";
			print LOG "drop ${idents[$ix]}\n";
			splice (@idents, $ix, 1); # yank out the Identity
		}	
	}
}

sub between { # adjust a ratio so it lies between two limits
	my ($over) = shift (@_); # numerator
	my ($under) = shift (@_); # denominator
	my ($lower) = shift (@_); # lower limit
	my ($upper) = shift (@_); # upper limit
	while ($over/$under < $lower) { # force >= $lower
		$over *= 2;
	}
	while ($over/$under > $upper) { # force <= $upper
		$under *=2;
	}
	return ($over, $under);
}

sub render { # actually play the piece
	my ($orcfile) = "-orc HarryIannis.saol";
	my ($scofile) = "-sco HarryIannis.sasl";
	my ($wavfile) = "-aout HarryIannis.wav";
	my ($mp4file) = "-bitout HarryIannis.mp4";
	my ($command) =
		"sfront -fixedseed ${mp4file} ${wavfile} ${orcfile} ${scofile}";
	system ("${command}"); # compile orchestra and score
	system ("gcc -O3 sa.c -lm -o sa"); # compile the sfront output
	system ("sa"); # generate the WAV

	# now play the "wav" if the right OS is present
	system ("start HarryIannis.wav") if $Config{'osname'} eq "MSWin32";
	system ("play HarryIannis.wav") if $Config{'osname'} eq "linux";
	# add others here if desired
}

