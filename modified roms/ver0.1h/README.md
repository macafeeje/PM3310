PM3310 ROM ver0.1h release notes;

Replaces stock ROM's D403 & D407, original Philips partnumbers 99214 & 99251. 
Scopes came with D403 in 16k or 32k EEPROM's, however only the upper 16k is accessible. 
With exception of D407, which is 32k and only present on scopes with a IEC bus interface card A14. 

Modifications:

Fixed Trigger Delay Mode

- Before modification; the trigger delay value automatically increments/decrements with a change in the timebase
This prevents expanding the timebase on a triggered edge (without manually readjusting)

- After modification; a negative trigger delay value can be fixed in place
Timebase can be changed and the trigger value will remain the same
Enabled by press+hold trigger delay UP + DOWN keys for ~1s, until the display flashes "FIXD"
Mode exits (or will not enter) with a non-negative trigger delay
Mode will not work in RECUR / Repetitive Only timebases as the RECUR mode does not support a negative trigger delay on the PM3310 (in sampling mode)

Continous Roll Mode

- Before modification; the roll capture mode would fill each STOx memory in turn before filling the ACCUmulator and stopping. 
To restart sampling, the operator must press CLEAR then the RUN key which repeats the process. 
Because of this, the roll mode becomes quite frustrating to use as you are constantly resetting it.

- After modifiation; by selecting the ROLL and RECURR keys at the same time, the continous roll mod is activated. 
Sampling will continue indefinitly until the ROLL key is pressed (which physically disengages the RECURR key) and the ACCUmulator is immediatley stored to the STOx memory. 
The scope will now continue as per the original roll mode described above. 


Software 10x Probe

- Before modification; an original Philips 10x probe had a special ring around the BNC connector which indicated to the scope that a 10x probe was being used. 
The PM3310 would switch automatically the V/DIV displays and AMPL/DIV displays to apply a 10x correction factor. 
Such probes are somewhat rare these days, and expensive if in good condition with all attaching parts.

- After modification; By turning the desired A or B channel's CAL knob out of CAL and back into CAL, in software the 10x correction as described above will be applied. 
This selection will be preserved in RAM after the scope is powered off and on (if the internal RAM batteries are installed and charged) for ease of use with 10x probes. 


Splash Screen

- Before modification; the alphanumeric displays displayed a filled display as a "lamp test".

- After modification; a purely cosmetic right to left fade in of the text "| PM |3310| ;) |" 
along with the modified ROM version # in the TRIGGER DIV display.
  (code size reduced from version 0.1g)


EEEEEEEEEEEEEEEE

- Before modification; the alphanumeric displays flash "|EEEE|EEEE|EEEE|EEEE|" on shutdown. This annoyed me.

- After modification; the alphanumeric displays are blanked on shutdown, for a more pleasing experience.
