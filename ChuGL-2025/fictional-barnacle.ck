/*

   Test bench for "When Harry Met Iannis 2025"
   CCRMA ChuGL Workshop, August 4 - 8, 2025

   Based on "ChuGL Example Walkthrough - Music for Airports"
   <https://chuck.stanford.edu/chugl/doc/walkthru.html>

   About the name: when you create a new repository on
   GitHub, it suggests "short, memorable" names. So I
   cycled through the suggestions until one resonated
   with my mood, and `fictional-barnacle` was it.
   This script lived in a private repository with
   that name until 2025-08-08.

   Previous version of "When Harry Met Iannis":
   https://algocompsynth.bandcamp.com/album/when-harry-met-iannis

   This file:
   https://github.com/AlgoCompSynth/when-harry-met-iannis/blob/master/ChuGL-2025/fictional-barnacle.ck
   

*/

// This part is mostly copied from the refactored script
// at https://chuck.stanford.edu/chugl/doc/walkthru.html#where-theres-one-there-are-many
// I didn't add reverb because I don't like reverb all that much.

// graphical coupling / animation
fun void addGraphics(Envelope env, float midi) {
    // graphics setup
    GPlane suz --> GG.scene();
    .1 => suz.sca;
    Color.BLACK => suz.color;

    // the value range is different from the original because I'm
    // going from A 220 through A 440
    Math.remap(
        midi,      // value
        56, 70,    // value range
        -2.0, 2.0  // output range 
    ) => suz.posX;

    // graphical loop
    while (true) {
        GG.nextFrame() => now;

        // sync color to volume
        (.1 + env.value()) * Color.WHITE => suz.color;
        // sync vertical scale to volume
        .05 + env.value() => suz.scaY;
    }
}

fun void addVoice(float midi, dur note_dur, dur loop_dur, dur offset) {
    // audio setup
    TriOsc osc => Envelope env => dac;
    .1 => osc.gain;
    Std.mtof(midi) => osc.freq; // set frequency
    note_dur / 2 => env.duration; // note duration

    // couple with graphics
    spork ~ addGraphics(env, midi);

    // wait for initial offset
    offset => now;

    // audio loop
    while (true) {
        env.keyOn(); // open envelope
        note_dur / 2 => now;
        env.keyOff(); // close envelope
        note_dur / 2 => now;

        // wait for remainder of loop
        loop_dur - note_dur => now;
    }
}

// Rather than spork a bunch of constant voice shreds here, I define
// a function to spork a cluster of notes when called. If I'm right
// about the MIDI values in ChucK now being floats instead of ints,
// what I'm creating is a cluster of five notes starting at the given
// MIDI note number and increasing in pitch by 1/5 of a semitone.
//
// Like Harry Partch, I am into microtonality.
fun void spork_cluster (float midi) {
    spork ~ addVoice(midi, 2::second, 3::second, 0::second);
    spork ~ addVoice(midi + 0.2, 1.75::second, 2.5::second, 0::second);
    spork ~ addVoice(midi + 0.4, 1.5::second, 2.0::second, 0::second);
    spork ~ addVoice(midi + 0.6, 1.25::second, 1.5::second, 0::second);
    spork ~ addVoice(midi + 0.8, 1::second, 1::second, 0::second);
}

// Now for the human intervention part. Define the white keys from
// A 220 through A440 inclusive by the keys from A to H. And yes, I
// know "H" is used in some classical music traditions for what we
// call B natural; see any biography of Shostakovich.

// When I press one of those keys I want to spork that cluster. 
// But I only want to spork it once for each note of the A minor
// scale. So I have eight flags, one for each key, initially set
// to zero. And no, I am not calling this "Eight Flags over
// Xenakis".
0 => int a_pressed; // has 'a' been pressed?
0 => int b_pressed;
0 => int c_pressed;
0 => int d_pressed;
0 => int e_pressed;
0 => int f_pressed;
0 => int g_pressed;
0 => int h_pressed; // A above middle C == A 440

// user interaction loop
while (true) {
    GG.nextFrame() => now;   // on every frame...

    // if the key is pressed and has not been pressed yet,
    // spork the cluster and raise the flag!
    if (GWindow.key(GWindow.Key_A) && a_pressed == 0) { 
      spork_cluster(57); // cluster on A below middle C
      1 => a_pressed; 
    }

    if (GWindow.key(GWindow.Key_B) && b_pressed == 0) { 
      spork_cluster(59); // cluster on B below middle C
      1 => b_pressed; 
    }

    if (GWindow.key(GWindow.Key_C) && c_pressed == 0) { 
      spork_cluster(60); // cluster on middle C
      1 => c_pressed; 
    }

    if (GWindow.key(GWindow.Key_D) && d_pressed == 0) { 
      spork_cluster(62); // cluster on D above middle C
      1 => d_pressed; 
    }

    if (GWindow.key(GWindow.Key_E) && e_pressed == 0) { 
      spork_cluster(64); // cluster on E above middle C
      1 => e_pressed; 
    }

    if (GWindow.key(GWindow.Key_F) && f_pressed == 0) { 
      spork_cluster(65); // cluster on F above middle C
      1 => f_pressed; 
    }

    if (GWindow.key(GWindow.Key_G) && g_pressed == 0) { 
      spork_cluster(67); // cluster on G above middle C
      1 => g_pressed; 
    }

    if (GWindow.key(GWindow.Key_H) && h_pressed == 0) { 
      spork_cluster(69); // cluster on A above middle C aka A 440
      1 => a_pressed; 
    }

}

/* 
   How to run:

   1. Open a terminal window.
   2. Put your speaker / headphone volume at zero.
   3. Start the script with `chuck fictional-barnacle.ck`.
   4. Press any key between "A" and "H" to spork one cluster.
   5. Manually adjust the volume to a comfortably low level.
   6. Whenever you feel like it, add in the other seven notes
      in any order. If you hit one that's already there,
      nothing will happen.
   7. Once you have all eight clusters going, when you get
      bored, fade the volume down manually and press `CTL-C`
      to shut everything down.

*/

