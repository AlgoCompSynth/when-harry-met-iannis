// graphical coupling / animation
fun void addGraphics(Envelope env, float midi) {
    // graphics setup
    GPlane suz --> GG.scene();
    .1 => suz.sca;
    Color.BLACK => suz.color;

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

fun void spork_cluster (float midi) {
    spork ~ addVoice(midi, 2::second, 3::second, 0::second);
    spork ~ addVoice(midi + 0.25, 1.75::second, 2.5::second, 0::second);
    spork ~ addVoice(midi + 0.5, 1.5::second, 2.0::second, 0::second);
    spork ~ addVoice(midi + 0.75, 1.25::second, 1.5::second, 0::second);
    spork ~ addVoice(midi + 1.0, 1::second, 1::second, 0::second);
}

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
