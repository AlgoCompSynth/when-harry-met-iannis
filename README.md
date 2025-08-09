# When Harry Met Iannis

[Hear on Bandcamp](https://algocompsynth.bandcamp.com/album/when-harry-met-iannis)

Iannis Xenakis passed away on February 4, 2001. When he arrived in Heaven, he sought out his mentor, Olivier Messiaen. Messiaen said, “Iannis, there’s someone here I want you to meet.” The two walked over to a small sidewalk cafe, and there sat Harry Partch.

And so from this fanciful meeting in Heaven, “When Harry Met Iannis” was born. From Partch, the piece inherits the 43-tone just scale, tuned to G = 392 Hz. Also from Partch, the piece inherits the need to build instruments to play in the Partch scale. The harmonies in “When Harry Met Iannis” are derived from the Tonality Diamond.

Now we have the harmonic structure – the Tonality Diamond. This is where Xenakis comes in. Xenakis was a pioneer in stochastic music – music composed algorithmically by using chance elements. The Perl script that composed “When Harry Met Iannis” was given an initial state – a chord on the Tonality Diamond – and a duration for the piece. All the other elements of the score are derived stochastically by a random walk.

The duration for each note is chosen at random. Whether the note is sounded or silent is chosen at random. And the progression of chords around the Tonality Diamond is chosen at random. At each transition, one of four chord transformations is chosen at random:

* The tonality can change from an Otonality to the corresponding Utonality or vice versa.
* The chord structure can stay the same but be shifted up or down an octave.
* A new Numerary Nexus can be chosen for the chord, with the rest of the structure remaining the same.
* An Odentity or Udentity can be added or deleted, with the rest of the structure remaining the same.

Because some of these transitions can take the piece out of a practical performance range, if the piece goes out of range, the operation is reversed. For example, if the chosen transformation is to go up an octave and the resulting chord would be pitched too high, the next chord is instead chosen by going down one octave. If removing an Odentity or Udentity would leave an empty chord, one is added instead.

Once the score has been computed, the piece is rendered. For this piece, I used a little-known digital synthesizer called sfront. Sfront is similar in principle to the much-better-known CSound digital synthesizer, but sfront code is vastly easier to read than CSound code. The synthesis consists of sung vowels with reverberation, and the position of the chord from left to right between the two stereo channels is chosen at random.

* Source code: https://gitlab.com/znmeb/when-harry-met-iannis/tree/master
* Partch - Genesis of a Music: <https:www.amazon.com/Genesis-Music-Account-Creative-Fulfillments-ebook/dp/B00292BH9A>
* Xenakis - Formalized Music: <https:www.amazon.com/Formalized-Music-Mathematics-Composition-Harmonologia/dp/1576470792>
* sfront: <https://john-lazzaro.github.io/sa/>
