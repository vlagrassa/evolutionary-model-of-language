# An Evolutionary Model of Language in a Population

Special thanks to Professor Kennedy! This was my final project for his class Code Making, Code Breaking (LING 26040) taught in Fall 2019.

This project adapts equations and ideas described in "Evolutionary Biology of Language" by Martin A Nowak, as outlined below.


# Project Write-Up

The following is copied from my writeup describing the project.



## 1. Inspiration
The model developed in this project was inspired by the paper "Evolutionary Biology of Language" by Martin A Nowak. In this project, I adapt the following ideas from the paper:

- The model of an organism as an association matrix between signals and objects
- That from generation to generation, this association matrix is passed on from parent to child with some chance of error
- That errors in communication are defined by some distance metric between different signals

Specifically, I adapt equations from sections 2 and 3 to produce a simulation of a population of organisms.
Before running this simulation, my hypothesis based on Nowak’s work and my own intuition was that the evolution of the system would pair the least ambiguous signals with the events that have the highest payoff. While the simulation I have build does not quite support this yet, I am optimistic that a better model could produce results more in line with this prediction.



## 2. Verbal Signals (`signals.hs`)

One of the most interesting ideas proposed by Nowak is that the error in signal communication might be defined in terms of some distance metric between possible signals. Since this is not expanded on in the paper, I decided to create my own rough model of the distance between signals. To do so, I defined a few basic features of consonants and vowels:

Consonants are defined by the following three criteria:

| Feature                | Possible Values                                   |
|------------------------|---------------------------------------------------|
| Place of Articulation  | `Labial`, `Coronal`, `Velar`, or `Glottal`        |
| Manner of Articulation | `Plosive`, `Nasal`, `Fricative`, or `Approximant` |
| Voice Onset            | `Voiced`, `Unvoiced`, `Aspirated`, or `Ejective`  |

_(Note that I use “Voice Onset” a bit more broadly than its standard use, to include ejective consonants)._

Vowels are defined by the following four criteria:

| Feature   | Representation                                                                              |
|-----------|---------------------------------------------------------------------------------------------|
| Height    | A decimal value between `0` and `1`, where `0` is the lowest tongue position and `1` is the highest |
| Frontness | A decimal value between `0` and `1`, where `0` is the furthest back and `1` is the furthest forward |
| Tenseness | A Boolean value, where `True` is tense and `False` is lax
| Rounded   | A Boolean value, where `True` is rounded and `False` is unrounded

I use a simplified metric for consonant distance, which at its core is the weighted sum of the differences between its three features. I use a similar metric for vowel distance, except that height and frontness are combined into a single measure that represents the vowel’s position in vowel space.

For simplicity, place, manner, tenseness, and roundness are all defined to have a distance of 1 if they are not equal, and a distance of 0 if they are. Voice onset is slightly more complicated, where each possible pairing of values has its own uniquely defined distance. The values for these are estimated, under the idea that voiced – unvoiced – aspirated form a spectrum and ejective is closest to aspirated; in a more accurate model, these values could be more rigorously calculated by comparing spectrograms.

The most complicated calculation is the distance between vowels in vowel space. To calculate this, I began with the idea that the distances between /a/ and /i/, between /i/ and /u/, and between /u/ and /α/ are all roughly equivalent, but that the distance along the bottom edge between /a/ and /α/ is only about half that distance. More formally, I decided that if two vowels have the same frontness value, then their distance in vowel space should be the difference of their height values. This made the calculation slightly more complicated than simply placing them on a trapezoid, since the distance between any point along the top of the space and another point equally far along the bottom of the space should always be the same. If you apply the Pythagorean theorem to this model, the value of the hypotenuse is different depending on whether the horizontal line is on the top or the bottom; I define the distance to be the average of these two values.

Consonants and vowels are combined into CVC syllables to produce the actual signals used in the simulation. The distance between syllables is once again the weighted sum of the distance of the components, where the vowel is weighted twice as much as either consonant.

In addition to the core calculations mentioned above, I wanted a steep decrease in the similarity between signals as opposed to a purely linear relationship. To this end, take the fourth, third, and square roots of the above calculations for syllables, vowels, and consonants, respectively. In the figure below, the blue line represents a linear relationship, the green the scaling for consonants, the red for vowels, and the orange for syllables. The x-axis represents the value of the weighted sum, and the y-axis represents the distance produced.

An interactive version of this plot can be found [here](https://www.desmos.com/calculator/qckpgagu9t).



## 3. Objects (`objects.hs`)

I use the term object to refer to anything which may be communicated by a signal, be it a physical object, an event, a situation, etc. In this simulation, I defined objects to have a frequency of occurrence and a payoff for accurate communication.



## 4. Initialization Parameters (`vars.hs`)

In this file, I store the parameters of the simulation:

| Variable | Description | Value |
|----------|-------------|-------|
| `signals`, `num_signals` | The list of signals used for communication between organisms, and the size of that list. | See code |
| `objects`, `num_objects` | The list of objects communicated about by organisms, and the size of that list. | See code |
| `err_add` | The probability that a child organism will form a new association that its parent lacks. Described by Nowak in section §2c as _w<sub>0</sub>_. | `0.01` |
| `err_rem` | The probability that a child organism will lose an association that its parent has. Described by Nowak in section §2c as _w<sub>1</sub>_. | `0.0005` |
| `reproduction_rate` | The maximum number of children that the most fit organism can have. | `3.0` |
| `carrying_capacity` | The maximum number of organisms in the population. This is to help keep the calculation manageable. | `30` |
| `simil_matrix` | The similarity between different signals, calculated using the distance metrics described above. Described by Nowak in §3. | See code |
| `error_matrix` | The likelihood that a given signal `i` will be (mis)heard as signal `j`. This is the similarity matrix normalized across its rows. Described by Nowak in §3. | See code |



## 5. Organisms and Populations (`organism.hs`)

Following Nowak’s model, I define an individual organism to be an association matrix between signals and objects. I calculate Nowak’s `P` and `Q` matrices with the functions `send_matrix` and `hear_matrix`, respectively, which are the association matrix normalized across its rows and across its columns. The element `(i,j)` in the `send_matrix` is the probability that the given organism will use the signal `i` to refer to object `j`; similarly, the element `(i,j)` in the `hear_matrix` is the probability that the organism will interpret the signal `i` to mean the object `j`. These, along with the error matrix, are used to calculate the probability of successful communication. (See equation 8 and `organism.hs` line 71 for details.)

In this file, I also define a population to be a list of organisms. I define the function `avg_association` to return the average association matrix of a given population. Here, I wanted to expand on Nowak’s evolutionary model by calculating the fitness for each possible association in a population at any given time. To do this, I had to define my own notion of fitness for an association; I define this to be the probability that the corresponding signal will be interpreted as each object, times the payoff for identifying each object. The idea is that the fitness of having a given association is defined in terms of how much payoff there will be in using it.



## 6. Payoffs and Fitness (`fitness.hs`)

In this file, I define a number of functions to calculate variations of fitness. The core ideas are in the functions `payoff` and `fitness`:

- The `payoff` function gives the payoff of two organisms communicating any arbitrary message with one another. This is adapted from Nowak’s function _F_ defined in equations (2) through (4); it also includes the notion of error in communication introduced in equation (8).

- The `fitness` function gives the average payoff of communication between any two organisms in a population. It is adapted from Nowak’s term φ described in equation (7).

The other functions in this file are variants on and applications of these two functions. In particular, most functions have a normalized version suffixed with `_n` that normalizes a given metric by the maximum possible payoff, which Nowak calculates to be the minimum of the number of signals and of objects.



## 7. Creating Children (`children.hs`)

In this file, I define a number of functions used to generate children from a parent organism. These functions move away from Nowak’s work somewhat, since they deal with the specifics of creating new organisms. To create a new child, the parent’s association matrix is modified based on the reproduction error terms described above and the fitness of each association.

Specifically, for each association, a random number is generated between 0 and 1. This number is then divided by 1 minus the fitness of that association, calculated as described above. If the fitness is close to 0, this value will be close to 1, meaning the random number will stay mostly the same; if the fitness is close to 1, this value will be close to 0, making the random number progressively larger. The error values are then used as thresholds, such that if the random number is lower than them, an association is gained or lost; thus, more fit associations are less likely to be changed.

The number of children each organism has is determined by multiplying their relative fitness within the population by the reproduction rate, generating a random number between that and 0, an truncating the resulting value. Thus, if the reproduction rate is 3.0 and the organism’s fitness is 0.5, then a random number will be generated between 0 and 1.5; if this value is, say, 1.38, then the organism will have 1 child.



## 8. Generations (`main.hs`)

The simulation is run by generating new populations of child organisms from the current generation. Children are generated as described above. To give the more fit organisms a higher chance of reproducing, the current generation is sorted in order of fitness, and children are then generated moving down this list until the carrying capacity is reached. At each step, the current generation is summarized by the `GenSummary` type, which tracks a few key metrics about the population: its number, its size, its average and highest fitness, and its average and most fit association matrix. To produce randomized results at each step, I used Haskell’s State monad to pass a random generator from one step to the next.



## 9. Running the Simulation

In the `main` file, I have included a sample population in the variable pop. A simulation can be run by opening `GHCi` and typing the command:

    (summaries, final_gen) <- test_sim n gen

...where `n` is the desired number of generations to run, and `gen` is one of the test generations provided in the file. The variable summaries will now contain the list of `GenSummary` objects from the simulation. These can be printed to the screen by typing

    summaries



## 10. Limitations and Discussion

While most of the functions described throughout this paper work as expected, the simulation itself does not always produce the desired results. It seems that, especially with this data, more fit populations arise not by the evolution of a more efficient method of communication, but by organisms with less fit association matrices dying out soon; once most of the population contains a given association matrix, this matrix is unlikely to change very much. Going forward, I would like to investigate whether different initial parameters or a different way of creating the next generation might produce a population that converges on more efficient association matrices.
