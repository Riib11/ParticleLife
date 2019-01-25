# ParticleLife

A life-like simulation with discrete particle-velocity updates in float-space.

_Primordial Particle Systems_ by IZGartLife [web](http://zool33.uni-graz.at/artlife/PPS)
_What is Chemical Evolution_ introduced by Stated Clearly [video](https://www.youtube.com/watch?v=mRzxTzKIsp8)

## Compilation

*Prerequisites*: [GHC](https://www.haskell.org/ghc/), [Gloss](https://hackage.haskell.org/package/gloss).

*Execution*: navigate to `ParticleLife/` and execute the following:

```bash
./compile
```

## Running

Navigate to `ParticleLife/` and execute the following:

```bash
./particlelife
```

## TODO

* [x] parallelize updates [[pdf](www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parallel_haskell2.pdf)]
* [x] aesthetic color scheme (`Particle -> Color`)
* [ ] literate documentation
* [ ] command-line options for number of cores, number of particles, initial particle configurations, etc.
* [ ] save Pictures to image files
