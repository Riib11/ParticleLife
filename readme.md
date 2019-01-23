# ParticleLife

A life-like simulation with discrete particle-velocity updates in float-space.

Inspired by IZGartLife: https://www.youtube.com/watch?v=makaJpLvbow&feature=youtu.be
Interesting, psuedo-related video by Stated Clearly: https://www.youtube.com/watch?v=mRzxTzKIsp8

## Compilation

*Requirments*: Glasgow Haskell compiler (ghc), Gloss haskell library (https://hackage.haskell.org/package/gloss).

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

- parallelize updates (www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parallel_haskell2.pdf)
- work on color scheme (`Particle -> Color`)
- literate documentation
