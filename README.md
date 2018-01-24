# Boids Simulator in Elm

Inspired by...
- [XnaBoids Video](https://www.youtube.com/watch?v=M028vafB0l8)
- [Craig Reynolds' Boids](http://www.red3d.com/cwr/boids/)
- [Conrad Parker's Boid Pseudocode](http://www.kfish.org/boids/pseudocode.html)

To run...

```sh
./bin/run.sh

# ^ That runs the following:
# elm-live src/Main.elm --output=public/js/main.js --dir=public/
```

## TODO

- Performance
  - elm-benchmark for updates
  - chrome or stats.js for view
- 3D
  - Just use vec3 instead of vec2
- Programmatic rules
  - Input -> Func -> (New Angle, Weight)
  - Input = ((Boids .pos dir or .angle within range) -> (sum/avg)) / MousePos dir
- Misc
  - Listen to window resize
  - Toggleable mouse follow
  - Drawable walls/attractors/predators
