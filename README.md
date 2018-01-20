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

Fly towards center of mass
- Returns the average position of nearby boids
Match velocity
- Returns the average velocity of nearby boids
Avoid collisions
- Strongly repel colliding boids
Wiggle!
- Random velocity
