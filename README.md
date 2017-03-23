# Game of Leaf

Game of Leaf is a version of Conway's Game of Life, implemented in the Elm programming language. I learned an awful lot about functional programming, and I'm looking forward to tinkering with this some more!

Deployed at [gameofleaf.herokuapp.com](http://gameofleaf.herokuapp.com).

To build locally, navigate to the root directory and type:

```bash
elm-reactor
```

Then, in your browser, navigate to [http://localhost:8000/index.html](http://localhost:8000/index.html).

TODO:
* The ability to set the size of the board. I have that in a variable, so should be able to extend that.
* The ability to hit play and have it auto-step. Just step with a timeout, so this should also be fairly easy.
* Some SERIOUS refactoring, as I'm still approaching the Elm nail with a JavaScript hammer.
