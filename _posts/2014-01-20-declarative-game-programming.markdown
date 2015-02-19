---
layout: post
title: "Declarative Game Programming"
date: 2015-02-18 00:12:22 +0000
comments: true
published: true
categories: haskell frp tutorial
---

Since far long ago I have wanted to get a better understanding of Arrows, and Functional Reactive Programming. After many encounters with the subjects, I finally managed to spend enough time learning about these topics. I am happy to say that I'm no longer in a state of ignorance (if still far from a level I would call proficient).

This tutorial/journal summarizes my knowledge on these topics through the developing a game using the FRP framework ([Netwire 5.0](http://hackage.haskell.org/package/netwire)) and Haskell. I am far from an expert on the subject, so if you have suggestions for improvement, please let me know.

Before we go any further, let me mention that I was inspired by the following [blog post](http://ocharles.org.uk/blog/posts/2013-08-01-getting-started-with-netwire-and-sdl.html) by [ocharles](http://ocharles.org.uk), and due to lack of ideas for a game I decided to create my own version of [this](http://www.helicoptergame.net/).

[Here](https://github.com/andreasg/FRPCopter/) you'll find the full source code for what's presented in this article.


# A Helicopter Game

The main challenge for me when first creating the game was getting my hands around how Netwire actually worked and how the library is composed. The first challenge I tackled was generation of a ceiling and a floor, scrolling across the screen from right to left. The goal is for the ceiling and floor to give the impression that the player are within an infinitely scrolling cave.

Further, we want the player to move along at the same speed as the world/camera, and to always be moving in some direction along the y-axis.

Thus we will need three main components:

 * A player position and trajectory,
 * an infinite list of rectangles for ceiling and floor,
 * a scrolling camera.


# Introducing [Netwire](http://hackage.haskell.org/package/netwire)

Netwire is a domain specific language for functional reactive programming written in Haskell. It gives us the concept of `Wires`. Wires are quite powerful and they incorporate a lot of information, for the purpose of this tutorial we will use a simplified type synonym that we'll call `Wire'`.

```haskell
import Control.Wire
type Wire' a b = (HasTime t s, Monad m, Fractional t) => Wire s t () a b
```

A `Wire' a b` models a continuous behavior that takes input of type `a` and outputs values of type `b`. Wires are either *producing* or *inhibiting* (blocking): this is our switching mechanism, as we'll see when we introduce how Wires are composed.

Netwire (and its Wires) assumes a continuous time-model. For modeling discrete events, such as key-presses or sampling of Wires, we have the concept of an events. An `Event a` represents a value of type `a` at a discrete point in time.

Netwire provides quite a rich language for reasoning about Wires and Events, a few important functions are:

# Included wires

#  `for :: t -> Wire' a a`
Creates a wire that behaves as the identity function (statically passing through its input) for `t` seconds, after which it *inhibits*

# `periodic :: t -> Wire' a (Event a)`
With period `t`, sample the continuous input and generate discrete events with the input value at that time.

# `hold :: Wire' (Event a) a`
As Events are discrete, and our time-model is continuous, if we want to observe the value of an Event, we need to *remember* it, which is exactly what `hold` does. For each `Event a`, `hold` will output the value of the last observed event.

# Switching between wires

 - `w0 --> w1` Creates a wire that behaves as `w0` until `w0` inhibits and then switches to behave as `w1`, freeing up `w0` from memory.

 - `w0 <|> w1` is left-biased selection (behaves as `w0` if its producing, else `w1`).

# Stitching wires together

  - Since Wires instantiates both Arrow and Applicative, we have access to all the nice functions associated with those type-classes as well.

  - Further, Wires can be composed using our normal category operators `(.)` and `>>>`.

# A (very) simple example

To create a step-function that produces 0 for two seconds, and then switches to producing 1 forever,

```haskell
for 2 . pure 0 --> pure 1
```

As we seen above, `for 2` will pass it's input (`pure 0`) through for the specified amount of seconds, and then inhibit. Upon inhibition, `-->` will switch to the second argument, which is a constant wire outputting `1`.


# Creating the Game

For the purpose focusing on Netwire and FRP, we will assume that we have a function `render` to render our game using some graphical Assets. `render` only requires only requires

 - player position,
 - camera position,
 - rectangles for ceiling and floor,

in order to successfully render the game to the screen.

We can represent this using a tuple

```haskell
type Game = (Double            -- camera position
            ,([Rect], [Rect])  -- ceiling and floor
            ,(Double, Double)  -- player position
            )
render :: Assets -> Game -> IO ()
```

# Scrolling the World

Scrolling the world is obviously a function of time, so we can simply make a wire that grows faster than time by multiplying time by a coefficient >1.

```haskell
scroll :: Wire a Double
scroll = arr realToFrac . time * scrollSpeed
```

Where `scrollSpeed :: Num a => a` is a parameter to our game.


# Level generation

Now that we have the concept of scrolling, we can begin to generate the cave. We will create a wire that produces events with the appropriate x-position and height of the rectangles.

We know that we want to generate both x-coordinates and y-coordinates. Of which the x-coordinate will just be the scrolling offset by the screen width (we want to generate the level off-screen). The y-coordinate will be a random value within a given interval, depending on if we are doing the ceiling or the floor.

```haskell
xcoord :: Wire' a (Event Double)
xcoord = periodic 1 . (scroll + screenW)

ycoord :: (Double, Double) -> Wire' a (Event Double)
ycoord interval = stdNoiseR 1 interval seed
 where seed = 1234 -- not very fun to keep this for the final version
                   -- as the same level will be generated on each play,
                   -- but it makes thing simpler for this example.
```

We use the two wires to create a general function for generating rectangle obstacles

``` haskell
obst :: ((Double, Double) -> Rect) -- how to create a rect from
                                   -- an (x,y) value
     -> (Double, Double) -- interval for the height of the rect
     -> Wire' a [Rect]
obst toRect interval = arr (map toRect . flip zip) 
                     . (accumList *** accumList) 
                     . (xcoord &&& ycoord)

-- Accum list accumulates events over time into a list. Note: the list
-- will become strictly larger over time (i.e., it is a space leak).
-- Also note the usage of hold.
accumList :: Wire' (Event a) [a]
accumList = hold . accumE (flip (:)) []
```

We can now easily define our ceiling and floor

```haskell 
ceiling :: Wire' a [Rect]
ceiling = obst toCeiling (0, 200)
  where toCeiling (x,y) = mkRect x 0 scrollSpeed y

floor :: Wire' a [Rect]
floor = obst toFloor (400, 600)
  where toFloor (x,y) = mkRect x y scrollSpeed 200
```

We're now done with generating the cave! On to:

# Player Positioning

As the position of the player depends on receiving input, we first need to model that.

# Input Events

Netwire provides us with two very helpful wires

```haskell
became :: (a -> Bool) -> Wire' a a -- Given a predicate, only produce if it holds true.
between :: Wire' (a, Event b, Event c) -> a -- produce with a between Event b and Event c.
```

Don't confuse Netwire events with system events that we get from SDL:

 - ```SDL.Event```: Events from the operating-system, and
 -  ```Event```: discrete events in time as modeled by Netwire.

```haskell
keyDown :: SDL.Keysym -> Wire' SDL.Event (Event SDL.Event)
keyDown key = became $ \e -> case e of
  (SDL.KeyDown (SDL.Keysym k _ _)) -> k == key; _ -> False

keyUp :: SDL.Keysym -> Wire' SDL.Event (Event SDL.Event)
keyUp key = became $ \e -> case e of
  (SDL.KeyUp (SDL.Keysym k _ _)) -> k == key; _ -> False
```

Writing a Wire that produces while we are pressing Space is now trivial:

```haskell
spacePressed :: Wire' SDL.Event ()
spacePressed = between . arr (\(on, off) -> ((), on, off)) 
             . (keyDown space &&& keyUp space)
 where space = SDL.SDLK_SPACE
```

the output of `spacePressed` is `()`, as we are only interested if this wire is producing or inhibiting, not *what* it is producing.


# Finally, positioning

To compute the player position we need the players velocity over time. this is expressed as either the integral of gravitational acceleration (falling), or a constant upward-pointing velocity-vector. Note the use of scrollSpeed for velocity in x-axis, so that the player will move along with the camera.

```haskell
velocity :: Wire' SDL.Event (Double, Double)
velocity = gravity . until . pressSpace
           --> goUp . spacePressed
           --> velocity
  where gravity = integral (scrollSpeed, 100) . pure (0, 800)
        pressSpace = pure () &&& keyDown SDL.SDLK_SPACE
        goUp = pure (0, 200)
```

The player position is then the integral of the velocity

```haskell 
position :: Wire' SDL.Event (Double, Double)
position = integral startPos . velocity
 where startPos = (200, 200)
```


# Putting it all together

```haskell
game :: Wire SDL.Event Game
game = proc e -> do
  camera    <- scroll   -< ()
  l         <- level    -< ()
  playerPos <- position -< e
  returnA -< (camera, l, playerPos)
```

Now we just need to add some collision detection


```haskell
isColliding :: Wire' ((Double, Double), [Rect]) -> Bool
isColliding = arr $ \(p,rs) -> any (contains p) rs
```

```haskell
game :: Wire SDL.Event Game
game = proc e -> do
  camera    <- scroll   -< ()
  l         <- level    -< ()
  playerPos <- position -< e

  -- check if we are colliding
  colliding <- isColliding -< (p, fst l ++ snd l)

  -- if collision, inhibit the game wire
  when (==False) --> inhibit () -< colliding

  returnA -< (camera, l, playerPos)
```

and we're done with our game.

# Rendering the game and advancing time

Our code above simply implements the logic of our game. We still need a "main loop" that advances our wires and retrieves events from the system. You'll find the full source in the github-repository, but here's the essential part of our `main :: IO ()` function.

```haskell
-- ... 
go assets screen clockSession_ game
  where go as scr s w = do
          e <- SDL.pollEvent
          (ds, s') <- stepSession s
          (res, w') <- stepWire w ds (Right e)
          case res of
            Left _ -> putStrLn "qutting"
            Right gm -> do render scr as gm
                           SDL.flip scr
                           go as scr s' w'
```

As you can see, `go` is a recursive function that in each execution calculates a *session delta* that it in turn uses to /step/ the wires and get the current state of the system, stored in `res`.

# End Result

Again, you'll find the full source in my [git-repo](https://github.com/andreasg/FRPCopter/). Check the `tutorial` branch for the code that's presented in this tutorial.

Here's a screenshot of the finished game

![Complete game screenshot](/assets/frp_copter.png)
