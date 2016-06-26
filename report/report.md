*183.653 Methodisches, industrielles Software-Engineering mit Funktionalen Sprachen am Fallbeispiel Haskell*

*Michael Kainer e1325106 (033534)*

*Simon Strassl eXXXXXXX (033534)*

# Superhaskell

TODO TODO TODO insert screenshot here

Superhaskell ist ein 2D-Spiel, in dem man einen Hasen von links nach rechts
durch ein prozedural generiertes Level steuern muss. Dabei gibt es keine
Möglichkeit stehen zu bleiben oder rückwärts zu gehen; man wird also immer
dazu gedrängt weiter zu gehen.

Die Möglichkeiten den Hasen zu steuern umfassen:

- Springen ("Jump")
- Schneller Sturzflug nach unten ("Drop")
- Horizontaler Schub im Sprung ("Boost")

Ein Level besteht aus Plattformen in unterschiedlicher Höhe und
unterschiedlichem Abstand.

Die Grafiken wurde nicht selber angefertigt. Statdessen wurden frei verfügbare
Grafiken von http://kenney.nl/ verwendet.

## Architektur

Prinzipiell besteht die Anwendung aus zwei großen Teilen: Einem Thread, der sich
um das Rendering des Spielzustandes kümmert, und ein Thread, der sich um das
Generieren und Fortlaufen der Spielwelt kümmert. Beide Threads kommunizieren
über zwei `IORef`s: Einer der den `InputState`, das ist im wesentlichen der
Zustand der Tasten, beinhaltet und einer der den derzeitigen `GameState`, den
Spielzustand, beinhaltet.

```haskell
run :: IO ()
run = do
  sdlState <- initSDL
  inputStateBox <- newIORef defaultInputState
  gameStateBox <- newIORef initialGameState

  forkIO $ runGameLoop inputStateBox gameStateBox
  runRenderLoop sdlState inputStateBox gameStateBox
```

### Rendering

Das Rendering ist mittels der SDL2-Library und OpenGL umgesetzt, für die es Bindings für
Haskell gibt. Prinzipiell ist der Rendering-Code nichts außergewöhnliches und
würde in einem C-Programm ziemlich ähnlich aussehen. Das liegt insbesondere
daran, dass der gesamte Rendering-Code im `IO`-Monad liegt, da dieses Feature
inheränt Seiteneffektbehaftet ist.

Einzig die Konvertierung des aktuellen Spielzustandes in eine Liste von
Rendering Kommandos (`toRenderList`) ist purer Code.

Aus technischen Gründen muss das Einlesen der Eingaben auch im Rendering-Thread
geschehen.

```haskell
getInputState :: SDLInputState -> InputState -> IO InputState
inputState `deepseq` atomicWrite inputStateBox inputState

gameState <- atomicRead gameStateBox
toRenderList :: GameState -> RenderList
executeRenderList :: SDLRenderingState -> RenderList -> IO ()
```

### Level-Generation

Die Level-Generation erzeugt einerseits rechts vom aktuellen Viewport neue
Objekte im Level und andererseits zerstört sie Objecte, die den Viewport
links verlassen haben. Das umfasst einerseits die Plattformen auf denen der Hase
hüpfen muss und andererseits auch die Wolken die im Hintergrund über die Szene
schweben.

Technisch ist das mithilfe des `Rand`-Monad umgesetzt. Das hat es uns erlaubt
dieses Modul pur zu implementieren aber trotzdem Zugriff auf Zufallszahlen
zu haben.

### Game-Loop

### Entities

GATDs
