\title{
Superhaskell \\
\large 183.653 Methodisches, industrielles Software-Engineering mit Funktionalen Sprachen am Fallbeispiel Haskell
}

\author{
Michael Kainer e1325106 (033534)
\and
Simon Strassl e1326936 (033534)
}

\maketitle

# Superhaskell

![Screenshot](screen.png)

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

Die Grafiken wurde nicht selber angefertigt. Stattdessen wurden frei verfügbare
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

Der Kern des Game-Loops besteht aus drei Elementen:

1. Das Lesen des aktuellen `InputState`s aus der `IORef`.
2. Weiterticken des `GameState`s.
    * Welt generieren/zerstören
    * Entities ticken
    * Entities kollidieren
3. Schreiben des neuen `GameState`s in die `IORef`.

Der resultierende `GameState` wird dann wieder als Input für die nächste Iteration des Loops verwendet.
Ein schöner Nebeneffekt dieser Architektur ist, dass das Spiel jederzeit in einen beliebigen Zustand versetzt werden kann, indem einfach der jeweilige `GameState` retourniert wird (z.B. um einen Neustart zu bewirken muss lediglich der initiale `GameState` retourniert werden, alles andere geschieht von selbst).

### Entities

Jedes Element das auf dem Bildschirm angezeigt wird ist eine Entity (Spieler, Wolken, Score Counter, Plattformen ...).
Jede dieser Entities muss andere Daten speichern (der Spieler braucht z.B. eine Geschwindigkeit, die für die statischen Plattformen irrelevant ist) und hat anderes Verhalten, welches sich in einer unterschiedlichen tick-Funktion äußert.
Dazu kommen dann noch unterschiedliche Rendering Funktionen, Collission Handler, usw.

#### Summentyp

Eine Möglichkeit dieses Problem zu lösen ist, einen einzigen großen Summentyp für alle Entities einzuführen.
```haskell
data Entity = Player PlayerData | Platform PlatformData | ...
data GameState = GameState { gsEntities :: [Entity], ... }
```

Die tick Funktion dispatched dann die jeweiligen Tick Funktionen.
```haskell
eTick :: InputState -> GameState -> Entity -> Entity
eTick is gs (Player d) = tickPlayer is gs d
eTick is gs (Platform d) = tickPlatform is gs d
...
```


#### GATDs

Diese Lösung hat jedoch erfordert, dass es eine große "master"-Tick-Funktion gibt
die zwischen allen Entities die im Spiel existieren dispatched. Insbesondere muss man diese
Funktion jedes mal anpassen, wenn ein neues Entity programmiert wird. Das ist
bei einer kleinen Applikation wie unserer zwar kein Problem aber dennoch sehr
unschön.

Um dieses Problem zu lösen haben wir jedes Entity in ein eigenes Modul verschoben
und jedes Entity Mitglied der Typklasse `IsEntity` gemacht. Dank der Haskell-Spracherweiterung
GADTs ist es möglich diese `IsEntity`-Typen in einen gemeinsamen Typ zusammenzufassen,
ohne jeden konkreten Typen aufzählen zu müssen wie beim Summentypen.

Dieser Datentyp, `Entity`, ist vergleichbar mit einem Upcast in objektorientierten
Sprachen, nur, dass es keine Möglichkeit, ohne zu tricksen, gibt von `Entity`
jemals wieder einen Downcast auf ein konkretes Entity zu schaffen.

```haskell
class IsEntity e where
  eTick :: InputState -> GameState -> e -> e
  eTick _ _ e = e
  eWrap :: e -> Entity
  eWrap = Entity

data Entity where
  Entity :: IsEntity e => e -> Entity -- !!!
instance IsEntity Entity where ...

data Player = Player PlayerData
instance IsEntity Player where ...

data Cloud = Cloud CloudData
instance IsEntity Cloud where ...

data GameState = GameState { gsEntities :: [Entity], ... }
```
