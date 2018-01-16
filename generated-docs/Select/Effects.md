## Module Select.Effects

#### `FX`

``` purescript
type FX e = Aff (Effects e)
```

The primitive Effects type, wrapped in Aff. This is used for convenience over Effects.

#### `Effects`

``` purescript
type Effects e = (dom :: DOM, console :: CONSOLE, ajax :: AJAX, avar :: AVAR, now :: NOW | e)
```

The primitive Effects type. To extend your own component with this type, you might do:
main :: ∀ e. Eff (HalogenEffects (Effects e)) Unit


