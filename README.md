# `reason-wall-demo`

Reason example of using [wall](https://github.com/let-def/wall) vector drawing.

![Wall Demo](./Demo.gif)


## Build:
Clone, and then run these commands:
```
npm install -g esy@next
cd reason-wall-demo
esy install
esy build
```

## Run The Built App
```
./_build/default/bin/Example.exe
```

## Release As Plain `npm`/`yarn` Package:

This command will package your app into a plain `npm`/`yarn` package that
doesn't require `esy`, and will include all prebuilt binaries, and dynamically
loaded libraries.

```
esy release bin
npm install -g _release/bin-darwin
```

### Origins:

`wall` is a port of NanoVG. `bin/Example.re` is a conversion of the example
file from the `wall` repo. See `ORIGINS.md`.


