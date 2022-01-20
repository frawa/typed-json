# Realising

For now, relasing is done manually.
(See https://docs.scala-lang.org/overviews/contributors/index.html.)

Choose version:

```bash
git describe
git tag -am releasing v0.0.1
```

And publish:

```bash
sbt publishSigned
sbt sonatypeRelease
```

## Reminder to upgrade dependencies

