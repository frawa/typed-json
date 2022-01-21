# Realising

For now, relasing is done manually.
(See https://docs.scala-lang.org/overviews/contributors/index.html.)

Choose version:

```bash
git describe
VERSION=0.0.1
git tag -am releasing v$VERSION
```

And publish:

```bash
sbt publishSigned
sbt sonatypeOpen
sbt sonatypeBundleRelease
git push origin v$VERSION
```

## Reminder to upgrade dependencies

```bash
sbt dependencyUpdates
```
