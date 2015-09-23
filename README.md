master
======

```
A master builder.
```

![master](http://vignette3.wikia.nocookie.net/lego/images/9/97/VitruviusTheAwesome.jpg/revision/latest?cb=20131123165335)

Concepts
--------

### master

`master` provides a uniform interface to building projects.

`master` is a configuration file definition for defining builds and
referencing a runner.

`master` is a command line interface for building project, but reading
the master configuration, grabbing the runner, setting up the build
environment, and then executing the runner with the environment.

### runners

Any executable. It accepts no arguments, but will have access to
environment variables for this build.

### builds

Basically a build is a name, and a collection of environment variables
for the build.

### `master.toml` configuration

A complete example:

```
runner = "s3://ambiata-dist/master-haskell/$OS/$ARCH/master-haskell-ab12f1"
sha1 = "abc123def"

[build.master]
   PUBLISH = "true"
   PUBLISH_BUCKET = "ambiata-dist"
   HADDOCK = "true"

[build.branches]
   PUBLISH = "false"

[build.cabal]
   runner = "s3://ambiata-dist/master-haskell/$OS/$ARCH/haskell-cabal-ab12f1"
   sha1 = "abc123def"

```

The `runner` attribute is mandatory. It is able to exploit the $OS and $ARCH
variables for platform specific requests. It also is optional on the build
level where it will overwrite the top level `runner`.

The `sha1` is to specify the sha1 of the runner. It is optional but really
should be set. If this is set, master will cache your runner, if it is not
set your colleagues will probably not like you.

Each `build.*` sub-section, defines a build name `*` and specifies a series
of environment variables. Each subkey to be passed to the `runner` must
 be a string in capitals.


Implementation
--------------

The first cut should be extreamlly basic. It should:
 - parse the config.
 - download the runner.
 - check the sha.
 - execute the runner with the environment variables for the specific build set.
