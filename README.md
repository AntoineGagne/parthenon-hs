# parthenon

[![Build Status](https://github.com/AntoineGagne/parthenon-hs/actions/workflows/haskell.yml/badge.svg)](https://github.com/AntoineGagne/parthenon-hs/actions)

`parthenon` is a tool that convert Athena terms into JSON values.

## How to get

### Building

#### Build from source

After cloning this repository, the following command can be used to build the
binary:

```sh
stack build
```

#### Build the Docker image

```sh
docker build . -t parthenon:latest
```

## Usage

### With the binary

```sh
# Create a schema
$ cat << EOF > vectors
array<struct<x: double, y: double, z: double>>
EOF

$ cat << EOF > athena-vectors
[{x=1.0, y=1.0, z=1.0}]
EOF

$ echo '[{x=1.0, y=1.0, z=1.0}]' | parthenon @vectors -
[{"x":1,"y":1,"z":1}]
$ echo '[{x=1.0, y=1.0, z=1.0}]' | parthenon 'array<struct<x: double, y: double, z: double>>' -
[{"x":1,"y":1,"z":1}]
$ parthenon 'array<struct<x: double, y: double, z: double>>' '[{x=1.0, y=1.0, z=1.0}]'
[{"x":1,"y":1,"z":1}]
$ parthenon 'array<struct<x: double, y: double, z: double>>' @athena-vectors
[{"x":1,"y":1,"z":1}]
```

### With the Docker image

Using the Docker image is similar to the commands above:

```sh
$ docker run --rm -i parthenon:latest 'array<struct<x: double, y: double, z: double>>' '[{x=1.0, y=1.0, z=1.0}]'
$ echo '[{x=1.0, y=1.0, z=1.0}]' | docker run --rm -i parthenon:latest 'array<struct<x: double, y: double, z: double>>'
```

To make it easier to invoke the image, the following alias can be used:

```sh
alias parthenon='docker run --rm -i parthenon:latest'
```
