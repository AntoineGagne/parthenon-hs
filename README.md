# parthenon

[![Build Status](https://github.com/AntoineGagne/parthenon-hs/actions/workflows/haskell.yml/badge.svg)](https://github.com/AntoineGagne/parthenon-hs/actions)

## How to get

### Build from source

After cloning this repository, the following command can be used to build the
binary:

```sh
stack build
```

## Usage

```sh
# Create a schema and put it inside the schemas folder (this folder location
# varies depending on the platform)
$ cat << EOF > ~/.local/share/parthenon/vectors
array<struct<x: double, y: double, z: double>>
EOF

# Use the `vectors` schema
$ parthenon vectors '[{x=1.0, y=2.0, z=3.0}, {x=4.0, y=5.1, z=6.2}]'
[{"x":1,"y":2,"z":3},{"x":4,"y":5.1,"z":6.2}]
```
