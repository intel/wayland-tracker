WAYLAND-TRACKER
===============

Wayland-tracker is a Wayland message protocol dumper, licensed with the Wayland
MIT license.

Using wayland-tracker
---------------------

Wayland-tracker sits between Wayland server (such as Weston) and a Wayland
client, dumping all message traffic to both directions.

For instance, command

    wayland-tracker -t binary -- weston-terminal

might print out this data (and more):

    [0.012587s   ] Request sender=1  op=0  size=12  03000000
    [0.012587s   ] Request sender=1  op=1  size=12  02000000
    [0.012968s   ] Event   sender=1  op=1  size=12  03000000
    [0.012968s   ] Event   sender=3  op=0  size=12  34090000
    [0.012968s   ] Event   sender=2  op=0  size=36  12000000 0e000000 73637265 656e7368 6f6f7465 72006765 01000000
    [0.012968s   ] Event   sender=2  op=0  size=40  11000000 12000000 776f726b 73706163 655f6d61 6e616765 72000000 01000000
    [0.012968s   ] Event   sender=2  op=0  size=32  10000000 0c000000 73637265 656e7361 76657200 01000000
    [0.012968s   ] Event   sender=2  op=0  size=36  0f000000 0e000000 6465736b 746f705f 7368656c 6c000000 02000000
    [0.012968s   ] Event   sender=2  op=0  size=32  0e000000 0a000000 7864675f 7368656c 6c00616e 01000000
    [0.012968s   ] Event   sender=2  op=0  size=32  0d000000 09000000 776c5f73 68656c6c 0070616e 01000000
    [0.012968s   ] Event   sender=2  op=0  size=36  0c000000 0f000000 776c5f69 6e707574 5f70616e 656c0000 01000000
    [0.012968s   ] Event   sender=2  op=0  size=32  0b000000 0a000000 776c5f6f 75747075 74006574 02000000
    [0.012968s   ] Event   sender=2  op=0  size=36  0a000000 10000000 776c5f69 6e707574 5f6d6574 686f6400 01000000

Wayland-tracker has three output modes: `binary`, `json` and `json_pretty`.
Binary mode outputs the wayland messages in binary format, and json mode outputs
the messages in JSON format. More output format options are expected in the
future.

The binary format log contains first the time stamp since program launch, then
parsed Wayland message heades (sender, opcode, and message size). The message
data is then printed out in hexadecimal format split into 32-bit words.

To use JSON format, you need to have the protocol description XML files that are
being used by the application and server you are running. The XML protocol files
are provided using `-x` command line option.

For example, command

    wayland-tracker -t json -x wayland.xml -x xdg-shell.xml -- weston-terminal

might print out this data:

    {"message":{"name":"sync","type":"Request","arguments":[{"name":"callback","value":{"value":3,"type":"NewId"}}],"interface":"wl_display"},"timestamp":"0.072162s"}
    {"message":{"name":"get_registry","type":"Request","arguments":[{"name":"registry","value":{"value":2,"type":"NewId"}}],"interface":"wl_display"},"timestamp":"0.072162s"}
    {"message":{"name":"delete_id","type":"Event","arguments":[{"name":"id","value":{"value":3,"type":"UInt"}}],"interface":"wl_display"},"timestamp":"0.07244s"}
    {"message":{"name":"done","type":"Event","arguments":[{"name":"callback_data","value":{"value":2458,"type":"UInt"}}],"interface":"wl_callback"},"timestamp":"0.07244s"}
    {"message":{"name":"global","type":"Event","arguments":[{"name":"name","value":{"value":18,"type":"UInt"}},{"name":"interface","value":{"value":"screenshooter","type":"UInt"}},{"name":"version","value":{"value":1,"type":"UInt"}}],"interface":"wl_registry"},"timestamp":"0.07244s"}

Output mode `json_pretty` uses a JSON pretty-printer to make JSON messages more
human-readable:

    {
        "message": {
            "type": "Request",
            "name": "set_window_geometry",
            "interface": "xdg_surface",
            "arguments": [
                {
                    "name": "x",
                    "value": {
                        "type": "Int",
                        "value": 32
                    }
                },
                {
                    "name": "y",
                    "value": {
                        "type": "Int",
                        "value": 32
                    }
                },
                {
                    "name": "width",
                    "value": {
                        "type": "Int",
                        "value": 742
                    }
                },
                {
                    "name": "height",
                    "value": {
                        "type": "Int",
                        "value": 427
                    }
                }
            ]
        },
        "timestamp": "0.157951s"
    }

A good place to find the protocol XML files are the Wayland and Weston git
repositiories.

The diagnostic output from wayland-tracker and all of the application output are
redirected to stderr, while the message dump is provided to stdout. This means
that you can redirect the application output elsewhere using the normal command
line semantics:

    wayland-tracker -t binary -- weston-terminal 2> /dev/null

You can also use command line option `-o` to direct the message dump to a file.

The application and its command line parameters are provided after `--` in the
command line:

    wayland-tracker -t binary -- weston-terminal --help


Building wayland-tracker
------------------------

Wayland-tracker is written (mostly) in Haskell. To build the software, you need
to first install ghc, gcc and cabal using your package manager. For instance, in
Fedora 20, you would say:

    sudo yum install cabal-install ghc gcc

In Ubuntu 14.04 a similar command would be:

    sudo yum install cabal-install ghc build-essentials

Then, in the source repository update the cabal package database:

    cabal update

Install all dependencies of wayland-tracker:

    cabal install --only-dependencies

And finally configure and build:

    cabal configure
    cabal build

The binary will be in `dist/build/wayland-tracker/wayland-tracker` directory. To
install it in `$HOME/.cabal/bin/wayland-tracker`, use:

    cabal install

Technical information
---------------------

The low-level Wayland message sending/receiving is using C code that is directly
based on Wayland library code for maximum compatibility. There is no direct
Wayland dependency, however.

Message parsing is done using [Attoparsec](https://github.com/bos/attoparsec).
JSON generation uses [Aeson](https://github.com/bos/aeson).

Future work and improvement ideas
---------------------------------

* ["pcap" output mode] (https://github.com/bos/pcap) for analysing log files with WireShark
* "simple" output mode with human-readable output and one line messages
* use quickcheck for testing parsing and log formats
* handle message parsing and log output in separate OS thread?
* use hashmap instead of trees in static maps (such as interfaces)

