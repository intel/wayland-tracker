DISCONTINUATION OF PROJECT.

This project will no longer be maintained by Intel.

Intel has ceased development and contributions including, but not limited to, maintenance, bug fixes, new releases, or updates, to this project. 

Intel no longer accepts patches to this project.

If you have an ongoing need to use this project, are interested in independently developing it, or would like to maintain patches for the open source software community, please create your own fork of this project. 
WAYLAND-TRACKER
===============

Wayland-tracker is a Wayland message protocol dumper, licensed with the Wayland
MIT license.

*Question:* Why use this instead of just setting WAYLAND_DEBUG environment
variable for the application?

*Answer:* Having message tracking outside Wayland library helps debug
difficult-to-catch problems. The wayland-tracker tool does not depend on
application implementation or Wayland library, and it potentially gives more
information about the messages such as the names of the message arguments. Also,
being able to output the messages in JSON format means that you can feed the
results to an external tool that, for instance, counts how many messages needed
to be sent for some application use case or finds warning messages.

Using wayland-tracker
---------------------

Wayland-tracker sits between Wayland server (such as Weston) and a Wayland
client, dumping all message traffic to both directions.

Wayland-tracker has four output modes: `binary`, `simple`, `json` and
`json_pretty`. The simple mode outputs the messages in a format that is very
close to WAYLAND_DEBUG style logging.  The JSON modes output the messages in
JSON format and binary mode outputs the messages in binary format. To use JSON
or simple format, you need to have the protocol description XML files that are
being used by the application and server you are running. The XML protocol files
are provided using `-x` command line option.

For example, command

    wayland-tracker json -x wayland.xml -x xdg-shell.xml -x workspaces.xml -- weston-terminal

might print messages such as these (and much more):

    {"message":{"arguments":[{"value":{"value":3,"type":"NewId"},"name":"callback"}],"name":"sync","interface":"wl_display","object":1,"type":"Request"},"timestamp":"0.158632s"}
    {"message":{"arguments":[{"value":{"value":2,"type":"NewId"},"name":"registry"}],"name":"get_registry","interface":"wl_display","object":1,"type":"Request"},"timestamp":"0.158632s"}
    {"message":{"arguments":[{"value":{"value":3,"type":"UInt"},"name":"id"}],"name":"delete_id","interface":"wl_display","object":1,"type":"Event"},"timestamp":"0.158947s"}
    {"message":{"arguments":[{"value":{"value":5244,"type":"UInt"},"name":"callback_data"}],"name":"done","interface":"wl_callback","object":3,"type":"Event"},"timestamp":"0.158947s"}

Output mode `json_pretty` uses a JSON pretty-printer to make JSON messages more
human-readable while using exactly the same options as `json`:

    {
        "message": {
            "type": "Request",
            "name": "set_window_geometry",
            "object": 15,
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

Output mode `simple` is used with the following:

    wayland-tracker simple -x wayland.xml -x xdg-shell.xml -x workspaces.xml -- weston-terminal

The simple output looks like this:

    [0.100927s   ]  -> wl_registry@2.bind(3, "wl_subcompositor", 1, new id [unknown]@5)
    [0.100927s   ]  -> wl_registry@2.bind(2, "wl_compositor", 3, new id [unknown]@4)
    [0.102968s   ] <-  xdg_surface@15.configure(0, 0, [04000000], 9969)
    [0.102968s   ] <-  wl_surface@14.enter(object 11)
    [0.102968s   ] <-  wl_display@1.delete_id(17)
    [0.102968s   ] <-  wl_display@1.delete_id(16)
    [0.102968s   ] <-  workspace_manager@13.state(0, 1)
    [0.102968s   ] <-  wl_output@11.done()
    [0.102968s   ] <-  wl_output@11.mode(3, 1024, 640, 60000)

The arrows indicate the message direction (`->` for requests, `<-` for events).
A good place to find the protocol XML files are the Wayland and Weston git
repositiories.

Binary mode doesn't require XML files, since the protocol state is not
tracked. For instance, command

    wayland-tracker binary -- weston-terminal

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

The binary format log contains first the time stamp since program launch, then
parsed Wayland message heades (sender, opcode, and message size). The message
data is then printed out in hexadecimal format split into 32-bit words.

More output modes are expected in the future.

The diagnostic output from wayland-tracker and all of the application output are
redirected to stderr, while the message dump is provided to stdout. This means
that you can redirect the application output elsewhere using the normal command
line semantics:

    wayland-tracker binary -- weston-terminal 2> /dev/null

You can also use command line option `-o` to direct the message dump to a file.

The application and its command line parameters are provided after `--` in the
command line:

    wayland-tracker binary -- weston-terminal --help

Building wayland-tracker using stack
------------------------------------

[First install and set up stack](http://docs.haskellstack.org/) for your
(Linux) platform. After that you can build the software in the source
directory with `stack build` and install it to stack binary installation
directory with `stack install`.

Building wayland-tracker using cabal
------------------------------------

Wayland-tracker is written (mostly) in Haskell. To build the software, you need
to first install ghc, gcc and cabal using your package manager. For instance, in
Fedora 20, you would say:

    sudo yum install cabal-install ghc gcc

In Ubuntu 14.04 a similar command would be:

    sudo apt-get install cabal-install ghc build-essentials

Then, in the source directory update the cabal package database:

    cabal update

Install all dependencies of wayland-tracker:

    cabal install --only-dependencies

And finally configure and build:

    cabal configure
    cabal build

The binary will be created to `dist/build/wayland-tracker/wayland-tracker`. To
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

* "pcap" output mode (maybe using text2pcap tool?)
* use quickcheck for testing parsing and log formats

