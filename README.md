SmartChatRoom 
==========================

[![Smartmesh](https://smartmesh.io/wp-content/uploads/2017/10/logopng%E7%99%BD%E8%89%B2.png)](https://smartmesh.io)  


Smartchatroom is a distributed, fault-tolerant technology that allows the creation
of large-scale instant messaging applications. The server can reliably support
thousands of simultaneous users on a single node and has been designed to
provide exceptional standards of fault tolerance. The SmartChatRoom can be used 
with or without internet, everyone can talk each other.

Key Features
------------

- **chat with or without internet**  
  client which without internet can send message through the client with internet

- **chat in the same room**  
  the users with or without internet chat in the same chatroom

Additional Features
-------------------

- **Modularity**
  - Load only the modules you want.
  - Extend SmartChatRoom with your own custom modules.

Quickstart guide
----------------

### 0. Requirements

To compile SmartChatRoom you need:

 - GNU Make.
 - GCC.
 - Libexpat 1.95 or higher.
 - Libyaml 0.1.4 or higher.
 - Erlang/OTP 17.1 or higher.
 - OpenSSL 1.0.0 or higher, for STARTTLS, SASL and SSL encryption.
 - Zlib 1.2.3 or higher, for Stream Compression support (XEP-0138). Optional.
 - PAM library. Optional. For Pluggable Authentication Modules (PAM).
 - GNU Iconv 1.8 or higher, for the IRC Transport (mod_irc). Optional. Not
   needed on systems with GNU Libc.


### 1. Compile and install on *nix systems

To compile SmartChatRoom, execute the following commands.  The first one is only
necessary if your source tree didn't come with a `configure` script.

    ./configure
    make

To install SmartChatRoom, run this command with system administrator rights (root
user):

    sudo make install

### 2. Start SmartChatRoom

You can use the `ejabberdctl` command line administration script to
start and stop ejabberd. For example:

    ejabberdctl start


For detailed information please refer to the ejabberd Installation and
Operation Guide available online and in the `doc` directory of the source
tarball.


Development
-----------

To start SmartChatRoom in development mode from the repository directory, you can
type a command like:

    erl -pa ebin -pa deps/*/ebin -pa test -pa ebin/ -s mod_muc_room

Links
-----

- Community site: https://www.ejabberd.im
