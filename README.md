kvdb_android
============

###### Key/Value Database Backend for Android - Mnesia Based


The application is designed for storing data into a mnesia database backend on a single Android device, using erlang
as the main backend for controlling the stream of data in actions over database as:

  * get
  * put
  * update
  * delete
  * multi-delete

The erlang base system must be installed on the Android device before install the backend, to install erlang get the apk for the platform. The apk installs erlang and many libraries such as mnesia.
To ensure erlang is installed this path must be accesible by the system:

  /data/data/com.ernovation.erlangforandroid/files/erlang/erts-5.10.1

Installing
==========

To install kvdb_android backend database just download this repository and copy to the sdcard on the desired device, then compile and start the backend from any terminal application:

WARNING: Terminal app must be started as superuser (root).

First move to the /mnt/sdcard/kvdb_android directory:

	$ cd /mnt/sdcard/kvdb_android
	
Now compile and start the backend by typing two commands:

	$ sh kvdb-android-tool compile
	
	$ sh kvdb-android-tool start
	
This initializes the kvdb_android backend for using from any application (apk).


Tables of buckets
==================

The tables creation are managed for the backend before it starts, in the priv directory of the project there's a file named 'kvdb_backend_tables' which one contains the tables (or bucket) definition and its attributtes.
The file is composed for the next structure:

	{ table_name_or bucket, [key, attribute1, attribute2, ...., attributeN ] }.

Each line in the file describe a single table as above is shown. Add more lines for more tables creation.


Targets
=======

Targets are modules that can process an incoming request of an application (apk). To enable a target just edit the file under priv directory named "kvdb_backend_targets" and add a new target with the next structure:

	{ <<"/identifier">>, {module, function}}.
	
Identifier must be a binary term and is configured when a request is made from the application (apk), module and function are called as an erlang module with a function with arity /2, the first incoming parameter could be the action and the second parameter is the data to be treated:

	Module:Function(Method, Body).
	
Method could be values as: <<"GET">>, <<"POST">>, <<"PUT">>, <<"DEL">>, and Body is a binary string containing a valid data (example: json, xml or plain text).
Then in your Module:Function/2 you must treat the data to execute actions on the backend.


Api usage
=========

To use the kvdb_android_mnesia api accessing to the data then see the module code under src directory.
NOTE: Add examples how to use API must be included in next commits.

The java library interface
===========================

The kvdb_android can be controlled for an external agent from a designed library created in java (jar), connected through a 'non-blocking' socket. To use just load the library into your project.
The example how to use the interface within an example on this project also in the java library documentation can be found.

For more information how to use the library refer to [kvdb_android java library](https://github.com/zgbjgg/kvdb_android_java_lib)


Troubleshooting
===============

The application is designed for connect the backend only in the localhost in the port 2187, however the java library manages this connection, but if the socket crash then a module target revision could be the cause.

Author
======

Jorge Garrido <zgbjgg@gmail.com>





