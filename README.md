kvdb_android
============

#### Key/Value Database Backend for Android - Mnesia Based


The application is designed for storing data into a mnesia database backend on a single Android device, using erlang
as the main backend for controlling the stream of data in actions over database as:

  * get
  * put
  * update
  * delete
  * multi-delete

The erlang base system must be installed on the Android device before install the backend, to install erlang get the apk for the platform. The apk installs erlang and many libraries such as mnesia.
To ensure erlang is installed this path must be accesible by the system:

> /data/data/com.ernovation.erlangforandroid/files/erlang/erts-5.10.1

Installing
==========

To install kvdb_android backend database just download this repository and copy to the sdcard on the desired device, then compile and start the backend from any terminal application:

> WARNING: Terminal app must be started as superuser (root).

First move to the /mnt/sdcard/kvdb_android directory:

```sh
	$ cd /mnt/sdcard/kvdb_android
```
	
Now compile and start the backend by typing two commands:

```sh
	$ sh kvdb-android-tool compile
```
```sh	
	$ sh kvdb-android-tool start
```
	
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
	
Method could be values as: ```<<"GET">>, <<"POST">>, <<"PUT">>, <<"DEL">>```, and Body is a binary string containing a valid data (example: json, xml or plain text).
Then in your Module:Function/2 you must treat the data to execute actions on the backend.


Api usage
=========

### Storing new data

To store data into kvdb android backend use a table name already created and a key with its attributes.

> Before store data you must have defined the table, key and attributes

```erlang
	4> kvdb_android:put(your_table, [{yourkey, "xyz"}, {data1, 2}, {data2, "another string"}, {data3, another_atom}]).
	{ok,<<"ok">>}	
```
### Fetching data 

To retrieve data back from kvdb android backend there are many cases or queries that can be used. The data back is returned as an erlang list.

**fetching all attributes of all records from a table**
```erlang
	5> kvdb_android:get(your_table, all, none, none).
	{ok,[{your_table,"xyz",2,"another string",another_atom},
     	{your_table,"abc",1,"this is a string",this_is_an_atom}]}
```
**fetching one attribute of all records from a table**
```erlang
	7> kvdb_android:get(your_table, data2, none, none).
	{ok,["another string","this is a string"]}
```
**fetching all records from a table matching data**
```erlang
	8> kvdb_android:get(your_table, all, [yourkey], ["xyz"]).
	{ok,[{your_table,"xyz",2,"another string",another_atom}]}
```
**fetching one attribute from a table matching data**
```erlang
	9> kvdb_android:get(your_table, data1, [yourkey], ["abc"]).
	{ok,[1]}
```
**fetching with multiple matching (AND)**
```erlang
	11> kvdb_android:get(your_table, data1, [yourkey, data1], ["abc", 1]).
	{ok,[1]}
```
**fetching when a record no exists in the backend**
```erlang
	10> kvdb_android:get(your_table, data1, [yourkey, data1], ["abc", 2]).
	{ok,<<"error_notfound">>}
```
### Updating data

For updating data there are two options: replace or append data. The replace is executed with any data type but append just use on string and number data types.

**replace an attribute**
```erlang
	13> kvdb_android:update(replace, your_table, data1, 3, yourkey, "abc").
	{ok,<<"ok">>}
```
**append an attribute**
```erlang
	16> kvdb_android:update(append, your_table, data1, 3, yourkey, "abc").
	{ok,<<"ok">>}
```
### Deleting data

To delete data the table name and key for the record must be provided to the API
```erlang
	19> kvdb_android:delete(your_table, "abc").
	{ok,<<"ok">>}
```
The API must be used inside a target module for making a correct query to the backend, as the logical treating of the incoming request and data.

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





