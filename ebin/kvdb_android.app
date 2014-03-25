{application, kvdb_android,
 [
  {description, "Key/Value Database Backend For Android - Mnesia Based"},
  {vsn, "3.0"},
  {modules, [kvdb_android_app, kvdb_android_sup, kvdb_android_agent]},
  {registered, [kvdb_android_app, kvdb_android_sup, kvdb_android_agent]},
  {applications,[ kernel, stdlib]},
  {mod, { kvdb_android_app, []}}
]}.
