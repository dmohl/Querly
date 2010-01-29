{application, ecouch,
 [
  {description, "CouchDb API"},
  {vsn, "0.1"},
  {id, "ecouch"},
  {modules,      [ec_listener, ec_client, rfc4627]},
  {registered,   [ec_client_sup, ec_listener]},
  {applications, [kernel, stdlib, inets]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {ecouch, {"127.0.0.1", "5984", [], []}}},
  {env, []}
 ]
}.
