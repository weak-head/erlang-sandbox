{application, usr,
  [{description, "Database Service"},
   {vsn, "1.0"},
   {modules, [usr, usr_db, usr_sup, usr_app]},
   {registered, [usr, usr_sup]},
   {application, [kernel, stdlib]},
   {env, [{dets_name, "usrDb"}]},
   {mod, {usr_app, []}}
  ]
}