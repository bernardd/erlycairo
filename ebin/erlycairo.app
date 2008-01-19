{application, erlycairo,
 [{description, "ErlyCairo Server"},
  {vsn, "0.1"},
  {modules, [
    erlycairo,
    erlycairo_server,
    erlycairo_app,
    erlycairo_sup
  ]},
  {registered, []},
  {applications, [
    kernel,
    stdlib
  ]},
  {included_applications, []},
  {env, []},
  {mod, {erlycairo_app, []}}]}.