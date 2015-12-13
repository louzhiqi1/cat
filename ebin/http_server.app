{application, http_server, [
        {description, "http_server"},
        {vsn, "1.0"},
        {modules, []},
        {registered, [web_server_sup]},
        {applications, [kernel,stdlib,crypto,cowboy]},
        {mod, {http_server_app, []}}
]}.