{   
    application, game,
    [   
        {description, "cat application."},   
        {vsn, "0.1"},   
        {modules,	[]},   
        {registered, [game]},   
        {applications, [kernel, stdlib, sasl, cowboy, ranch]},   
        {mod, {game, []}},   
        {start_phases, []} 
	]
}.    
