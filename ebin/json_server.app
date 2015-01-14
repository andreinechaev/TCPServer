{application,json_server,
             [{description,"ikwit server"},
              {vsn,"1.1"},
              {modules,[db_server,ikwit_server,json_server,json_server_app,
                        json_server_sup,security]},
              {registered,[json_server,security,db_server]},
              {applications,[kernel,stdlib]},
              {mod,{json_server_app,[]}},
              {env,[]}]}.
