{application,protets,
             [{description,"Protected ETS server"},
              {vsn,"0.1"},
              {registered,[protets]},
              {applications,[kernel,stdlib]},
              {mod,{protets_app,[]}},
              {env,[]},
              {modules,[protets,protets_app,protets_mon,protets_sup,
                        protets_supersup,protets_worker]}]}.
