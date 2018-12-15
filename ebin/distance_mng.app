{application,distance_mng,
             [{description,"Distance manager"},
              {vsn,"0.1.0"},
              {registered,[]},
              {mod,{distance_mng_app,[]}},
              {modules,[distance_mng,distance_mng_app,distance_mng_sup,
                        toppage_h,db_proxy]},
              {applications,[epgsql,cowboy,jsx]},
              {env,[]}]}.
