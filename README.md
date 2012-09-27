protets
=======

Protected ETS server

Usage:

protets:get(foo)            => {ok, TableId}.

protets:get(foo, [Opt])     => {ok, TableId}.

protets:drop(foo)           => ok.

After creation of the ETS table you can try to kill protets_mon (monitor process) or protets_worker (actual ETS owner). Goal of Protets is not allow you to loose your ETS data through process deaths.

I'll try to show you some:

    (protets@shizz-worktop)1> self().
    <0.219.0>

    %% getting new table
    (protets@shizz-worktop)2> protets:get(foo).
    {ok,45087}
    (protets@shizz-worktop)3> ets:insert(45087, {foo, bar}).
    true

    %% data inserted
    (protets@shizz-worktop)4> ets:first(45087).
    foo

    %% killing terminal process
    (protets@shizz-worktop)5> exit(normal).
    ** exception exit: normal
    (protets@shizz-worktop)6> self().
    <0.226.0>

    %% data wasn't lost
    (protets@shizz-worktop)7> ets:first(45087).
    foo

    %% looking for actual ETS owner (little dirty hack here)
    (protets@shizz-worktop)8> ets:lookup('$protets_workers$', foo).

    %% here it is, process <0.222.0>
    [{worker,foo,<0.222.0>,#Ref<0.0.0.2055>}]

    %% kill it with fire
    (protets@shizz-worktop)9> exit(list_to_pid("<0.222.0>"), abnormal_kill).
    true

    %% data wasn't lost
    (protets@shizz-worktop)10> ets:first(45087).
    foo
