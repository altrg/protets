Protets
=======

Bullet-proof ETS server. The goal is to build a little supporter application to make you not to worry about any ETS data loss.

Actually, you WILL lose your data if:

* process-owner of your ETS table will die *and*
* process `protets_mon` will die *and*
* this will happen in a very short period of time

As soon as `protets_mon` module is very simple, probability of such a bad set of circumstances is very low. However, if this happened to you - it is a very bad day. According to the law of compensation of bad luck some awesome things will happen to you tomorrow.

How it works?
-------------
This thing it pretty simple, but some of you will find it shitty.

When you create a new table with Protets, it will be created with `protets_worker` as owner. You will get access to this table because it will be created as public. Yes, this sucks. I have some ideas how to fix this.

However, if actual ETA table owner dies, monitor will get this table transferred. It will create new worker and transfer table back, so your data will not be lost.

So you can try to kill some of `protets_worker` processes, or even `protets_mon`. Your data is safe.

API
---

```erlang
protets:get(Name[, Opts])
```
Creates ETS table (or returns back saved one) (see `ets:new/1` or `ets:new/2`).

`Name` is any allowed erlang term to name a table.

`Opts` are options of ETS table. All options are supported excluding:

* `{heir, Pid, Data}` for obvious reasons.
* `protected`, `public`, `private` are also filtered.


```erlang
protets:drop(Name)
```
Drops down ETS table (see `ets:delete/1`).

`Name` is any allowed erlang term to name a table.

**Please, be sure you have called `protets:drop/1` before killing process permanently. If you dont, this may cause memory leaks. Huge memory leaks.**

Usage
-----
I'll try to show you some:

```erlang
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
```

License
-------
Are you serious? License?..

Ok, this code is licensed under WTFPL.