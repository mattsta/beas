beas: basic erlang account system
=================================

Status
------
`beas` is a redis-backed user account system for
erlang applications.  Non-erlang applications could
easily use the same user data by reading the
proper redis keys.

Usage
-----
See the tests for comprehensive usage examples.

Building
--------
        rebar get-deps
        rebar compile

Testing
-------
The tests are acceptably extensive to give you an
idea of the patterns behind how everything fits
together.

        rebar eunit skip_deps=true suite=beas

Next Steps
----------
Show more real-world examples of beas in action.
Use better-than cycling sha for password encoding.
