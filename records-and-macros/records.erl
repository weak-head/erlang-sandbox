-module(records).

-export([do_some_verification/0, yet_another_sugarry_stuff/0]).
-export([verify_signature/1, verify_identity/1]).

-record(credentials, {login, password, identity=none, digital_signature=none}).
-record(identity, {fingerpint=none, footprint=none}).

verify_signature(#credentials{digital_signature=Signature} = _Credentials) ->
    case Signature of
        none -> untrusted;
        _    -> ok
    end.

verify_identity(#credentials{identity=#identity{fingerpint=Fingerpint, footprint=Footprint}}) ->
    if
        (Fingerpint == none) and (Footprint == none) ->
            untrusted;

        true ->
            ok
    end;
verify_identity(_) -> untrusted.

do_some_verification() ->
    Trusted = #credentials {
        login = "login",
        password = "pwd",
        identity = #identity {
            fingerpint = "8AAF0",
            footprint  = "0xFFDE"
        },
        digital_signature = "FXDESIGN"
    },

    Untrusted = #credentials {
        login = "login",
        password = "pwd"
    },

    {
        { trusted_user,
            {signature, verify_signature(Trusted), Trusted#credentials.digital_signature},
            {identity, verify_identity(Trusted), (Trusted#credentials.identity)#identity.fingerpint}
        },
        { untrusted_user,
            {signature, verify_signature(Untrusted)},
            {identity, verify_identity(Untrusted)}
        }
    }.

yet_another_sugarry_stuff() ->
    Trusted = #credentials {
        login = "login",
        password = "pwd",
        identity = #identity {
            fingerpint = "8AAF0",
            footprint  = "0xFFDE"
        },
        digital_signature = "FXDESIGN"
    },

    unsweet_the_syntactic_sugar(Trusted).

unsweet_the_syntactic_sugar({credentials, Login, Password, {identity, Fingerpint, Footprint}, DigitalSignature}) ->
    {Login, Password, Fingerpint, Footprint, DigitalSignature}.