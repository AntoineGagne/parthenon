-record(schema_options, {
    null_as = undefined :: atom() | binary()
}).

-type schema_options() :: #schema_options{}.
