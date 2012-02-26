Simple Unicode Collator
=======================

2 functions are public:

* `ucol:compare/2`
* `ucol\_primary:compare/2`

Examples:

```
(ucol@omicron)1> ucol:compare(<<"test">>, <<"test">>).
equal
(ucol@omicron)2> ucol:compare(<<"test?">>, <<"test">>).
greater
(ucol@omicron)3> ucol_primary:compare(<<"test">>, <<"test">>). 
equal
(ucol@omicron)4> ucol_primary:compare(<<"test?">>, <<"test">>).
equal
```

Based on https://github.com/freeakk/ux


Where is `ucol\_unidata`? 

[This](https://github.com/freeakk/ucol\_data) application generates 
`ucol\_unidata` and `ucol\_testdata` modules. These files can be copied into the
ebin directory. 

The feature is that this application do not need `ucol\_data` and `ux` in runtime :)


Tips
----

* Turn off the `native` option in rebar.config file when debugging.
* Turn on the `native` option when release. 
* The application don't need `ucol\_testdata` in runtime.
