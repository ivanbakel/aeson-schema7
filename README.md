# aeson-schema7

A draft7-compliant Aeson-compatible JSON Schema library for Haskell, using effects.

`aeson-schema7` uses the Polysemy library for effect handling. If you're not familiar with Polysemy, it is not difficult to pick up - but it is strictly necessary in order to parse schemas and validate against them.

## Why use effects?

There are two main reasons: *standards compliance*, and *annotations*.

### Standards compliance

Several parts of the JSON Schema standard make reference to existing standards documents - like for RegEx. In the best case, a Haskell library already exists for handling validation in line with that standard, but often that isn't the case. Rather than force you into standards non-compliance, `aeson-schema7` instead delegates parsing and validation of these effects to the user.

If you feel that exact standard compliance for things like RegEx flavours isn't important, it's easy to define an interpreter for those effects in an existing implementation - the testsuite does exactly that by using `pcre-heavy`.

### Annotations

An annotation is part of a JSON Schema which a schema validation implementation is not allowed to enforce directly - in other words, a semantic part of a schema that `aeson-schema7` can't do anything about. This is often for good reason, such as if you want to define and check your own `format` strings.

Wherever there's an annotation in the JSON Schema specification, `aeson-schema7` has a corresponding effect for handling that annotation - which your code can then pick up to do what it wants with.
