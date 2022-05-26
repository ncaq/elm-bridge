module MyTests exposing (..)
-- This module requires the following packages:
-- * bartavelle/json-helpers
-- * NoRedInk/elm-json-decode-pipeline
-- * elm/json
-- * elm-explorations/test

import Dict exposing (Dict, fromList)
import Expect exposing (Expectation, equal)
import Set exposing (Set)
import Json.Decode exposing (field, Value)
import Json.Encode
import Json.Helpers exposing (..)
import String
import Test exposing (Test, describe, test)

newtypeDecode : Test
newtypeDecode = describe "Newtype decoding checks"
              [ ntDecode1
              , ntDecode2
              , ntDecode3
              , ntDecode4
              ]

newtypeEncode : Test
newtypeEncode = describe "Newtype encoding checks"
              [ ntEncode1
              , ntEncode2
              , ntEncode3
              , ntEncode4
              ]

recordDecode : Test
recordDecode = describe "Record decoding checks"
              [ recordDecode1
              , recordDecode2
              ]

recordEncode : Test
recordEncode = describe "Record encoding checks"
              [ recordEncode1
              , recordEncode2
              ]

sumDecode : Test
sumDecode = describe "Sum decoding checks"
              [ sumDecode01
              , sumDecode02
              , sumDecode03
              , sumDecode04
              , sumDecode05
              , sumDecode06
              , sumDecode07
              , sumDecode08
              , sumDecode09
              , sumDecode10
              , sumDecode11
              , sumDecode12
              , sumDecodeUntagged
              ]

sumEncode : Test
sumEncode = describe "Sum encoding checks"
              [ sumEncode01
              , sumEncode02
              , sumEncode03
              , sumEncode04
              , sumEncode05
              , sumEncode06
              , sumEncode07
              , sumEncode08
              , sumEncode09
              , sumEncode10
              , sumEncode11
              , sumEncode12
              , sumEncodeUntagged
              ]

simpleDecode : Test
simpleDecode = describe "Simple records/types decode checks"
                [ simpleDecode01
                , simpleDecode02
                , simpleDecode03
                , simpleDecode04
                , simplerecordDecode01
                , simplerecordDecode02
                , simplerecordDecode03
                , simplerecordDecode04
                ]

simpleEncode : Test
simpleEncode = describe "Simple records/types encode checks"
                [ simpleEncode01
                , simpleEncode02
                , simpleEncode03
                , simpleEncode04
                , simplerecordEncode01
                , simplerecordEncode02
                , simplerecordEncode03
                , simplerecordEncode04
                ]

-- this is done to prevent artificial differences due to object ordering, this won't work with Maybe's though :(
equalHack : String -> String -> Expectation
equalHack a b =
    let remix = Json.Decode.decodeString Json.Decode.value
    in equal (remix a) (remix b)


type Record1 a = Record1
   { foo: Int
   , bar: (Maybe Int)
   , baz: a
   , qux: (Maybe a)
   , jmap: (Dict String Int)
   }

jsonDecRecord1 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Record1 a )
jsonDecRecord1 localDecoder_a =
   Json.Decode.succeed (\pfoo pbar pbaz pqux pjmap -> (Record1 {foo = pfoo, bar = pbar, baz = pbaz, qux = pqux, jmap = pjmap}))
   |> required "foo" (Json.Decode.int)
   |> fnullable "bar" (Json.Decode.int)
   |> required "baz" (localDecoder_a)
   |> fnullable "qux" (localDecoder_a)
   |> required "jmap" (Json.Decode.dict (Json.Decode.int))

jsonEncRecord1 : (a -> Value) -> Record1 a -> Value
jsonEncRecord1 localEncoder_a (Record1 val) =
   Json.Encode.object
   [ ("foo", Json.Encode.int val.foo)
   , ("bar", (maybeEncode (Json.Encode.int)) val.bar)
   , ("baz", localEncoder_a val.baz)
   , ("qux", (maybeEncode (localEncoder_a)) val.qux)
   , ("jmap", (Json.Encode.dict identity (Json.Encode.int)) val.jmap)
   ]



type Record2 a = Record2
   { foo: Int
   , bar: (Maybe Int)
   , baz: a
   , qux: (Maybe a)
   }

jsonDecRecord2 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Record2 a )
jsonDecRecord2 localDecoder_a =
   Json.Decode.succeed (\pfoo pbar pbaz pqux -> (Record2 {foo = pfoo, bar = pbar, baz = pbaz, qux = pqux}))
   |> required "foo" (Json.Decode.int)
   |> fnullable "bar" (Json.Decode.int)
   |> required "baz" (localDecoder_a)
   |> fnullable "qux" (localDecoder_a)

jsonEncRecord2 : (a -> Value) -> Record2 a -> Value
jsonEncRecord2 localEncoder_a (Record2 val) =
   Json.Encode.object
   [ ("foo", Json.Encode.int val.foo)
   , ("bar", (maybeEncode (Json.Encode.int)) val.bar)
   , ("baz", localEncoder_a val.baz)
   , ("qux", (maybeEncode (localEncoder_a)) val.qux)
   ]



type Sum01 a =
    Sum01A a
    | Sum01B (Maybe a)
    | Sum01C a a
    | Sum01D {foo: a}
    | Sum01E {bar: Int, baz: Int}

jsonDecSum01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum01 a )
jsonDecSum01 localDecoder_a =
    let jsonDecDictSum01 = Dict.fromList
            [ ("Sum01A", Json.Decode.lazy (\_ -> Json.Decode.map Sum01A (localDecoder_a)))
            , ("Sum01B", Json.Decode.lazy (\_ -> Json.Decode.map Sum01B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum01C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum01C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum01D", Json.Decode.lazy (\_ -> Json.Decode.map Sum01D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum01E", Json.Decode.lazy (\_ -> Json.Decode.map Sum01E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum01 = Set.fromList ["Sum01D", "Sum01E"]
    in  decodeSumTaggedObject "Sum01" "tag" "content" jsonDecDictSum01 jsonDecObjectSetSum01

jsonEncSum01 : (a -> Value) -> Sum01 a -> Value
jsonEncSum01 localEncoder_a val =
    let keyval v = case v of
                    Sum01A v1 -> ("Sum01A", encodeValue (localEncoder_a v1))
                    Sum01B v1 -> ("Sum01B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum01C v1 v2 -> ("Sum01C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum01D vs -> ("Sum01D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum01E vs -> ("Sum01E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum02 a =
    Sum02A a
    | Sum02B (Maybe a)
    | Sum02C a a
    | Sum02D {foo: a}
    | Sum02E {bar: Int, baz: Int}

jsonDecSum02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum02 a )
jsonDecSum02 localDecoder_a =
    let jsonDecDictSum02 = Dict.fromList
            [ ("Sum02A", Json.Decode.lazy (\_ -> Json.Decode.map Sum02A (localDecoder_a)))
            , ("Sum02B", Json.Decode.lazy (\_ -> Json.Decode.map Sum02B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum02C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum02C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum02D", Json.Decode.lazy (\_ -> Json.Decode.map Sum02D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum02E", Json.Decode.lazy (\_ -> Json.Decode.map Sum02E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum02 = Set.fromList ["Sum02D", "Sum02E"]
    in  decodeSumTaggedObject "Sum02" "tag" "content" jsonDecDictSum02 jsonDecObjectSetSum02

jsonEncSum02 : (a -> Value) -> Sum02 a -> Value
jsonEncSum02 localEncoder_a val =
    let keyval v = case v of
                    Sum02A v1 -> ("Sum02A", encodeValue (localEncoder_a v1))
                    Sum02B v1 -> ("Sum02B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum02C v1 v2 -> ("Sum02C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum02D vs -> ("Sum02D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum02E vs -> ("Sum02E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum03 a =
    Sum03A a
    | Sum03B (Maybe a)
    | Sum03C a a
    | Sum03D {foo: a}
    | Sum03E {bar: Int, baz: Int}

jsonDecSum03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum03 a )
jsonDecSum03 localDecoder_a =
    let jsonDecDictSum03 = Dict.fromList
            [ ("Sum03A", Json.Decode.lazy (\_ -> Json.Decode.map Sum03A (localDecoder_a)))
            , ("Sum03B", Json.Decode.lazy (\_ -> Json.Decode.map Sum03B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum03C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum03C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum03D", Json.Decode.lazy (\_ -> Json.Decode.map Sum03D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum03E", Json.Decode.lazy (\_ -> Json.Decode.map Sum03E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum03 = Set.fromList ["Sum03D", "Sum03E"]
    in  decodeSumTaggedObject "Sum03" "tag" "content" jsonDecDictSum03 jsonDecObjectSetSum03

jsonEncSum03 : (a -> Value) -> Sum03 a -> Value
jsonEncSum03 localEncoder_a val =
    let keyval v = case v of
                    Sum03A v1 -> ("Sum03A", encodeValue (localEncoder_a v1))
                    Sum03B v1 -> ("Sum03B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum03C v1 v2 -> ("Sum03C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum03D vs -> ("Sum03D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum03E vs -> ("Sum03E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum04 a =
    Sum04A a
    | Sum04B (Maybe a)
    | Sum04C a a
    | Sum04D {foo: a}
    | Sum04E {bar: Int, baz: Int}

jsonDecSum04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum04 a )
jsonDecSum04 localDecoder_a =
    let jsonDecDictSum04 = Dict.fromList
            [ ("Sum04A", Json.Decode.lazy (\_ -> Json.Decode.map Sum04A (localDecoder_a)))
            , ("Sum04B", Json.Decode.lazy (\_ -> Json.Decode.map Sum04B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum04C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum04C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum04D", Json.Decode.lazy (\_ -> Json.Decode.map Sum04D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum04E", Json.Decode.lazy (\_ -> Json.Decode.map Sum04E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
        jsonDecObjectSetSum04 = Set.fromList ["Sum04D", "Sum04E"]
    in  decodeSumTaggedObject "Sum04" "tag" "content" jsonDecDictSum04 jsonDecObjectSetSum04

jsonEncSum04 : (a -> Value) -> Sum04 a -> Value
jsonEncSum04 localEncoder_a val =
    let keyval v = case v of
                    Sum04A v1 -> ("Sum04A", encodeValue (localEncoder_a v1))
                    Sum04B v1 -> ("Sum04B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum04C v1 v2 -> ("Sum04C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum04D vs -> ("Sum04D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum04E vs -> ("Sum04E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTaggedObject "tag" "content" keyval val



type Sum05 a =
    Sum05A a
    | Sum05B (Maybe a)
    | Sum05C a a
    | Sum05D {foo: a}
    | Sum05E {bar: Int, baz: Int}

jsonDecSum05 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum05 a )
jsonDecSum05 localDecoder_a =
    let jsonDecDictSum05 = Dict.fromList
            [ ("Sum05A", Json.Decode.lazy (\_ -> Json.Decode.map Sum05A (localDecoder_a)))
            , ("Sum05B", Json.Decode.lazy (\_ -> Json.Decode.map Sum05B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum05C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum05C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum05D", Json.Decode.lazy (\_ -> Json.Decode.map Sum05D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum05E", Json.Decode.lazy (\_ -> Json.Decode.map Sum05E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum05" jsonDecDictSum05

jsonEncSum05 : (a -> Value) -> Sum05 a -> Value
jsonEncSum05 localEncoder_a val =
    let keyval v = case v of
                    Sum05A v1 -> ("Sum05A", encodeValue (localEncoder_a v1))
                    Sum05B v1 -> ("Sum05B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum05C v1 v2 -> ("Sum05C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum05D vs -> ("Sum05D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum05E vs -> ("Sum05E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum06 a =
    Sum06A a
    | Sum06B (Maybe a)
    | Sum06C a a
    | Sum06D {foo: a}
    | Sum06E {bar: Int, baz: Int}

jsonDecSum06 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum06 a )
jsonDecSum06 localDecoder_a =
    let jsonDecDictSum06 = Dict.fromList
            [ ("Sum06A", Json.Decode.lazy (\_ -> Json.Decode.map Sum06A (localDecoder_a)))
            , ("Sum06B", Json.Decode.lazy (\_ -> Json.Decode.map Sum06B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum06C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum06C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum06D", Json.Decode.lazy (\_ -> Json.Decode.map Sum06D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum06E", Json.Decode.lazy (\_ -> Json.Decode.map Sum06E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum06" jsonDecDictSum06

jsonEncSum06 : (a -> Value) -> Sum06 a -> Value
jsonEncSum06 localEncoder_a val =
    let keyval v = case v of
                    Sum06A v1 -> ("Sum06A", encodeValue (localEncoder_a v1))
                    Sum06B v1 -> ("Sum06B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum06C v1 v2 -> ("Sum06C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum06D vs -> ("Sum06D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum06E vs -> ("Sum06E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum07 a =
    Sum07A a
    | Sum07B (Maybe a)
    | Sum07C a a
    | Sum07D {foo: a}
    | Sum07E {bar: Int, baz: Int}

jsonDecSum07 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum07 a )
jsonDecSum07 localDecoder_a =
    let jsonDecDictSum07 = Dict.fromList
            [ ("Sum07A", Json.Decode.lazy (\_ -> Json.Decode.map Sum07A (localDecoder_a)))
            , ("Sum07B", Json.Decode.lazy (\_ -> Json.Decode.map Sum07B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum07C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum07C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum07D", Json.Decode.lazy (\_ -> Json.Decode.map Sum07D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum07E", Json.Decode.lazy (\_ -> Json.Decode.map Sum07E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum07" jsonDecDictSum07

jsonEncSum07 : (a -> Value) -> Sum07 a -> Value
jsonEncSum07 localEncoder_a val =
    let keyval v = case v of
                    Sum07A v1 -> ("Sum07A", encodeValue (localEncoder_a v1))
                    Sum07B v1 -> ("Sum07B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum07C v1 v2 -> ("Sum07C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum07D vs -> ("Sum07D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum07E vs -> ("Sum07E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum08 a =
    Sum08A a
    | Sum08B (Maybe a)
    | Sum08C a a
    | Sum08D {foo: a}
    | Sum08E {bar: Int, baz: Int}

jsonDecSum08 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum08 a )
jsonDecSum08 localDecoder_a =
    let jsonDecDictSum08 = Dict.fromList
            [ ("Sum08A", Json.Decode.lazy (\_ -> Json.Decode.map Sum08A (localDecoder_a)))
            , ("Sum08B", Json.Decode.lazy (\_ -> Json.Decode.map Sum08B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum08C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum08C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum08D", Json.Decode.lazy (\_ -> Json.Decode.map Sum08D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum08E", Json.Decode.lazy (\_ -> Json.Decode.map Sum08E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Sum08" jsonDecDictSum08

jsonEncSum08 : (a -> Value) -> Sum08 a -> Value
jsonEncSum08 localEncoder_a val =
    let keyval v = case v of
                    Sum08A v1 -> ("Sum08A", encodeValue (localEncoder_a v1))
                    Sum08B v1 -> ("Sum08B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum08C v1 v2 -> ("Sum08C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum08D vs -> ("Sum08D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum08E vs -> ("Sum08E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumObjectWithSingleField keyval val



type Sum09 a =
    Sum09A a
    | Sum09B (Maybe a)
    | Sum09C a a
    | Sum09D {foo: a}
    | Sum09E {bar: Int, baz: Int}

jsonDecSum09 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum09 a )
jsonDecSum09 localDecoder_a =
    let jsonDecDictSum09 = Dict.fromList
            [ ("Sum09A", Json.Decode.lazy (\_ -> Json.Decode.map Sum09A (localDecoder_a)))
            , ("Sum09B", Json.Decode.lazy (\_ -> Json.Decode.map Sum09B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum09C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum09C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum09D", Json.Decode.lazy (\_ -> Json.Decode.map Sum09D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum09E", Json.Decode.lazy (\_ -> Json.Decode.map Sum09E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum09" jsonDecDictSum09

jsonEncSum09 : (a -> Value) -> Sum09 a -> Value
jsonEncSum09 localEncoder_a val =
    let keyval v = case v of
                    Sum09A v1 -> ("Sum09A", encodeValue (localEncoder_a v1))
                    Sum09B v1 -> ("Sum09B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum09C v1 v2 -> ("Sum09C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum09D vs -> ("Sum09D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum09E vs -> ("Sum09E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum10 a =
    Sum10A a
    | Sum10B (Maybe a)
    | Sum10C a a
    | Sum10D {foo: a}
    | Sum10E {bar: Int, baz: Int}

jsonDecSum10 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum10 a )
jsonDecSum10 localDecoder_a =
    let jsonDecDictSum10 = Dict.fromList
            [ ("Sum10A", Json.Decode.lazy (\_ -> Json.Decode.map Sum10A (localDecoder_a)))
            , ("Sum10B", Json.Decode.lazy (\_ -> Json.Decode.map Sum10B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum10C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum10C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum10D", Json.Decode.lazy (\_ -> Json.Decode.map Sum10D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum10E", Json.Decode.lazy (\_ -> Json.Decode.map Sum10E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum10" jsonDecDictSum10

jsonEncSum10 : (a -> Value) -> Sum10 a -> Value
jsonEncSum10 localEncoder_a val =
    let keyval v = case v of
                    Sum10A v1 -> ("Sum10A", encodeValue (localEncoder_a v1))
                    Sum10B v1 -> ("Sum10B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum10C v1 v2 -> ("Sum10C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum10D vs -> ("Sum10D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum10E vs -> ("Sum10E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum11 a =
    Sum11A a
    | Sum11B (Maybe a)
    | Sum11C a a
    | Sum11D {foo: a}
    | Sum11E {bar: Int, baz: Int}

jsonDecSum11 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum11 a )
jsonDecSum11 localDecoder_a =
    let jsonDecDictSum11 = Dict.fromList
            [ ("Sum11A", Json.Decode.lazy (\_ -> Json.Decode.map Sum11A (localDecoder_a)))
            , ("Sum11B", Json.Decode.lazy (\_ -> Json.Decode.map Sum11B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum11C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum11C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum11D", Json.Decode.lazy (\_ -> Json.Decode.map Sum11D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum11E", Json.Decode.lazy (\_ -> Json.Decode.map Sum11E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum11" jsonDecDictSum11

jsonEncSum11 : (a -> Value) -> Sum11 a -> Value
jsonEncSum11 localEncoder_a val =
    let keyval v = case v of
                    Sum11A v1 -> ("Sum11A", encodeValue (localEncoder_a v1))
                    Sum11B v1 -> ("Sum11B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum11C v1 v2 -> ("Sum11C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum11D vs -> ("Sum11D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum11E vs -> ("Sum11E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Sum12 a =
    Sum12A a
    | Sum12B (Maybe a)
    | Sum12C a a
    | Sum12D {foo: a}
    | Sum12E {bar: Int, baz: Int}

jsonDecSum12 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Sum12 a )
jsonDecSum12 localDecoder_a =
    let jsonDecDictSum12 = Dict.fromList
            [ ("Sum12A", Json.Decode.lazy (\_ -> Json.Decode.map Sum12A (localDecoder_a)))
            , ("Sum12B", Json.Decode.lazy (\_ -> Json.Decode.map Sum12B (Json.Decode.maybe (localDecoder_a))))
            , ("Sum12C", Json.Decode.lazy (\_ -> Json.Decode.map2 Sum12C (Json.Decode.index 0 (localDecoder_a)) (Json.Decode.index 1 (localDecoder_a))))
            , ("Sum12D", Json.Decode.lazy (\_ -> Json.Decode.map Sum12D (   Json.Decode.succeed (\pfoo -> {foo = pfoo})    |> required "foo" (localDecoder_a))))
            , ("Sum12E", Json.Decode.lazy (\_ -> Json.Decode.map Sum12E (   Json.Decode.succeed (\pbar pbaz -> {bar = pbar, baz = pbaz})    |> required "bar" (Json.Decode.int)    |> required "baz" (Json.Decode.int))))
            ]
    in  decodeSumTwoElemArray  "Sum12" jsonDecDictSum12

jsonEncSum12 : (a -> Value) -> Sum12 a -> Value
jsonEncSum12 localEncoder_a val =
    let keyval v = case v of
                    Sum12A v1 -> ("Sum12A", encodeValue (localEncoder_a v1))
                    Sum12B v1 -> ("Sum12B", encodeValue ((maybeEncode (localEncoder_a)) v1))
                    Sum12C v1 v2 -> ("Sum12C", encodeValue (Json.Encode.list identity [localEncoder_a v1, localEncoder_a v2]))
                    Sum12D vs -> ("Sum12D", encodeObject [("foo", localEncoder_a vs.foo)])
                    Sum12E vs -> ("Sum12E", encodeObject [("bar", Json.Encode.int vs.bar), ("baz", Json.Encode.int vs.baz)])
    in encodeSumTwoElementArray keyval val



type Simple01 a =
    Simple01 a

jsonDecSimple01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple01 a )
jsonDecSimple01 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple01 (localDecoder_a))


jsonEncSimple01 : (a -> Value) -> Simple01 a -> Value
jsonEncSimple01 localEncoder_a(Simple01 v1) =
    localEncoder_a v1



type Simple02 a =
    Simple02 a

jsonDecSimple02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple02 a )
jsonDecSimple02 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple02 (localDecoder_a))


jsonEncSimple02 : (a -> Value) -> Simple02 a -> Value
jsonEncSimple02 localEncoder_a(Simple02 v1) =
    localEncoder_a v1



type Simple03 a =
    Simple03 a

jsonDecSimple03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple03 a )
jsonDecSimple03 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple03 (localDecoder_a))


jsonEncSimple03 : (a -> Value) -> Simple03 a -> Value
jsonEncSimple03 localEncoder_a(Simple03 v1) =
    localEncoder_a v1



type Simple04 a =
    Simple04 a

jsonDecSimple04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( Simple04 a )
jsonDecSimple04 localDecoder_a =
    Json.Decode.lazy (\_ -> Json.Decode.map Simple04 (localDecoder_a))


jsonEncSimple04 : (a -> Value) -> Simple04 a -> Value
jsonEncSimple04 localEncoder_a(Simple04 v1) =
    localEncoder_a v1



type SimpleRecord01 a = SimpleRecord01
   { qux: a
   }

jsonDecSimpleRecord01 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord01 a )
jsonDecSimpleRecord01 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord01 {qux = pqux}))
   |> required "qux" (localDecoder_a)

jsonEncSimpleRecord01 : (a -> Value) -> SimpleRecord01 a -> Value
jsonEncSimpleRecord01 localEncoder_a (SimpleRecord01 val) =
   Json.Encode.object
   [ ("qux", localEncoder_a val.qux)
   ]



type SimpleRecord02 a = SimpleRecord02
   { qux: a
   }

jsonDecSimpleRecord02 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord02 a )
jsonDecSimpleRecord02 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord02 {qux = pqux})) |> custom (localDecoder_a)

jsonEncSimpleRecord02 : (a -> Value) -> SimpleRecord02 a -> Value
jsonEncSimpleRecord02 localEncoder_a (SimpleRecord02 val) =
   localEncoder_a val.qux


type SimpleRecord03 a = SimpleRecord03
   { qux: a
   }

jsonDecSimpleRecord03 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord03 a )
jsonDecSimpleRecord03 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord03 {qux = pqux}))
   |> required "qux" (localDecoder_a)

jsonEncSimpleRecord03 : (a -> Value) -> SimpleRecord03 a -> Value
jsonEncSimpleRecord03 localEncoder_a (SimpleRecord03 val) =
   Json.Encode.object
   [ ("qux", localEncoder_a val.qux)
   ]



type SimpleRecord04 a = SimpleRecord04
   { qux: a
   }

jsonDecSimpleRecord04 : Json.Decode.Decoder a -> Json.Decode.Decoder ( SimpleRecord04 a )
jsonDecSimpleRecord04 localDecoder_a =
   Json.Decode.succeed (\pqux -> (SimpleRecord04 {qux = pqux})) |> custom (localDecoder_a)

jsonEncSimpleRecord04 : (a -> Value) -> SimpleRecord04 a -> Value
jsonEncSimpleRecord04 localEncoder_a (SimpleRecord04 val) =
   localEncoder_a val.qux


type SumUntagged a =
    SMInt Int
    | SMList a

jsonDecSumUntagged : Json.Decode.Decoder a -> Json.Decode.Decoder ( SumUntagged a )
jsonDecSumUntagged localDecoder_a =
    let jsonDecDictSumUntagged = Dict.fromList
            [ ("SMInt", Json.Decode.lazy (\_ -> Json.Decode.map SMInt (Json.Decode.int)))
            , ("SMList", Json.Decode.lazy (\_ -> Json.Decode.map SMList (localDecoder_a)))
            ]
    in  Json.Decode.oneOf (Dict.values jsonDecDictSumUntagged)

jsonEncSumUntagged : (a -> Value) -> SumUntagged a -> Value
jsonEncSumUntagged localEncoder_a val =
    let keyval v = case v of
                    SMInt v1 -> ("SMInt", encodeValue (Json.Encode.int v1))
                    SMList v1 -> ("SMList", encodeValue (localEncoder_a v1))
    in encodeSumUntagged keyval val



type alias NT1  = (List Int)

jsonDecNT1 : Json.Decode.Decoder ( NT1 )
jsonDecNT1 =
    Json.Decode.list (Json.Decode.int)

jsonEncNT1 : NT1 -> Value
jsonEncNT1  val = (Json.Encode.list Json.Encode.int) val



type alias NT2  = (List Int)

jsonDecNT2 : Json.Decode.Decoder ( NT2 )
jsonDecNT2 =
    Json.Decode.list (Json.Decode.int)

jsonEncNT2 : NT2 -> Value
jsonEncNT2  val = (Json.Encode.list Json.Encode.int) val



type alias NT3  = (List Int)

jsonDecNT3 : Json.Decode.Decoder ( NT3 )
jsonDecNT3 =
    Json.Decode.list (Json.Decode.int)

jsonEncNT3 : NT3 -> Value
jsonEncNT3  val = (Json.Encode.list Json.Encode.int) val



type NT4  = NT4
   { foo: (List Int)
   }

jsonDecNT4 : Json.Decode.Decoder ( NT4 )
jsonDecNT4 =
   Json.Decode.succeed (\pfoo -> (NT4 {foo = pfoo}))
   |> required "foo" (Json.Decode.list (Json.Decode.int))

jsonEncNT4 : NT4 -> Value
jsonEncNT4  (NT4 val) =
   Json.Encode.object
   [ ("foo", (Json.Encode.list Json.Encode.int) val.foo)
   ]



sumEncode01 : Test
sumEncode01 = describe "Sum encode 01"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum01D\",\"foo\":[]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01D {foo = []}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum01E\",\"bar\":-2,\"baz\":1}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01E {bar = -2, baz = 1}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum01B\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01B (Just [])))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum01D\",\"foo\":[-4,-1,1,-2,4,-4]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01D {foo = [-4,-1,1,-2,4,-4]}))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum01C\",\"content\":[[-1,-1,0,8,4,-8],[-8,-5,-7,-4]]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01C [-1,-1,0,8,4,-8] [-8,-5,-7,-4]))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum01E\",\"bar\":5,\"baz\":5}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01E {bar = 5, baz = 5}))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum01C\",\"content\":[[2,7],[5,9,11,-5,3]]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01C [2,7] [5,9,11,-5,3]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum01B\",\"content\":[-1,-12,-13,10,5,-10,11,-7,-2,-1,-9,-12]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01B (Just [-1,-12,-13,10,5,-10,11,-7,-2,-1,-9,-12])))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum01A\",\"content\":[-3,8,11,5,5,-12,-7,-11,-12,1,-4,13]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01A [-3,8,11,5,5,-12,-7,-11,-12,1,-4,13]))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum01D\",\"foo\":[4,-11,-18,-6,1,-3,-18,-4]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01D {foo = [4,-11,-18,-6,1,-3,-18,-4]}))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum01C\",\"content\":[[20,0,13,-19,-17,-15,-12,7,2,-6,-7,18,15,9,15,-18],[10,16,-10,6,-20,20,-2,-11,-9,0,-10,-9,9,-15,12]]}"(Json.Encode.encode 0 (jsonEncSum01(Json.Encode.list Json.Encode.int) (Sum01C [20,0,13,-19,-17,-15,-12,7,2,-6,-7,18,15,9,15,-18] [10,16,-10,6,-20,20,-2,-11,-9,0,-10,-9,9,-15,12]))))
  ]

sumEncode02 : Test
sumEncode02 = describe "Sum encode 02"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum02C\",\"content\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02C [] []))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum02C\",\"content\":[[1,1],[]]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02C [1,1] []))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum02C\",\"content\":[[1],[-2,0,3,4]]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02C [1] [-2,0,3,4]))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":3,\"baz\":6}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = 3, baz = 6}))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum02A\",\"content\":[4]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02A [4]))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum02E\",\"bar\":6,\"baz\":2}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02E {bar = 6, baz = 2}))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[4,11,-9,8,11,-8]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02D {foo = [4,11,-9,8,11,-8]}))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[1,0,13,-11,-1]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02D {foo = [1,0,13,-11,-1]}))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[-5,-4,5,-1,12,-9,-5,-14,-7,14,-11,5,5,-10,11]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02D {foo = [-5,-4,5,-1,12,-9,-5,-14,-7,14,-11,5,5,-10,11]}))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum02C\",\"content\":[[-1,16],[11,8,8,0,7,-3,-13,14,17,-9]]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02C [-1,16] [11,8,8,0,7,-3,-13,14,17,-9]))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum02D\",\"foo\":[20,-2,-16,1,-8,-3,-7,10,-14,3]}"(Json.Encode.encode 0 (jsonEncSum02(Json.Encode.list Json.Encode.int) (Sum02D {foo = [20,-2,-16,1,-8,-3,-7,10,-14,3]}))))
  ]

sumEncode03 : Test
sumEncode03 = describe "Sum encode 03"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":0,\"baz\":0}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = 0, baz = 0}))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":1,\"baz\":-2}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = 1, baz = -2}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":-2,\"baz\":-2}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = -2, baz = -2}))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[],[-1]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03C [] [-1]))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum03B\",\"content\":[0,-5,-8,-3,3]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03B (Just [0,-5,-8,-3,3])))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum03B\",\"content\":[6,-8,9,-1,4,-9,6,-8,3,0]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03B (Just [6,-8,9,-1,4,-9,6,-8,3,0])))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum03A\",\"content\":[-10,-4,-9,-9,12]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03A [-10,-4,-9,-9,12]))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum03E\",\"bar\":14,\"baz\":-7}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03E {bar = 14, baz = -7}))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[-14,0,16,-9,3],[6,-6,9,-16]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03C [-14,0,16,-9,3] [6,-6,9,-16]))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum03C\",\"content\":[[14,-9,-12,10,1,-3,0,-8,12,12,-12,-14],[-5,-2,2,-9,-5,-15,-5,1,3,13,14,6,-10,15,11,-10,-12,15]]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03C [14,-9,-12,10,1,-3,0,-8,12,12,-12,-14] [-5,-2,2,-9,-5,-15,-5,1,3,13,14,6,-10,15,11,-10,-12,15]))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum03D\",\"foo\":[-3,-9,18,13,9,-7,-10,1,-5,-7,17,20,1,-16,8]}"(Json.Encode.encode 0 (jsonEncSum03(Json.Encode.list Json.Encode.int) (Sum03D {foo = [-3,-9,18,13,9,-7,-10,1,-5,-7,17,20,1,-16,8]}))))
  ]

sumEncode04 : Test
sumEncode04 = describe "Sum encode 04"
  [ test "1" (\_ -> equalHack "{\"tag\":\"Sum04C\",\"content\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04C [] []))))
  , test "2" (\_ -> equalHack "{\"tag\":\"Sum04E\",\"bar\":1,\"baz\":1}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04E {bar = 1, baz = 1}))))
  , test "3" (\_ -> equalHack "{\"tag\":\"Sum04E\",\"bar\":1,\"baz\":-4}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04E {bar = 1, baz = -4}))))
  , test "4" (\_ -> equalHack "{\"tag\":\"Sum04A\",\"content\":[5,6,1,5,5,0]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04A [5,6,1,5,5,0]))))
  , test "5" (\_ -> equalHack "{\"tag\":\"Sum04B\",\"content\":[3,3,-1]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04B (Just [3,3,-1])))))
  , test "6" (\_ -> equalHack "{\"tag\":\"Sum04B\",\"content\":[9,9,7,3,6,7,1,-1]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04B (Just [9,9,7,3,6,7,1,-1])))))
  , test "7" (\_ -> equalHack "{\"tag\":\"Sum04A\",\"content\":[]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04A []))))
  , test "8" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[6,5,-3,-9,10,3,-10,-5,11,-10]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [6,5,-3,-9,10,3,-10,-5,11,-10]}))))
  , test "9" (\_ -> equalHack "{\"tag\":\"Sum04B\",\"content\":[8,-10,3,0,7,-16]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04B (Just [8,-10,3,0,7,-16])))))
  , test "10" (\_ -> equalHack "{\"tag\":\"Sum04A\",\"content\":[-8,4,-3,2,2,-12,-1,-8,-12,-2,-13,7,-15,1,-5,-8]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04A [-8,4,-3,2,2,-12,-1,-8,-12,-2,-13,7,-15,1,-5,-8]))))
  , test "11" (\_ -> equalHack "{\"tag\":\"Sum04D\",\"foo\":[17,-13,12,-14,8,-10,18,-13,5,4,17,8,10]}"(Json.Encode.encode 0 (jsonEncSum04(Json.Encode.list Json.Encode.int) (Sum04D {foo = [17,-13,12,-14,8,-10,18,-13,5,4,17,8,10]}))))
  ]

sumEncode05 : Test
sumEncode05 = describe "Sum encode 05"
  [ test "1" (\_ -> equalHack "{\"Sum05A\":[]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05A []))))
  , test "2" (\_ -> equalHack "{\"Sum05C\":[[-2],[1,1]]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05C [-2] [1,1]))))
  , test "3" (\_ -> equalHack "{\"Sum05A\":[1,-1,1,3]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05A [1,-1,1,3]))))
  , test "4" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05D {foo = []}))))
  , test "5" (\_ -> equalHack "{\"Sum05A\":[8,6,3]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05A [8,6,3]))))
  , test "6" (\_ -> equalHack "{\"Sum05B\":[5,9,9,8,8,4,3,-10,-10,-9]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [5,9,9,8,8,4,3,-10,-10,-9])))))
  , test "7" (\_ -> equalHack "{\"Sum05A\":[12,-4,-4,11,-8,0,2,-12]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05A [12,-4,-4,11,-8,0,2,-12]))))
  , test "8" (\_ -> equalHack "{\"Sum05E\":{\"bar\":-10,\"baz\":-9}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05E {bar = -10, baz = -9}))))
  , test "9" (\_ -> equalHack "{\"Sum05D\":{\"foo\":[16,-13,-13,-14,-16,-8,5,1,-6,-12,12]}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05D {foo = [16,-13,-13,-14,-16,-8,5,1,-6,-12,12]}))))
  , test "10" (\_ -> equalHack "{\"Sum05B\":[3,-8,3,10,4,-2,-3,-6,-8,0,18,-5,1]}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05B (Just [3,-8,3,10,4,-2,-3,-6,-8,0,18,-5,1])))))
  , test "11" (\_ -> equalHack "{\"Sum05E\":{\"bar\":-1,\"baz\":19}}"(Json.Encode.encode 0 (jsonEncSum05(Json.Encode.list Json.Encode.int) (Sum05E {bar = -1, baz = 19}))))
  ]

sumEncode06 : Test
sumEncode06 = describe "Sum encode 06"
  [ test "1" (\_ -> equalHack "{\"Sum06A\":[]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06A []))))
  , test "2" (\_ -> equalHack "{\"Sum06E\":{\"bar\":0,\"baz\":0}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06E {bar = 0, baz = 0}))))
  , test "3" (\_ -> equalHack "{\"Sum06C\":[[1,2],[]]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06C [1,2] []))))
  , test "4" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[-1,3,0,2,-1,4]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = [-1,3,0,2,-1,4]}))))
  , test "5" (\_ -> equalHack "{\"Sum06A\":[]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06A []))))
  , test "6" (\_ -> equalHack "{\"Sum06E\":{\"bar\":4,\"baz\":5}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06E {bar = 4, baz = 5}))))
  , test "7" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[-8,5,-4,2,11,4]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = [-8,5,-4,2,11,4]}))))
  , test "8" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[0,-3,-8,13,-9,1,0]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = [0,-3,-8,13,-9,1,0]}))))
  , test "9" (\_ -> equalHack "{\"Sum06B\":[13,3,-3,-16,4,-1,-4,11,-3,3,4,12]}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06B (Just [13,3,-3,-16,4,-1,-4,11,-3,3,4,12])))))
  , test "10" (\_ -> equalHack "{\"Sum06E\":{\"bar\":8,\"baz\":-16}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06E {bar = 8, baz = -16}))))
  , test "11" (\_ -> equalHack "{\"Sum06D\":{\"foo\":[-5,11,-2,-5]}}"(Json.Encode.encode 0 (jsonEncSum06(Json.Encode.list Json.Encode.int) (Sum06D {foo = [-5,11,-2,-5]}))))
  ]

sumEncode07 : Test
sumEncode07 = describe "Sum encode 07"
  [ test "1" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07D {foo = []}))))
  , test "2" (\_ -> equalHack "{\"Sum07A\":[-2,1]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07A [-2,1]))))
  , test "3" (\_ -> equalHack "{\"Sum07B\":[-1,-1,-2,0]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [-1,-1,-2,0])))))
  , test "4" (\_ -> equalHack "{\"Sum07A\":[-2,-1,0]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07A [-2,-1,0]))))
  , test "5" (\_ -> equalHack "{\"Sum07B\":[1,-5,-5,7,6]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [1,-5,-5,7,6])))))
  , test "6" (\_ -> equalHack "{\"Sum07B\":[2,2,4,-2]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07B (Just [2,2,4,-2])))))
  , test "7" (\_ -> equalHack "{\"Sum07C\":[[-12],[]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07C [-12] []))))
  , test "8" (\_ -> equalHack "{\"Sum07C\":[[13,-3],[12,-6,-10,-4]]}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07C [13,-3] [12,-6,-10,-4]))))
  , test "9" (\_ -> equalHack "{\"Sum07E\":{\"bar\":15,\"baz\":-13}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07E {bar = 15, baz = -13}))))
  , test "10" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[7,10,-11,-6,4,15,10,-9,-1,14,11,-1,12,8]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07D {foo = [7,10,-11,-6,4,15,10,-9,-1,14,11,-1,12,8]}))))
  , test "11" (\_ -> equalHack "{\"Sum07D\":{\"foo\":[0,-12,-13,20,15,8,-14,4,2,-20,-5,8,-16]}}"(Json.Encode.encode 0 (jsonEncSum07(Json.Encode.list Json.Encode.int) (Sum07D {foo = [0,-12,-13,20,15,8,-14,4,2,-20,-5,8,-16]}))))
  ]

sumEncode08 : Test
sumEncode08 = describe "Sum encode 08"
  [ test "1" (\_ -> equalHack "{\"Sum08C\":[[],[]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08C [] []))))
  , test "2" (\_ -> equalHack "{\"Sum08A\":[1]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08A [1]))))
  , test "3" (\_ -> equalHack "{\"Sum08A\":[-2,4]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08A [-2,4]))))
  , test "4" (\_ -> equalHack "{\"Sum08C\":[[0,1],[-3,0,-1,3,-3,-5]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08C [0,1] [-3,0,-1,3,-3,-5]))))
  , test "5" (\_ -> equalHack "{\"Sum08B\":[-4,2]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08B (Just [-4,2])))))
  , test "6" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[7,2]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08D {foo = [7,2]}))))
  , test "7" (\_ -> equalHack "{\"Sum08A\":[-6,-12,5,-5,8,-4,6,-5,-3,10,11]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08A [-6,-12,5,-5,8,-4,6,-5,-3,10,11]))))
  , test "8" (\_ -> equalHack "{\"Sum08C\":[[-1,14,11,-3,-8,14,-14,-10,7,-11,11,0],[-11,11,7,-1,-12,4,-6,-5]]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08C [-1,14,11,-3,-8,14,-14,-10,7,-11,11,0] [-11,11,7,-1,-12,4,-6,-5]))))
  , test "9" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[5,5,-7,-6,11]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08D {foo = [5,5,-7,-6,11]}))))
  , test "10" (\_ -> equalHack "{\"Sum08B\":[]}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08B (Just [])))))
  , test "11" (\_ -> equalHack "{\"Sum08D\":{\"foo\":[0,-19,-20,19,20,-8,-19,-7,-6,-9,19,8,14,-9,-6,-16,-16,-3,-6,-6]}}"(Json.Encode.encode 0 (jsonEncSum08(Json.Encode.list Json.Encode.int) (Sum08D {foo = [0,-19,-20,19,20,-8,-19,-7,-6,-9,19,8,14,-9,-6,-16,-16,-3,-6,-6]}))))
  ]

sumEncode09 : Test
sumEncode09 = describe "Sum encode 09"
  [ test "1" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09D {foo = []}))))
  , test "2" (\_ -> equalHack "[\"Sum09C\",[[],[-2,2]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09C [] [-2,2]))))
  , test "3" (\_ -> equalHack "[\"Sum09C\",[[],[-3,1,-2]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09C [] [-3,1,-2]))))
  , test "4" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[1,-4,-1,1]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09D {foo = [1,-4,-1,1]}))))
  , test "5" (\_ -> equalHack "[\"Sum09E\",{\"bar\":7,\"baz\":8}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = 7, baz = 8}))))
  , test "6" (\_ -> equalHack "[\"Sum09E\",{\"bar\":10,\"baz\":8}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = 10, baz = 8}))))
  , test "7" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[-3,1,-4,-7,11,-6]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09D {foo = [-3,1,-4,-7,11,-6]}))))
  , test "8" (\_ -> equalHack "[\"Sum09E\",{\"bar\":-6,\"baz\":11}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09E {bar = -6, baz = 11}))))
  , test "9" (\_ -> equalHack "[\"Sum09D\",{\"foo\":[0,15,14,-1,-10,1,0,-9,-4,8,-8,-11,6,-15]}]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09D {foo = [0,15,14,-1,-10,1,0,-9,-4,8,-8,-11,6,-15]}))))
  , test "10" (\_ -> equalHack "[\"Sum09A\",[-11,-1,9]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09A [-11,-1,9]))))
  , test "11" (\_ -> equalHack "[\"Sum09C\",[[4,16,3,-15,-1,-10,15],[11,12,-6,18,11,-20,-16,-20,16,-12]]]"(Json.Encode.encode 0 (jsonEncSum09(Json.Encode.list Json.Encode.int) (Sum09C [4,16,3,-15,-1,-10,15] [11,12,-6,18,11,-20,-16,-20,16,-12]))))
  ]

sumEncode10 : Test
sumEncode10 = describe "Sum encode 10"
  [ test "1" (\_ -> equalHack "[\"Sum10B\",[]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10B (Just [])))))
  , test "2" (\_ -> equalHack "[\"Sum10C\",[[-1,0],[1,1]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10C [-1,0] [1,1]))))
  , test "3" (\_ -> equalHack "[\"Sum10C\",[[],[-4,1]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10C [] [-4,1]))))
  , test "4" (\_ -> equalHack "[\"Sum10D\",{\"foo\":[-5,1,-3,5]}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10D {foo = [-5,1,-3,5]}))))
  , test "5" (\_ -> equalHack "[\"Sum10B\",[0,-6,7,8,8,-6]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10B (Just [0,-6,7,8,8,-6])))))
  , test "6" (\_ -> equalHack "[\"Sum10E\",{\"bar\":5,\"baz\":4}]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10E {bar = 5, baz = 4}))))
  , test "7" (\_ -> equalHack "[\"Sum10A\",[5,-1,-2,12]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10A [5,-1,-2,12]))))
  , test "8" (\_ -> equalHack "[\"Sum10B\",[-2,8,-8,10,-10]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10B (Just [-2,8,-8,10,-10])))))
  , test "9" (\_ -> equalHack "[\"Sum10A\",[16,-16,-10,8]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10A [16,-16,-10,8]))))
  , test "10" (\_ -> equalHack "[\"Sum10C\",[[-17,2,3],[14,-18,10,12,0,-8]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10C [-17,2,3] [14,-18,10,12,0,-8]))))
  , test "11" (\_ -> equalHack "[\"Sum10C\",[[-12,7,16,15,9,17,1,-15,5,-13,-13,16,10,5],[-2,-13,-4,1,6,-4,-10,-2,-8,18]]]"(Json.Encode.encode 0 (jsonEncSum10(Json.Encode.list Json.Encode.int) (Sum10C [-12,7,16,15,9,17,1,-15,5,-13,-13,16,10,5] [-2,-13,-4,1,6,-4,-10,-2,-8,18]))))
  ]

sumEncode11 : Test
sumEncode11 = describe "Sum encode 11"
  [ test "1" (\_ -> equalHack "[\"Sum11B\",[]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11B (Just [])))))
  , test "2" (\_ -> equalHack "[\"Sum11C\",[[0,0],[]]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11C [0,0] []))))
  , test "3" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[-4]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11D {foo = [-4]}))))
  , test "4" (\_ -> equalHack "[\"Sum11C\",[[4],[-1,-6,-4]]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11C [4] [-1,-6,-4]))))
  , test "5" (\_ -> equalHack "[\"Sum11A\",[]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A []))))
  , test "6" (\_ -> equalHack "[\"Sum11A\",[-6,-9,-10,-7,8,1]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A [-6,-9,-10,-7,8,1]))))
  , test "7" (\_ -> equalHack "[\"Sum11A\",[3,-3,-10,-2]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11A [3,-3,-10,-2]))))
  , test "8" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[-2,-11]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11D {foo = [-2,-11]}))))
  , test "9" (\_ -> equalHack "[\"Sum11D\",{\"foo\":[-7,-4,-12,-3,5,5,-10,-5,3,7,-9,-16,5]}]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11D {foo = [-7,-4,-12,-3,5,5,-10,-5,3,7,-9,-16,5]}))))
  , test "10" (\_ -> equalHack "[\"Sum11B\",[1,17,11]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11B (Just [1,17,11])))))
  , test "11" (\_ -> equalHack "[\"Sum11B\",[-16,2,10,-14,-9,-16,15,0,19,15,18,15,12,12,-9,1]]"(Json.Encode.encode 0 (jsonEncSum11(Json.Encode.list Json.Encode.int) (Sum11B (Just [-16,2,10,-14,-9,-16,15,0,19,15,18,15,12,12,-9,1])))))
  ]

sumEncode12 : Test
sumEncode12 = describe "Sum encode 12"
  [ test "1" (\_ -> equalHack "[\"Sum12A\",[]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12A []))))
  , test "2" (\_ -> equalHack "[\"Sum12E\",{\"bar\":-1,\"baz\":-1}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12E {bar = -1, baz = -1}))))
  , test "3" (\_ -> equalHack "[\"Sum12D\",{\"foo\":[1,3,1,1]}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12D {foo = [1,3,1,1]}))))
  , test "4" (\_ -> equalHack "[\"Sum12B\",[3,4,5,0,-1]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [3,4,5,0,-1])))))
  , test "5" (\_ -> equalHack "[\"Sum12D\",{\"foo\":[-2,2]}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12D {foo = [-2,2]}))))
  , test "6" (\_ -> equalHack "[\"Sum12D\",{\"foo\":[2]}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12D {foo = [2]}))))
  , test "7" (\_ -> equalHack "[\"Sum12E\",{\"bar\":0,\"baz\":9}]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12E {bar = 0, baz = 9}))))
  , test "8" (\_ -> equalHack "[\"Sum12B\",[3,2,7,13,8,-5,6,13,3]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12B (Just [3,2,7,13,8,-5,6,13,3])))))
  , test "9" (\_ -> equalHack "[\"Sum12C\",[[13,5,14,6,15,-15,-15,-10,16,-3,13,13],[-15,-16,-5,9,13,2,0,6,-7,1,9,15,4]]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12C [13,5,14,6,15,-15,-15,-10,16,-3,13,13] [-15,-16,-5,9,13,2,0,6,-7,1,9,15,4]))))
  , test "10" (\_ -> equalHack "[\"Sum12A\",[-7,17,-2,4,18,5,3,-5,1,-18,2,-4,-2,2,9]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12A [-7,17,-2,4,18,5,3,-5,1,-18,2,-4,-2,2,9]))))
  , test "11" (\_ -> equalHack "[\"Sum12A\",[-19,15,-1,7]]"(Json.Encode.encode 0 (jsonEncSum12(Json.Encode.list Json.Encode.int) (Sum12A [-19,15,-1,7]))))
  ]

sumDecode01 : Test
sumDecode01 = describe "Sum decode 01"
  [ test "1" (\_ -> equal (Ok (Sum01D {foo = []})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01D\",\"foo\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum01E {bar = -2, baz = 1})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01E\",\"bar\":-2,\"baz\":1}"))
  , test "3" (\_ -> equal (Ok (Sum01B (Just []))) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01B\",\"content\":[]}"))
  , test "4" (\_ -> equal (Ok (Sum01D {foo = [-4,-1,1,-2,4,-4]})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01D\",\"foo\":[-4,-1,1,-2,4,-4]}"))
  , test "5" (\_ -> equal (Ok (Sum01C [-1,-1,0,8,4,-8] [-8,-5,-7,-4])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01C\",\"content\":[[-1,-1,0,8,4,-8],[-8,-5,-7,-4]]}"))
  , test "6" (\_ -> equal (Ok (Sum01E {bar = 5, baz = 5})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01E\",\"bar\":5,\"baz\":5}"))
  , test "7" (\_ -> equal (Ok (Sum01C [2,7] [5,9,11,-5,3])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01C\",\"content\":[[2,7],[5,9,11,-5,3]]}"))
  , test "8" (\_ -> equal (Ok (Sum01B (Just [-1,-12,-13,10,5,-10,11,-7,-2,-1,-9,-12]))) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01B\",\"content\":[-1,-12,-13,10,5,-10,11,-7,-2,-1,-9,-12]}"))
  , test "9" (\_ -> equal (Ok (Sum01A [-3,8,11,5,5,-12,-7,-11,-12,1,-4,13])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01A\",\"content\":[-3,8,11,5,5,-12,-7,-11,-12,1,-4,13]}"))
  , test "10" (\_ -> equal (Ok (Sum01D {foo = [4,-11,-18,-6,1,-3,-18,-4]})) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01D\",\"foo\":[4,-11,-18,-6,1,-3,-18,-4]}"))
  , test "11" (\_ -> equal (Ok (Sum01C [20,0,13,-19,-17,-15,-12,7,2,-6,-7,18,15,9,15,-18] [10,16,-10,6,-20,20,-2,-11,-9,0,-10,-9,9,-15,12])) (Json.Decode.decodeString (jsonDecSum01 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum01C\",\"content\":[[20,0,13,-19,-17,-15,-12,7,2,-6,-7,18,15,9,15,-18],[10,16,-10,6,-20,20,-2,-11,-9,0,-10,-9,9,-15,12]]}"))
  ]

sumDecode02 : Test
sumDecode02 = describe "Sum decode 02"
  [ test "1" (\_ -> equal (Ok (Sum02C [] [])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02C\",\"content\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (Sum02C [1,1] [])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02C\",\"content\":[[1,1],[]]}"))
  , test "3" (\_ -> equal (Ok (Sum02C [1] [-2,0,3,4])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02C\",\"content\":[[1],[-2,0,3,4]]}"))
  , test "4" (\_ -> equal (Ok (Sum02E {bar = 3, baz = 6})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":3,\"baz\":6}"))
  , test "5" (\_ -> equal (Ok (Sum02A [4])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02A\",\"content\":[4]}"))
  , test "6" (\_ -> equal (Ok (Sum02E {bar = 6, baz = 2})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02E\",\"bar\":6,\"baz\":2}"))
  , test "7" (\_ -> equal (Ok (Sum02D {foo = [4,11,-9,8,11,-8]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[4,11,-9,8,11,-8]}"))
  , test "8" (\_ -> equal (Ok (Sum02D {foo = [1,0,13,-11,-1]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[1,0,13,-11,-1]}"))
  , test "9" (\_ -> equal (Ok (Sum02D {foo = [-5,-4,5,-1,12,-9,-5,-14,-7,14,-11,5,5,-10,11]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[-5,-4,5,-1,12,-9,-5,-14,-7,14,-11,5,5,-10,11]}"))
  , test "10" (\_ -> equal (Ok (Sum02C [-1,16] [11,8,8,0,7,-3,-13,14,17,-9])) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02C\",\"content\":[[-1,16],[11,8,8,0,7,-3,-13,14,17,-9]]}"))
  , test "11" (\_ -> equal (Ok (Sum02D {foo = [20,-2,-16,1,-8,-3,-7,10,-14,3]})) (Json.Decode.decodeString (jsonDecSum02 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum02D\",\"foo\":[20,-2,-16,1,-8,-3,-7,10,-14,3]}"))
  ]

sumDecode03 : Test
sumDecode03 = describe "Sum decode 03"
  [ test "1" (\_ -> equal (Ok (Sum03E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":0,\"baz\":0}"))
  , test "2" (\_ -> equal (Ok (Sum03E {bar = 1, baz = -2})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":1,\"baz\":-2}"))
  , test "3" (\_ -> equal (Ok (Sum03E {bar = -2, baz = -2})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":-2,\"baz\":-2}"))
  , test "4" (\_ -> equal (Ok (Sum03C [] [-1])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[],[-1]]}"))
  , test "5" (\_ -> equal (Ok (Sum03B (Just [0,-5,-8,-3,3]))) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03B\",\"content\":[0,-5,-8,-3,3]}"))
  , test "6" (\_ -> equal (Ok (Sum03B (Just [6,-8,9,-1,4,-9,6,-8,3,0]))) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03B\",\"content\":[6,-8,9,-1,4,-9,6,-8,3,0]}"))
  , test "7" (\_ -> equal (Ok (Sum03A [-10,-4,-9,-9,12])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03A\",\"content\":[-10,-4,-9,-9,12]}"))
  , test "8" (\_ -> equal (Ok (Sum03E {bar = 14, baz = -7})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03E\",\"bar\":14,\"baz\":-7}"))
  , test "9" (\_ -> equal (Ok (Sum03C [-14,0,16,-9,3] [6,-6,9,-16])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[-14,0,16,-9,3],[6,-6,9,-16]]}"))
  , test "10" (\_ -> equal (Ok (Sum03C [14,-9,-12,10,1,-3,0,-8,12,12,-12,-14] [-5,-2,2,-9,-5,-15,-5,1,3,13,14,6,-10,15,11,-10,-12,15])) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03C\",\"content\":[[14,-9,-12,10,1,-3,0,-8,12,12,-12,-14],[-5,-2,2,-9,-5,-15,-5,1,3,13,14,6,-10,15,11,-10,-12,15]]}"))
  , test "11" (\_ -> equal (Ok (Sum03D {foo = [-3,-9,18,13,9,-7,-10,1,-5,-7,17,20,1,-16,8]})) (Json.Decode.decodeString (jsonDecSum03 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum03D\",\"foo\":[-3,-9,18,13,9,-7,-10,1,-5,-7,17,20,1,-16,8]}"))
  ]

sumDecode04 : Test
sumDecode04 = describe "Sum decode 04"
  [ test "1" (\_ -> equal (Ok (Sum04C [] [])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04C\",\"content\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (Sum04E {bar = 1, baz = 1})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04E\",\"bar\":1,\"baz\":1}"))
  , test "3" (\_ -> equal (Ok (Sum04E {bar = 1, baz = -4})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04E\",\"bar\":1,\"baz\":-4}"))
  , test "4" (\_ -> equal (Ok (Sum04A [5,6,1,5,5,0])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04A\",\"content\":[5,6,1,5,5,0]}"))
  , test "5" (\_ -> equal (Ok (Sum04B (Just [3,3,-1]))) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04B\",\"content\":[3,3,-1]}"))
  , test "6" (\_ -> equal (Ok (Sum04B (Just [9,9,7,3,6,7,1,-1]))) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04B\",\"content\":[9,9,7,3,6,7,1,-1]}"))
  , test "7" (\_ -> equal (Ok (Sum04A [])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04A\",\"content\":[]}"))
  , test "8" (\_ -> equal (Ok (Sum04D {foo = [6,5,-3,-9,10,3,-10,-5,11,-10]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[6,5,-3,-9,10,3,-10,-5,11,-10]}"))
  , test "9" (\_ -> equal (Ok (Sum04B (Just [8,-10,3,0,7,-16]))) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04B\",\"content\":[8,-10,3,0,7,-16]}"))
  , test "10" (\_ -> equal (Ok (Sum04A [-8,4,-3,2,2,-12,-1,-8,-12,-2,-13,7,-15,1,-5,-8])) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04A\",\"content\":[-8,4,-3,2,2,-12,-1,-8,-12,-2,-13,7,-15,1,-5,-8]}"))
  , test "11" (\_ -> equal (Ok (Sum04D {foo = [17,-13,12,-14,8,-10,18,-13,5,4,17,8,10]})) (Json.Decode.decodeString (jsonDecSum04 (Json.Decode.list Json.Decode.int)) "{\"tag\":\"Sum04D\",\"foo\":[17,-13,12,-14,8,-10,18,-13,5,4,17,8,10]}"))
  ]

sumDecode05 : Test
sumDecode05 = describe "Sum decode 05"
  [ test "1" (\_ -> equal (Ok (Sum05A [])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05A\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum05C [-2] [1,1])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05C\":[[-2],[1,1]]}"))
  , test "3" (\_ -> equal (Ok (Sum05A [1,-1,1,3])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05A\":[1,-1,1,3]}"))
  , test "4" (\_ -> equal (Ok (Sum05D {foo = []})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[]}}"))
  , test "5" (\_ -> equal (Ok (Sum05A [8,6,3])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05A\":[8,6,3]}"))
  , test "6" (\_ -> equal (Ok (Sum05B (Just [5,9,9,8,8,4,3,-10,-10,-9]))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[5,9,9,8,8,4,3,-10,-10,-9]}"))
  , test "7" (\_ -> equal (Ok (Sum05A [12,-4,-4,11,-8,0,2,-12])) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05A\":[12,-4,-4,11,-8,0,2,-12]}"))
  , test "8" (\_ -> equal (Ok (Sum05E {bar = -10, baz = -9})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":-10,\"baz\":-9}}"))
  , test "9" (\_ -> equal (Ok (Sum05D {foo = [16,-13,-13,-14,-16,-8,5,1,-6,-12,12]})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05D\":{\"foo\":[16,-13,-13,-14,-16,-8,5,1,-6,-12,12]}}"))
  , test "10" (\_ -> equal (Ok (Sum05B (Just [3,-8,3,10,4,-2,-3,-6,-8,0,18,-5,1]))) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05B\":[3,-8,3,10,4,-2,-3,-6,-8,0,18,-5,1]}"))
  , test "11" (\_ -> equal (Ok (Sum05E {bar = -1, baz = 19})) (Json.Decode.decodeString (jsonDecSum05 (Json.Decode.list Json.Decode.int)) "{\"Sum05E\":{\"bar\":-1,\"baz\":19}}"))
  ]

sumDecode06 : Test
sumDecode06 = describe "Sum decode 06"
  [ test "1" (\_ -> equal (Ok (Sum06A [])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06A\":[]}"))
  , test "2" (\_ -> equal (Ok (Sum06E {bar = 0, baz = 0})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":0,\"baz\":0}}"))
  , test "3" (\_ -> equal (Ok (Sum06C [1,2] [])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06C\":[[1,2],[]]}"))
  , test "4" (\_ -> equal (Ok (Sum06D {foo = [-1,3,0,2,-1,4]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[-1,3,0,2,-1,4]}}"))
  , test "5" (\_ -> equal (Ok (Sum06A [])) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06A\":[]}"))
  , test "6" (\_ -> equal (Ok (Sum06E {bar = 4, baz = 5})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":4,\"baz\":5}}"))
  , test "7" (\_ -> equal (Ok (Sum06D {foo = [-8,5,-4,2,11,4]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[-8,5,-4,2,11,4]}}"))
  , test "8" (\_ -> equal (Ok (Sum06D {foo = [0,-3,-8,13,-9,1,0]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[0,-3,-8,13,-9,1,0]}}"))
  , test "9" (\_ -> equal (Ok (Sum06B (Just [13,3,-3,-16,4,-1,-4,11,-3,3,4,12]))) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06B\":[13,3,-3,-16,4,-1,-4,11,-3,3,4,12]}"))
  , test "10" (\_ -> equal (Ok (Sum06E {bar = 8, baz = -16})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06E\":{\"bar\":8,\"baz\":-16}}"))
  , test "11" (\_ -> equal (Ok (Sum06D {foo = [-5,11,-2,-5]})) (Json.Decode.decodeString (jsonDecSum06 (Json.Decode.list Json.Decode.int)) "{\"Sum06D\":{\"foo\":[-5,11,-2,-5]}}"))
  ]

sumDecode07 : Test
sumDecode07 = describe "Sum decode 07"
  [ test "1" (\_ -> equal (Ok (Sum07D {foo = []})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[]}}"))
  , test "2" (\_ -> equal (Ok (Sum07A [-2,1])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07A\":[-2,1]}"))
  , test "3" (\_ -> equal (Ok (Sum07B (Just [-1,-1,-2,0]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[-1,-1,-2,0]}"))
  , test "4" (\_ -> equal (Ok (Sum07A [-2,-1,0])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07A\":[-2,-1,0]}"))
  , test "5" (\_ -> equal (Ok (Sum07B (Just [1,-5,-5,7,6]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[1,-5,-5,7,6]}"))
  , test "6" (\_ -> equal (Ok (Sum07B (Just [2,2,4,-2]))) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07B\":[2,2,4,-2]}"))
  , test "7" (\_ -> equal (Ok (Sum07C [-12] [])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[-12],[]]}"))
  , test "8" (\_ -> equal (Ok (Sum07C [13,-3] [12,-6,-10,-4])) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07C\":[[13,-3],[12,-6,-10,-4]]}"))
  , test "9" (\_ -> equal (Ok (Sum07E {bar = 15, baz = -13})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07E\":{\"bar\":15,\"baz\":-13}}"))
  , test "10" (\_ -> equal (Ok (Sum07D {foo = [7,10,-11,-6,4,15,10,-9,-1,14,11,-1,12,8]})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[7,10,-11,-6,4,15,10,-9,-1,14,11,-1,12,8]}}"))
  , test "11" (\_ -> equal (Ok (Sum07D {foo = [0,-12,-13,20,15,8,-14,4,2,-20,-5,8,-16]})) (Json.Decode.decodeString (jsonDecSum07 (Json.Decode.list Json.Decode.int)) "{\"Sum07D\":{\"foo\":[0,-12,-13,20,15,8,-14,4,2,-20,-5,8,-16]}}"))
  ]

sumDecode08 : Test
sumDecode08 = describe "Sum decode 08"
  [ test "1" (\_ -> equal (Ok (Sum08C [] [])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[],[]]}"))
  , test "2" (\_ -> equal (Ok (Sum08A [1])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08A\":[1]}"))
  , test "3" (\_ -> equal (Ok (Sum08A [-2,4])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08A\":[-2,4]}"))
  , test "4" (\_ -> equal (Ok (Sum08C [0,1] [-3,0,-1,3,-3,-5])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[0,1],[-3,0,-1,3,-3,-5]]}"))
  , test "5" (\_ -> equal (Ok (Sum08B (Just [-4,2]))) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08B\":[-4,2]}"))
  , test "6" (\_ -> equal (Ok (Sum08D {foo = [7,2]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[7,2]}}"))
  , test "7" (\_ -> equal (Ok (Sum08A [-6,-12,5,-5,8,-4,6,-5,-3,10,11])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08A\":[-6,-12,5,-5,8,-4,6,-5,-3,10,11]}"))
  , test "8" (\_ -> equal (Ok (Sum08C [-1,14,11,-3,-8,14,-14,-10,7,-11,11,0] [-11,11,7,-1,-12,4,-6,-5])) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08C\":[[-1,14,11,-3,-8,14,-14,-10,7,-11,11,0],[-11,11,7,-1,-12,4,-6,-5]]}"))
  , test "9" (\_ -> equal (Ok (Sum08D {foo = [5,5,-7,-6,11]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[5,5,-7,-6,11]}}"))
  , test "10" (\_ -> equal (Ok (Sum08B (Just []))) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08B\":[]}"))
  , test "11" (\_ -> equal (Ok (Sum08D {foo = [0,-19,-20,19,20,-8,-19,-7,-6,-9,19,8,14,-9,-6,-16,-16,-3,-6,-6]})) (Json.Decode.decodeString (jsonDecSum08 (Json.Decode.list Json.Decode.int)) "{\"Sum08D\":{\"foo\":[0,-19,-20,19,20,-8,-19,-7,-6,-9,19,8,14,-9,-6,-16,-16,-3,-6,-6]}}"))
  ]

sumDecode09 : Test
sumDecode09 = describe "Sum decode 09"
  [ test "1" (\_ -> equal (Ok (Sum09D {foo = []})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[]}]"))
  , test "2" (\_ -> equal (Ok (Sum09C [] [-2,2])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[],[-2,2]]]"))
  , test "3" (\_ -> equal (Ok (Sum09C [] [-3,1,-2])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[],[-3,1,-2]]]"))
  , test "4" (\_ -> equal (Ok (Sum09D {foo = [1,-4,-1,1]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[1,-4,-1,1]}]"))
  , test "5" (\_ -> equal (Ok (Sum09E {bar = 7, baz = 8})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":7,\"baz\":8}]"))
  , test "6" (\_ -> equal (Ok (Sum09E {bar = 10, baz = 8})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":10,\"baz\":8}]"))
  , test "7" (\_ -> equal (Ok (Sum09D {foo = [-3,1,-4,-7,11,-6]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[-3,1,-4,-7,11,-6]}]"))
  , test "8" (\_ -> equal (Ok (Sum09E {bar = -6, baz = 11})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09E\",{\"bar\":-6,\"baz\":11}]"))
  , test "9" (\_ -> equal (Ok (Sum09D {foo = [0,15,14,-1,-10,1,0,-9,-4,8,-8,-11,6,-15]})) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09D\",{\"foo\":[0,15,14,-1,-10,1,0,-9,-4,8,-8,-11,6,-15]}]"))
  , test "10" (\_ -> equal (Ok (Sum09A [-11,-1,9])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09A\",[-11,-1,9]]"))
  , test "11" (\_ -> equal (Ok (Sum09C [4,16,3,-15,-1,-10,15] [11,12,-6,18,11,-20,-16,-20,16,-12])) (Json.Decode.decodeString (jsonDecSum09 (Json.Decode.list Json.Decode.int)) "[\"Sum09C\",[[4,16,3,-15,-1,-10,15],[11,12,-6,18,11,-20,-16,-20,16,-12]]]"))
  ]

sumDecode10 : Test
sumDecode10 = describe "Sum decode 10"
  [ test "1" (\_ -> equal (Ok (Sum10B (Just []))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[]]"))
  , test "2" (\_ -> equal (Ok (Sum10C [-1,0] [1,1])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[-1,0],[1,1]]]"))
  , test "3" (\_ -> equal (Ok (Sum10C [] [-4,1])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[],[-4,1]]]"))
  , test "4" (\_ -> equal (Ok (Sum10D {foo = [-5,1,-3,5]})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10D\",{\"foo\":[-5,1,-3,5]}]"))
  , test "5" (\_ -> equal (Ok (Sum10B (Just [0,-6,7,8,8,-6]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[0,-6,7,8,8,-6]]"))
  , test "6" (\_ -> equal (Ok (Sum10E {bar = 5, baz = 4})) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10E\",{\"bar\":5,\"baz\":4}]"))
  , test "7" (\_ -> equal (Ok (Sum10A [5,-1,-2,12])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[5,-1,-2,12]]"))
  , test "8" (\_ -> equal (Ok (Sum10B (Just [-2,8,-8,10,-10]))) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10B\",[-2,8,-8,10,-10]]"))
  , test "9" (\_ -> equal (Ok (Sum10A [16,-16,-10,8])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10A\",[16,-16,-10,8]]"))
  , test "10" (\_ -> equal (Ok (Sum10C [-17,2,3] [14,-18,10,12,0,-8])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[-17,2,3],[14,-18,10,12,0,-8]]]"))
  , test "11" (\_ -> equal (Ok (Sum10C [-12,7,16,15,9,17,1,-15,5,-13,-13,16,10,5] [-2,-13,-4,1,6,-4,-10,-2,-8,18])) (Json.Decode.decodeString (jsonDecSum10 (Json.Decode.list Json.Decode.int)) "[\"Sum10C\",[[-12,7,16,15,9,17,1,-15,5,-13,-13,16,10,5],[-2,-13,-4,1,6,-4,-10,-2,-8,18]]]"))
  ]

sumDecode11 : Test
sumDecode11 = describe "Sum decode 11"
  [ test "1" (\_ -> equal (Ok (Sum11B (Just []))) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11B\",[]]"))
  , test "2" (\_ -> equal (Ok (Sum11C [0,0] [])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11C\",[[0,0],[]]]"))
  , test "3" (\_ -> equal (Ok (Sum11D {foo = [-4]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[-4]}]"))
  , test "4" (\_ -> equal (Ok (Sum11C [4] [-1,-6,-4])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11C\",[[4],[-1,-6,-4]]]"))
  , test "5" (\_ -> equal (Ok (Sum11A [])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[]]"))
  , test "6" (\_ -> equal (Ok (Sum11A [-6,-9,-10,-7,8,1])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[-6,-9,-10,-7,8,1]]"))
  , test "7" (\_ -> equal (Ok (Sum11A [3,-3,-10,-2])) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11A\",[3,-3,-10,-2]]"))
  , test "8" (\_ -> equal (Ok (Sum11D {foo = [-2,-11]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[-2,-11]}]"))
  , test "9" (\_ -> equal (Ok (Sum11D {foo = [-7,-4,-12,-3,5,5,-10,-5,3,7,-9,-16,5]})) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11D\",{\"foo\":[-7,-4,-12,-3,5,5,-10,-5,3,7,-9,-16,5]}]"))
  , test "10" (\_ -> equal (Ok (Sum11B (Just [1,17,11]))) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11B\",[1,17,11]]"))
  , test "11" (\_ -> equal (Ok (Sum11B (Just [-16,2,10,-14,-9,-16,15,0,19,15,18,15,12,12,-9,1]))) (Json.Decode.decodeString (jsonDecSum11 (Json.Decode.list Json.Decode.int)) "[\"Sum11B\",[-16,2,10,-14,-9,-16,15,0,19,15,18,15,12,12,-9,1]]"))
  ]

sumDecode12 : Test
sumDecode12 = describe "Sum decode 12"
  [ test "1" (\_ -> equal (Ok (Sum12A [])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12A\",[]]"))
  , test "2" (\_ -> equal (Ok (Sum12E {bar = -1, baz = -1})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":-1,\"baz\":-1}]"))
  , test "3" (\_ -> equal (Ok (Sum12D {foo = [1,3,1,1]})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12D\",{\"foo\":[1,3,1,1]}]"))
  , test "4" (\_ -> equal (Ok (Sum12B (Just [3,4,5,0,-1]))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[3,4,5,0,-1]]"))
  , test "5" (\_ -> equal (Ok (Sum12D {foo = [-2,2]})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12D\",{\"foo\":[-2,2]}]"))
  , test "6" (\_ -> equal (Ok (Sum12D {foo = [2]})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12D\",{\"foo\":[2]}]"))
  , test "7" (\_ -> equal (Ok (Sum12E {bar = 0, baz = 9})) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12E\",{\"bar\":0,\"baz\":9}]"))
  , test "8" (\_ -> equal (Ok (Sum12B (Just [3,2,7,13,8,-5,6,13,3]))) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12B\",[3,2,7,13,8,-5,6,13,3]]"))
  , test "9" (\_ -> equal (Ok (Sum12C [13,5,14,6,15,-15,-15,-10,16,-3,13,13] [-15,-16,-5,9,13,2,0,6,-7,1,9,15,4])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12C\",[[13,5,14,6,15,-15,-15,-10,16,-3,13,13],[-15,-16,-5,9,13,2,0,6,-7,1,9,15,4]]]"))
  , test "10" (\_ -> equal (Ok (Sum12A [-7,17,-2,4,18,5,3,-5,1,-18,2,-4,-2,2,9])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12A\",[-7,17,-2,4,18,5,3,-5,1,-18,2,-4,-2,2,9]]"))
  , test "11" (\_ -> equal (Ok (Sum12A [-19,15,-1,7])) (Json.Decode.decodeString (jsonDecSum12 (Json.Decode.list Json.Decode.int)) "[\"Sum12A\",[-19,15,-1,7]]"))
  ]

recordDecode1 : Test
recordDecode1 = describe "Record decode 1"
  [ test "1" (\_ -> equal (Ok (Record1 {foo = 0, bar = Just 0, baz = [], qux = Just [], jmap = fromList [("a",0)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[],\"jmap\":{\"a\":0}}"))
  , test "2" (\_ -> equal (Ok (Record1 {foo = 0, bar = Just 0, baz = [-1,0], qux = Just [], jmap = fromList [("a",-1)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":0,\"bar\":0,\"baz\":[-1,0],\"qux\":[],\"jmap\":{\"a\":-1}}"))
  , test "3" (\_ -> equal (Ok (Record1 {foo = -3, bar = Just (-1), baz = [-1,-2,-3], qux = Just [4,-4,3,3], jmap = fromList [("a",1)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-3,\"bar\":-1,\"baz\":[-1,-2,-3],\"qux\":[4,-4,3,3],\"jmap\":{\"a\":1}}"))
  , test "4" (\_ -> equal (Ok (Record1 {foo = -6, bar = Just 2, baz = [1,-3,-1,4], qux = Just [-4,6,5], jmap = fromList [("a",-1)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-6,\"bar\":2,\"baz\":[1,-3,-1,4],\"qux\":[-4,6,5],\"jmap\":{\"a\":-1}}"))
  , test "5" (\_ -> equal (Ok (Record1 {foo = 4, bar = Just 0, baz = [], qux = Just [2,-2,8], jmap = fromList [("a",8)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":4,\"bar\":0,\"baz\":[],\"qux\":[2,-2,8],\"jmap\":{\"a\":8}}"))
  , test "6" (\_ -> equal (Ok (Record1 {foo = -3, bar = Just 0, baz = [-5,-7,8,-5], qux = Just [5,2], jmap = fromList [("a",-4)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-3,\"bar\":0,\"baz\":[-5,-7,8,-5],\"qux\":[5,2],\"jmap\":{\"a\":-4}}"))
  , test "7" (\_ -> equal (Ok (Record1 {foo = -6, bar = Just 5, baz = [12], qux = Just [-6,2,-8,8,-7,4,11,8,-3,2,9], jmap = fromList [("a",-1)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-6,\"bar\":5,\"baz\":[12],\"qux\":[-6,2,-8,8,-7,4,11,8,-3,2,9],\"jmap\":{\"a\":-1}}"))
  , test "8" (\_ -> equal (Ok (Record1 {foo = -11, bar = Just 1, baz = [12,6,-7,6], qux = Just [-2,12,-7,0,1,2,10,-13], jmap = fromList [("a",12)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-11,\"bar\":1,\"baz\":[12,6,-7,6],\"qux\":[-2,12,-7,0,1,2,10,-13],\"jmap\":{\"a\":12}}"))
  , test "9" (\_ -> equal (Ok (Record1 {foo = -10, bar = Just 2, baz = [-7,-7,1,-16,3,-11,5], qux = Just [9,10,-7], jmap = fromList [("a",6)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-10,\"bar\":2,\"baz\":[-7,-7,1,-16,3,-11,5],\"qux\":[9,10,-7],\"jmap\":{\"a\":6}}"))
  , test "10" (\_ -> equal (Ok (Record1 {foo = 2, bar = Just 10, baz = [12,11,10,-10,4,2,-18], qux = Just [8,-16,5,-1,-11,6,-11], jmap = fromList [("a",15)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":2,\"bar\":10,\"baz\":[12,11,10,-10,4,2,-18],\"qux\":[8,-16,5,-1,-11,6,-11],\"jmap\":{\"a\":15}}"))
  , test "11" (\_ -> equal (Ok (Record1 {foo = -3, bar = Just 9, baz = [-10], qux = Just [16,18,14,0,19,-1,-3,0,-13,16,13,-14,18,-14], jmap = fromList [("a",19)]})) (Json.Decode.decodeString (jsonDecRecord1 (Json.Decode.list Json.Decode.int)) "{\"foo\":-3,\"bar\":9,\"baz\":[-10],\"qux\":[16,18,14,0,19,-1,-3,0,-13,16,13,-14,18,-14],\"jmap\":{\"a\":19}}"))
  ]

recordDecode2 : Test
recordDecode2 = describe "Record decode 2"
  [ test "1" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just 0, baz = [], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":0,\"qux\":[],\"foo\":0,\"baz\":[]}"))
  , test "2" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just (-2), baz = [2], qux = Just []})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-2,\"qux\":[],\"foo\":0,\"baz\":[2]}"))
  , test "3" (\_ -> equal (Ok (Record2 {foo = 1, bar = Just (-1), baz = [2,0], qux = Just [4,-3,1]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-1,\"qux\":[4,-3,1],\"foo\":1,\"baz\":[2,0]}"))
  , test "4" (\_ -> equal (Ok (Record2 {foo = 1, bar = Just (-1), baz = [3], qux = Just [2,-2,3,6,-6,-6]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-1,\"qux\":[2,-2,3,6,-6,-6],\"foo\":1,\"baz\":[3]}"))
  , test "5" (\_ -> equal (Ok (Record2 {foo = 7, bar = Just 0, baz = [3,-1,2,5,-8,3,4], qux = Just [-6,0,-3]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":0,\"qux\":[-6,0,-3],\"foo\":7,\"baz\":[3,-1,2,5,-8,3,4]}"))
  , test "6" (\_ -> equal (Ok (Record2 {foo = -1, bar = Just (-10), baz = [7,7,6,8,10,5,5], qux = Just [-9,-5,-5,9,4,5,7,-8,-7]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-10,\"qux\":[-9,-5,-5,9,4,5,7,-8,-7],\"foo\":-1,\"baz\":[7,7,6,8,10,5,5]}"))
  , test "7" (\_ -> equal (Ok (Record2 {foo = 10, bar = Just (-4), baz = [1,-2,-6,0,11,8,10,2,-1,-5,-3], qux = Just [-1,-9,-8,-3,6,8,3]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-4,\"qux\":[-1,-9,-8,-3,6,8,3],\"foo\":10,\"baz\":[1,-2,-6,0,11,8,10,2,-1,-5,-3]}"))
  , test "8" (\_ -> equal (Ok (Record2 {foo = 0, bar = Just (-7), baz = [-2,11,7,1], qux = Just [-8,4,-4,10,6,-7,7,4,-7,-12]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-7,\"qux\":[-8,4,-4,10,6,-7,7,4,-7,-12],\"foo\":0,\"baz\":[-2,11,7,1]}"))
  , test "9" (\_ -> equal (Ok (Record2 {foo = -16, bar = Just (-12), baz = [14,-14,-13,13,-9,-10,-13,-8], qux = Just [-10,12,-10,-8]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-12,\"qux\":[-10,12,-10,-8],\"foo\":-16,\"baz\":[14,-14,-13,13,-9,-10,-13,-8]}"))
  , test "10" (\_ -> equal (Ok (Record2 {foo = 10, bar = Just (-4), baz = [5,-11,-16,-9,-8,16,-10,-8,15,-10,3,-11,13,2,1,1], qux = Just [16,-1,-5,3,-13,13,1]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-4,\"qux\":[16,-1,-5,3,-13,13,1],\"foo\":10,\"baz\":[5,-11,-16,-9,-8,16,-10,-8,15,-10,3,-11,13,2,1,1]}"))
  , test "11" (\_ -> equal (Ok (Record2 {foo = 1, bar = Just (-5), baz = [-18,-2,2,-14,6,-19,-8,-12,-15,-8,20,9,-20,-17,16], qux = Just [11,-19,14,-1,8,-4,-15,11,-16]})) (Json.Decode.decodeString (jsonDecRecord2 (Json.Decode.list Json.Decode.int)) "{\"bar\":-5,\"qux\":[11,-19,14,-1,8,-4,-15,11,-16],\"foo\":1,\"baz\":[-18,-2,2,-14,6,-19,-8,-12,-15,-8,20,9,-20,-17,16]}"))
  ]

recordEncode1 : Test
recordEncode1 = describe "Record encode 1"
  [ test "1" (\_ -> equalHack "{\"foo\":0,\"bar\":0,\"baz\":[],\"qux\":[],\"jmap\":{\"a\":0}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 0, bar = Just 0, baz = [], qux = Just [], jmap = fromList [("a",0)]}))))
  , test "2" (\_ -> equalHack "{\"foo\":0,\"bar\":0,\"baz\":[-1,0],\"qux\":[],\"jmap\":{\"a\":-1}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 0, bar = Just 0, baz = [-1,0], qux = Just [], jmap = fromList [("a",-1)]}))))
  , test "3" (\_ -> equalHack "{\"foo\":-3,\"bar\":-1,\"baz\":[-1,-2,-3],\"qux\":[4,-4,3,3],\"jmap\":{\"a\":1}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -3, bar = Just (-1), baz = [-1,-2,-3], qux = Just [4,-4,3,3], jmap = fromList [("a",1)]}))))
  , test "4" (\_ -> equalHack "{\"foo\":-6,\"bar\":2,\"baz\":[1,-3,-1,4],\"qux\":[-4,6,5],\"jmap\":{\"a\":-1}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -6, bar = Just 2, baz = [1,-3,-1,4], qux = Just [-4,6,5], jmap = fromList [("a",-1)]}))))
  , test "5" (\_ -> equalHack "{\"foo\":4,\"bar\":0,\"baz\":[],\"qux\":[2,-2,8],\"jmap\":{\"a\":8}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 4, bar = Just 0, baz = [], qux = Just [2,-2,8], jmap = fromList [("a",8)]}))))
  , test "6" (\_ -> equalHack "{\"foo\":-3,\"bar\":0,\"baz\":[-5,-7,8,-5],\"qux\":[5,2],\"jmap\":{\"a\":-4}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -3, bar = Just 0, baz = [-5,-7,8,-5], qux = Just [5,2], jmap = fromList [("a",-4)]}))))
  , test "7" (\_ -> equalHack "{\"foo\":-6,\"bar\":5,\"baz\":[12],\"qux\":[-6,2,-8,8,-7,4,11,8,-3,2,9],\"jmap\":{\"a\":-1}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -6, bar = Just 5, baz = [12], qux = Just [-6,2,-8,8,-7,4,11,8,-3,2,9], jmap = fromList [("a",-1)]}))))
  , test "8" (\_ -> equalHack "{\"foo\":-11,\"bar\":1,\"baz\":[12,6,-7,6],\"qux\":[-2,12,-7,0,1,2,10,-13],\"jmap\":{\"a\":12}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -11, bar = Just 1, baz = [12,6,-7,6], qux = Just [-2,12,-7,0,1,2,10,-13], jmap = fromList [("a",12)]}))))
  , test "9" (\_ -> equalHack "{\"foo\":-10,\"bar\":2,\"baz\":[-7,-7,1,-16,3,-11,5],\"qux\":[9,10,-7],\"jmap\":{\"a\":6}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -10, bar = Just 2, baz = [-7,-7,1,-16,3,-11,5], qux = Just [9,10,-7], jmap = fromList [("a",6)]}))))
  , test "10" (\_ -> equalHack "{\"foo\":2,\"bar\":10,\"baz\":[12,11,10,-10,4,2,-18],\"qux\":[8,-16,5,-1,-11,6,-11],\"jmap\":{\"a\":15}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = 2, bar = Just 10, baz = [12,11,10,-10,4,2,-18], qux = Just [8,-16,5,-1,-11,6,-11], jmap = fromList [("a",15)]}))))
  , test "11" (\_ -> equalHack "{\"foo\":-3,\"bar\":9,\"baz\":[-10],\"qux\":[16,18,14,0,19,-1,-3,0,-13,16,13,-14,18,-14],\"jmap\":{\"a\":19}}"(Json.Encode.encode 0 (jsonEncRecord1(Json.Encode.list Json.Encode.int) (Record1 {foo = -3, bar = Just 9, baz = [-10], qux = Just [16,18,14,0,19,-1,-3,0,-13,16,13,-14,18,-14], jmap = fromList [("a",19)]}))))
  ]

recordEncode2 : Test
recordEncode2 = describe "Record encode 2"
  [ test "1" (\_ -> equalHack "{\"bar\":0,\"qux\":[],\"foo\":0,\"baz\":[]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 0, bar = Just 0, baz = [], qux = Just []}))))
  , test "2" (\_ -> equalHack "{\"bar\":-2,\"qux\":[],\"foo\":0,\"baz\":[2]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 0, bar = Just (-2), baz = [2], qux = Just []}))))
  , test "3" (\_ -> equalHack "{\"bar\":-1,\"qux\":[4,-3,1],\"foo\":1,\"baz\":[2,0]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 1, bar = Just (-1), baz = [2,0], qux = Just [4,-3,1]}))))
  , test "4" (\_ -> equalHack "{\"bar\":-1,\"qux\":[2,-2,3,6,-6,-6],\"foo\":1,\"baz\":[3]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 1, bar = Just (-1), baz = [3], qux = Just [2,-2,3,6,-6,-6]}))))
  , test "5" (\_ -> equalHack "{\"bar\":0,\"qux\":[-6,0,-3],\"foo\":7,\"baz\":[3,-1,2,5,-8,3,4]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 7, bar = Just 0, baz = [3,-1,2,5,-8,3,4], qux = Just [-6,0,-3]}))))
  , test "6" (\_ -> equalHack "{\"bar\":-10,\"qux\":[-9,-5,-5,9,4,5,7,-8,-7],\"foo\":-1,\"baz\":[7,7,6,8,10,5,5]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -1, bar = Just (-10), baz = [7,7,6,8,10,5,5], qux = Just [-9,-5,-5,9,4,5,7,-8,-7]}))))
  , test "7" (\_ -> equalHack "{\"bar\":-4,\"qux\":[-1,-9,-8,-3,6,8,3],\"foo\":10,\"baz\":[1,-2,-6,0,11,8,10,2,-1,-5,-3]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 10, bar = Just (-4), baz = [1,-2,-6,0,11,8,10,2,-1,-5,-3], qux = Just [-1,-9,-8,-3,6,8,3]}))))
  , test "8" (\_ -> equalHack "{\"bar\":-7,\"qux\":[-8,4,-4,10,6,-7,7,4,-7,-12],\"foo\":0,\"baz\":[-2,11,7,1]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 0, bar = Just (-7), baz = [-2,11,7,1], qux = Just [-8,4,-4,10,6,-7,7,4,-7,-12]}))))
  , test "9" (\_ -> equalHack "{\"bar\":-12,\"qux\":[-10,12,-10,-8],\"foo\":-16,\"baz\":[14,-14,-13,13,-9,-10,-13,-8]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = -16, bar = Just (-12), baz = [14,-14,-13,13,-9,-10,-13,-8], qux = Just [-10,12,-10,-8]}))))
  , test "10" (\_ -> equalHack "{\"bar\":-4,\"qux\":[16,-1,-5,3,-13,13,1],\"foo\":10,\"baz\":[5,-11,-16,-9,-8,16,-10,-8,15,-10,3,-11,13,2,1,1]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 10, bar = Just (-4), baz = [5,-11,-16,-9,-8,16,-10,-8,15,-10,3,-11,13,2,1,1], qux = Just [16,-1,-5,3,-13,13,1]}))))
  , test "11" (\_ -> equalHack "{\"bar\":-5,\"qux\":[11,-19,14,-1,8,-4,-15,11,-16],\"foo\":1,\"baz\":[-18,-2,2,-14,6,-19,-8,-12,-15,-8,20,9,-20,-17,16]}"(Json.Encode.encode 0 (jsonEncRecord2(Json.Encode.list Json.Encode.int) (Record2 {foo = 1, bar = Just (-5), baz = [-18,-2,2,-14,6,-19,-8,-12,-15,-8,20,9,-20,-17,16], qux = Just [11,-19,14,-1,8,-4,-15,11,-16]}))))
  ]

simpleEncode01 : Test
simpleEncode01 = describe "Simple encode 01"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 []))))
  , test "2" (\_ -> equalHack "[-1]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-1]))))
  , test "3" (\_ -> equalHack "[-3]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-3]))))
  , test "4" (\_ -> equalHack "[0,5,4,1,1,3]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [0,5,4,1,1,3]))))
  , test "5" (\_ -> equalHack "[7,0,-4,2,-6]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [7,0,-4,2,-6]))))
  , test "6" (\_ -> equalHack "[3]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [3]))))
  , test "7" (\_ -> equalHack "[11,3,5,9,-9,-3,0]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [11,3,5,9,-9,-3,0]))))
  , test "8" (\_ -> equalHack "[-7,-13,-14,-4,-11,5,10,4,0]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-7,-13,-14,-4,-11,5,10,4,0]))))
  , test "9" (\_ -> equalHack "[14]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [14]))))
  , test "10" (\_ -> equalHack "[-10,3,14,-11,-7,8,-16,-1,9]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-10,3,14,-11,-7,8,-16,-1,9]))))
  , test "11" (\_ -> equalHack "[-5,5,18,13,10,17,-9,-3,12,-7]"(Json.Encode.encode 0 (jsonEncSimple01(Json.Encode.list Json.Encode.int) (Simple01 [-5,5,18,13,10,17,-9,-3,12,-7]))))
  ]

simpleEncode02 : Test
simpleEncode02 = describe "Simple encode 02"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 []))))
  , test "2" (\_ -> equalHack "[1,2]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [1,2]))))
  , test "3" (\_ -> equalHack "[-4,-2]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-4,-2]))))
  , test "4" (\_ -> equalHack "[5,3,-5,-4]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [5,3,-5,-4]))))
  , test "5" (\_ -> equalHack "[-1,-3,-3,-4,-5,-3,3,1]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-1,-3,-3,-4,-5,-3,3,1]))))
  , test "6" (\_ -> equalHack "[-1,-5]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-1,-5]))))
  , test "7" (\_ -> equalHack "[-8,9,12,-4,2,-7,-2,12,9,7,3,-10]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-8,9,12,-4,2,-7,-2,12,9,7,3,-10]))))
  , test "8" (\_ -> equalHack "[3,-4,-11,10,5,13]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [3,-4,-11,10,5,13]))))
  , test "9" (\_ -> equalHack "[-15,-12,6,14,-6]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-15,-12,6,14,-6]))))
  , test "10" (\_ -> equalHack "[-1,-18,13,4,9]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [-1,-18,13,4,9]))))
  , test "11" (\_ -> equalHack "[20,-18,16,-1,-10,12,6,11,-15,-1,-17,4,9]"(Json.Encode.encode 0 (jsonEncSimple02(Json.Encode.list Json.Encode.int) (Simple02 [20,-18,16,-1,-10,12,6,11,-15,-1,-17,4,9]))))
  ]

simpleEncode03 : Test
simpleEncode03 = describe "Simple encode 03"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 []))))
  , test "2" (\_ -> equalHack "[1]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [1]))))
  , test "3" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 []))))
  , test "4" (\_ -> equalHack "[-3,1]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-3,1]))))
  , test "5" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 []))))
  , test "6" (\_ -> equalHack "[2,-3,7,-1,9,-9,6,1,8]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [2,-3,7,-1,9,-9,6,1,8]))))
  , test "7" (\_ -> equalHack "[1,-8]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [1,-8]))))
  , test "8" (\_ -> equalHack "[-13,2,4,-3]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [-13,2,4,-3]))))
  , test "9" (\_ -> equalHack "[11,0,-13,-10,-7]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [11,0,-13,-10,-7]))))
  , test "10" (\_ -> equalHack "[4,3,-6,-15,1,-7,13,13,11,5,-11,9,-12,3,9,15]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 [4,3,-6,-15,1,-7,13,13,11,5,-11,9,-12,3,9,15]))))
  , test "11" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple03(Json.Encode.list Json.Encode.int) (Simple03 []))))
  ]

simpleEncode04 : Test
simpleEncode04 = describe "Simple encode 04"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 []))))
  , test "2" (\_ -> equalHack "[-1]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-1]))))
  , test "3" (\_ -> equalHack "[2]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [2]))))
  , test "4" (\_ -> equalHack "[-3]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [-3]))))
  , test "5" (\_ -> equalHack "[8,-1,-6]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [8,-1,-6]))))
  , test "6" (\_ -> equalHack "[9,-4,-9,-10,-4,1,-1,5,-10,-1]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [9,-4,-9,-10,-4,1,-1,5,-10,-1]))))
  , test "7" (\_ -> equalHack "[9,-9,-11,-6,8,0,-4,6,-3,0]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [9,-9,-11,-6,8,0,-4,6,-3,0]))))
  , test "8" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 []))))
  , test "9" (\_ -> equalHack "[1,-13,1,16,-12,0,13,4,-14,10,-6]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [1,-13,1,16,-12,0,13,4,-14,10,-6]))))
  , test "10" (\_ -> equalHack "[16,-14,-13,-18,-12,6,-11,10]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [16,-14,-13,-18,-12,6,-11,10]))))
  , test "11" (\_ -> equalHack "[9,-15,15,-19,3,16,-10,-10]"(Json.Encode.encode 0 (jsonEncSimple04(Json.Encode.list Json.Encode.int) (Simple04 [9,-15,15,-19,3,16,-10,-10]))))
  ]

simpleDecode01 : Test
simpleDecode01 = describe "Simple decode 01"
  [ test "1" (\_ -> equal (Ok (Simple01 [])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple01 [-1])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-1]"))
  , test "3" (\_ -> equal (Ok (Simple01 [-3])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-3]"))
  , test "4" (\_ -> equal (Ok (Simple01 [0,5,4,1,1,3])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[0,5,4,1,1,3]"))
  , test "5" (\_ -> equal (Ok (Simple01 [7,0,-4,2,-6])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[7,0,-4,2,-6]"))
  , test "6" (\_ -> equal (Ok (Simple01 [3])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[3]"))
  , test "7" (\_ -> equal (Ok (Simple01 [11,3,5,9,-9,-3,0])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[11,3,5,9,-9,-3,0]"))
  , test "8" (\_ -> equal (Ok (Simple01 [-7,-13,-14,-4,-11,5,10,4,0])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-7,-13,-14,-4,-11,5,10,4,0]"))
  , test "9" (\_ -> equal (Ok (Simple01 [14])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[14]"))
  , test "10" (\_ -> equal (Ok (Simple01 [-10,3,14,-11,-7,8,-16,-1,9])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-10,3,14,-11,-7,8,-16,-1,9]"))
  , test "11" (\_ -> equal (Ok (Simple01 [-5,5,18,13,10,17,-9,-3,12,-7])) (Json.Decode.decodeString (jsonDecSimple01 (Json.Decode.list Json.Decode.int)) "[-5,5,18,13,10,17,-9,-3,12,-7]"))
  ]

simpleDecode02 : Test
simpleDecode02 = describe "Simple decode 02"
  [ test "1" (\_ -> equal (Ok (Simple02 [])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple02 [1,2])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[1,2]"))
  , test "3" (\_ -> equal (Ok (Simple02 [-4,-2])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-4,-2]"))
  , test "4" (\_ -> equal (Ok (Simple02 [5,3,-5,-4])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[5,3,-5,-4]"))
  , test "5" (\_ -> equal (Ok (Simple02 [-1,-3,-3,-4,-5,-3,3,1])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-1,-3,-3,-4,-5,-3,3,1]"))
  , test "6" (\_ -> equal (Ok (Simple02 [-1,-5])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-1,-5]"))
  , test "7" (\_ -> equal (Ok (Simple02 [-8,9,12,-4,2,-7,-2,12,9,7,3,-10])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-8,9,12,-4,2,-7,-2,12,9,7,3,-10]"))
  , test "8" (\_ -> equal (Ok (Simple02 [3,-4,-11,10,5,13])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[3,-4,-11,10,5,13]"))
  , test "9" (\_ -> equal (Ok (Simple02 [-15,-12,6,14,-6])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-15,-12,6,14,-6]"))
  , test "10" (\_ -> equal (Ok (Simple02 [-1,-18,13,4,9])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[-1,-18,13,4,9]"))
  , test "11" (\_ -> equal (Ok (Simple02 [20,-18,16,-1,-10,12,6,11,-15,-1,-17,4,9])) (Json.Decode.decodeString (jsonDecSimple02 (Json.Decode.list Json.Decode.int)) "[20,-18,16,-1,-10,12,6,11,-15,-1,-17,4,9]"))
  ]

simpleDecode03 : Test
simpleDecode03 = describe "Simple decode 03"
  [ test "1" (\_ -> equal (Ok (Simple03 [])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple03 [1])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[1]"))
  , test "3" (\_ -> equal (Ok (Simple03 [])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "4" (\_ -> equal (Ok (Simple03 [-3,1])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-3,1]"))
  , test "5" (\_ -> equal (Ok (Simple03 [])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "6" (\_ -> equal (Ok (Simple03 [2,-3,7,-1,9,-9,6,1,8])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[2,-3,7,-1,9,-9,6,1,8]"))
  , test "7" (\_ -> equal (Ok (Simple03 [1,-8])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[1,-8]"))
  , test "8" (\_ -> equal (Ok (Simple03 [-13,2,4,-3])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[-13,2,4,-3]"))
  , test "9" (\_ -> equal (Ok (Simple03 [11,0,-13,-10,-7])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[11,0,-13,-10,-7]"))
  , test "10" (\_ -> equal (Ok (Simple03 [4,3,-6,-15,1,-7,13,13,11,5,-11,9,-12,3,9,15])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[4,3,-6,-15,1,-7,13,13,11,5,-11,9,-12,3,9,15]"))
  , test "11" (\_ -> equal (Ok (Simple03 [])) (Json.Decode.decodeString (jsonDecSimple03 (Json.Decode.list Json.Decode.int)) "[]"))
  ]

simpleDecode04 : Test
simpleDecode04 = describe "Simple decode 04"
  [ test "1" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (Simple04 [-1])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-1]"))
  , test "3" (\_ -> equal (Ok (Simple04 [2])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[2]"))
  , test "4" (\_ -> equal (Ok (Simple04 [-3])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[-3]"))
  , test "5" (\_ -> equal (Ok (Simple04 [8,-1,-6])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[8,-1,-6]"))
  , test "6" (\_ -> equal (Ok (Simple04 [9,-4,-9,-10,-4,1,-1,5,-10,-1])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[9,-4,-9,-10,-4,1,-1,5,-10,-1]"))
  , test "7" (\_ -> equal (Ok (Simple04 [9,-9,-11,-6,8,0,-4,6,-3,0])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[9,-9,-11,-6,8,0,-4,6,-3,0]"))
  , test "8" (\_ -> equal (Ok (Simple04 [])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "9" (\_ -> equal (Ok (Simple04 [1,-13,1,16,-12,0,13,4,-14,10,-6])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[1,-13,1,16,-12,0,13,4,-14,10,-6]"))
  , test "10" (\_ -> equal (Ok (Simple04 [16,-14,-13,-18,-12,6,-11,10])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[16,-14,-13,-18,-12,6,-11,10]"))
  , test "11" (\_ -> equal (Ok (Simple04 [9,-15,15,-19,3,16,-10,-10])) (Json.Decode.decodeString (jsonDecSimple04 (Json.Decode.list Json.Decode.int)) "[9,-15,15,-19,3,16,-10,-10]"))
  ]

simplerecordEncode01 : Test
simplerecordEncode01 = describe "SimpleRecord encode 01"
  [ test "1" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "2" (\_ -> equalHack "{\"qux\":[-1,0]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-1,0]}))))
  , test "3" (\_ -> equalHack "{\"qux\":[0,-1,1]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [0,-1,1]}))))
  , test "4" (\_ -> equalHack "{\"qux\":[-2]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-2]}))))
  , test "5" (\_ -> equalHack "{\"qux\":[8,0,-2,-5,1,-8,3,-6]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [8,0,-2,-5,1,-8,3,-6]}))))
  , test "6" (\_ -> equalHack "{\"qux\":[-6,9,10]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-6,9,10]}))))
  , test "7" (\_ -> equalHack "{\"qux\":[10,7,-1,7,9,7,7,4,10,8]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [10,7,-1,7,9,7,7,4,10,8]}))))
  , test "8" (\_ -> equalHack "{\"qux\":[10,2,-14,-10,-2]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [10,2,-14,-10,-2]}))))
  , test "9" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = []}))))
  , test "10" (\_ -> equalHack "{\"qux\":[-8,1,18,9,-4,-2,-10,17,3,15,0,-8,-12,-17]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-8,1,18,9,-4,-2,-10,17,3,15,0,-8,-12,-17]}))))
  , test "11" (\_ -> equalHack "{\"qux\":[-16,11,-4,-17,-2,-15,9,-12,-11,-4,13,7,11,-17,13,10,-9,-11]}"(Json.Encode.encode 0 (jsonEncSimpleRecord01(Json.Encode.list Json.Encode.int) (SimpleRecord01 {qux = [-16,11,-4,-17,-2,-15,9,-12,-11,-4,13,7,11,-17,13,10,-9,-11]}))))
  ]

simplerecordEncode02 : Test
simplerecordEncode02 = describe "SimpleRecord encode 02"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = []}))))
  , test "2" (\_ -> equalHack "[2,1]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [2,1]}))))
  , test "3" (\_ -> equalHack "[2]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [2]}))))
  , test "4" (\_ -> equalHack "[1,-1]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [1,-1]}))))
  , test "5" (\_ -> equalHack "[4,-4,4,-6,8,3,-4,8]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [4,-4,4,-6,8,3,-4,8]}))))
  , test "6" (\_ -> equalHack "[-1,0,7,-10,-4]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-1,0,7,-10,-4]}))))
  , test "7" (\_ -> equalHack "[-2,7,0,-11,4,1,0,-6,8]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-2,7,0,-11,4,1,0,-6,8]}))))
  , test "8" (\_ -> equalHack "[-13]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-13]}))))
  , test "9" (\_ -> equalHack "[9,-7,0,7,9,-12,-3,-5,-6]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [9,-7,0,7,9,-12,-3,-5,-6]}))))
  , test "10" (\_ -> equalHack "[-15,-12,8,-14,-5,18,15,2,15,18,4]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-15,-12,8,-14,-5,18,15,2,15,18,4]}))))
  , test "11" (\_ -> equalHack "[-4,2,-11,-19]"(Json.Encode.encode 0 (jsonEncSimpleRecord02(Json.Encode.list Json.Encode.int) (SimpleRecord02 {qux = [-4,2,-11,-19]}))))
  ]

simplerecordEncode03 : Test
simplerecordEncode03 = describe "SimpleRecord encode 03"
  [ test "1" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = []}))))
  , test "2" (\_ -> equalHack "{\"qux\":[]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = []}))))
  , test "3" (\_ -> equalHack "{\"qux\":[3,-2]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [3,-2]}))))
  , test "4" (\_ -> equalHack "{\"qux\":[6,0]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [6,0]}))))
  , test "5" (\_ -> equalHack "{\"qux\":[0,-6,5,-5,7]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [0,-6,5,-5,7]}))))
  , test "6" (\_ -> equalHack "{\"qux\":[-9,-5]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-9,-5]}))))
  , test "7" (\_ -> equalHack "{\"qux\":[-8,11,-12,-5,4,-4,8]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-8,11,-12,-5,4,-4,8]}))))
  , test "8" (\_ -> equalHack "{\"qux\":[-2,-2,-2]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-2,-2,-2]}))))
  , test "9" (\_ -> equalHack "{\"qux\":[-5,4,-10,2,-14,13,-4,9,14,14,8,-5]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-5,4,-10,2,-14,13,-4,9,14,14,8,-5]}))))
  , test "10" (\_ -> equalHack "{\"qux\":[7,2,4,-11,-12,-8,5,6,-2,18,11,-6,5]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [7,2,4,-11,-12,-8,5,6,-2,18,11,-6,5]}))))
  , test "11" (\_ -> equalHack "{\"qux\":[-10,9,-8,7,-11,14,-18,-10,-7,-8,-11,19,-18,13]}"(Json.Encode.encode 0 (jsonEncSimpleRecord03(Json.Encode.list Json.Encode.int) (SimpleRecord03 {qux = [-10,9,-8,7,-11,14,-18,-10,-7,-8,-11,19,-18,13]}))))
  ]

simplerecordEncode04 : Test
simplerecordEncode04 = describe "SimpleRecord encode 04"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = []}))))
  , test "2" (\_ -> equalHack "[0]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [0]}))))
  , test "3" (\_ -> equalHack "[-4,-2,-4]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-4,-2,-4]}))))
  , test "4" (\_ -> equalHack "[-3]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-3]}))))
  , test "5" (\_ -> equalHack "[1,-3,3,-1,-7,2,2,8]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [1,-3,3,-1,-7,2,2,8]}))))
  , test "6" (\_ -> equalHack "[9]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [9]}))))
  , test "7" (\_ -> equalHack "[-4,-2,-4,2,6,9,3,-4,0,9]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-4,-2,-4,2,6,9,3,-4,0,9]}))))
  , test "8" (\_ -> equalHack "[3,4,-10,-14,5,13,12,8,-2,-6,2,-3]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [3,4,-10,-14,5,13,12,8,-2,-6,2,-3]}))))
  , test "9" (\_ -> equalHack "[-2,-3,16]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [-2,-3,16]}))))
  , test "10" (\_ -> equalHack "[18,18,-6]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = [18,18,-6]}))))
  , test "11" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSimpleRecord04(Json.Encode.list Json.Encode.int) (SimpleRecord04 {qux = []}))))
  ]

simplerecordDecode01 : Test
simplerecordDecode01 = describe "SimpleRecord decode 01"
  [ test "1" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (SimpleRecord01 {qux = [-1,0]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-1,0]}"))
  , test "3" (\_ -> equal (Ok (SimpleRecord01 {qux = [0,-1,1]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[0,-1,1]}"))
  , test "4" (\_ -> equal (Ok (SimpleRecord01 {qux = [-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-2]}"))
  , test "5" (\_ -> equal (Ok (SimpleRecord01 {qux = [8,0,-2,-5,1,-8,3,-6]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[8,0,-2,-5,1,-8,3,-6]}"))
  , test "6" (\_ -> equal (Ok (SimpleRecord01 {qux = [-6,9,10]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-6,9,10]}"))
  , test "7" (\_ -> equal (Ok (SimpleRecord01 {qux = [10,7,-1,7,9,7,7,4,10,8]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[10,7,-1,7,9,7,7,4,10,8]}"))
  , test "8" (\_ -> equal (Ok (SimpleRecord01 {qux = [10,2,-14,-10,-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[10,2,-14,-10,-2]}"))
  , test "9" (\_ -> equal (Ok (SimpleRecord01 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "10" (\_ -> equal (Ok (SimpleRecord01 {qux = [-8,1,18,9,-4,-2,-10,17,3,15,0,-8,-12,-17]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-8,1,18,9,-4,-2,-10,17,3,15,0,-8,-12,-17]}"))
  , test "11" (\_ -> equal (Ok (SimpleRecord01 {qux = [-16,11,-4,-17,-2,-15,9,-12,-11,-4,13,7,11,-17,13,10,-9,-11]})) (Json.Decode.decodeString (jsonDecSimpleRecord01 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-16,11,-4,-17,-2,-15,9,-12,-11,-4,13,7,11,-17,13,10,-9,-11]}"))
  ]

simplerecordDecode02 : Test
simplerecordDecode02 = describe "SimpleRecord decode 02"
  [ test "1" (\_ -> equal (Ok (SimpleRecord02 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SimpleRecord02 {qux = [2,1]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[2,1]"))
  , test "3" (\_ -> equal (Ok (SimpleRecord02 {qux = [2]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[2]"))
  , test "4" (\_ -> equal (Ok (SimpleRecord02 {qux = [1,-1]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[1,-1]"))
  , test "5" (\_ -> equal (Ok (SimpleRecord02 {qux = [4,-4,4,-6,8,3,-4,8]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[4,-4,4,-6,8,3,-4,8]"))
  , test "6" (\_ -> equal (Ok (SimpleRecord02 {qux = [-1,0,7,-10,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-1,0,7,-10,-4]"))
  , test "7" (\_ -> equal (Ok (SimpleRecord02 {qux = [-2,7,0,-11,4,1,0,-6,8]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-2,7,0,-11,4,1,0,-6,8]"))
  , test "8" (\_ -> equal (Ok (SimpleRecord02 {qux = [-13]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-13]"))
  , test "9" (\_ -> equal (Ok (SimpleRecord02 {qux = [9,-7,0,7,9,-12,-3,-5,-6]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[9,-7,0,7,9,-12,-3,-5,-6]"))
  , test "10" (\_ -> equal (Ok (SimpleRecord02 {qux = [-15,-12,8,-14,-5,18,15,2,15,18,4]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-15,-12,8,-14,-5,18,15,2,15,18,4]"))
  , test "11" (\_ -> equal (Ok (SimpleRecord02 {qux = [-4,2,-11,-19]})) (Json.Decode.decodeString (jsonDecSimpleRecord02 (Json.Decode.list Json.Decode.int)) "[-4,2,-11,-19]"))
  ]

simplerecordDecode03 : Test
simplerecordDecode03 = describe "SimpleRecord decode 03"
  [ test "1" (\_ -> equal (Ok (SimpleRecord03 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "2" (\_ -> equal (Ok (SimpleRecord03 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[]}"))
  , test "3" (\_ -> equal (Ok (SimpleRecord03 {qux = [3,-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[3,-2]}"))
  , test "4" (\_ -> equal (Ok (SimpleRecord03 {qux = [6,0]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[6,0]}"))
  , test "5" (\_ -> equal (Ok (SimpleRecord03 {qux = [0,-6,5,-5,7]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[0,-6,5,-5,7]}"))
  , test "6" (\_ -> equal (Ok (SimpleRecord03 {qux = [-9,-5]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-9,-5]}"))
  , test "7" (\_ -> equal (Ok (SimpleRecord03 {qux = [-8,11,-12,-5,4,-4,8]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-8,11,-12,-5,4,-4,8]}"))
  , test "8" (\_ -> equal (Ok (SimpleRecord03 {qux = [-2,-2,-2]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-2,-2,-2]}"))
  , test "9" (\_ -> equal (Ok (SimpleRecord03 {qux = [-5,4,-10,2,-14,13,-4,9,14,14,8,-5]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-5,4,-10,2,-14,13,-4,9,14,14,8,-5]}"))
  , test "10" (\_ -> equal (Ok (SimpleRecord03 {qux = [7,2,4,-11,-12,-8,5,6,-2,18,11,-6,5]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[7,2,4,-11,-12,-8,5,6,-2,18,11,-6,5]}"))
  , test "11" (\_ -> equal (Ok (SimpleRecord03 {qux = [-10,9,-8,7,-11,14,-18,-10,-7,-8,-11,19,-18,13]})) (Json.Decode.decodeString (jsonDecSimpleRecord03 (Json.Decode.list Json.Decode.int)) "{\"qux\":[-10,9,-8,7,-11,14,-18,-10,-7,-8,-11,19,-18,13]}"))
  ]

simplerecordDecode04 : Test
simplerecordDecode04 = describe "SimpleRecord decode 04"
  [ test "1" (\_ -> equal (Ok (SimpleRecord04 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SimpleRecord04 {qux = [0]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[0]"))
  , test "3" (\_ -> equal (Ok (SimpleRecord04 {qux = [-4,-2,-4]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-4,-2,-4]"))
  , test "4" (\_ -> equal (Ok (SimpleRecord04 {qux = [-3]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-3]"))
  , test "5" (\_ -> equal (Ok (SimpleRecord04 {qux = [1,-3,3,-1,-7,2,2,8]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[1,-3,3,-1,-7,2,2,8]"))
  , test "6" (\_ -> equal (Ok (SimpleRecord04 {qux = [9]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[9]"))
  , test "7" (\_ -> equal (Ok (SimpleRecord04 {qux = [-4,-2,-4,2,6,9,3,-4,0,9]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-4,-2,-4,2,6,9,3,-4,0,9]"))
  , test "8" (\_ -> equal (Ok (SimpleRecord04 {qux = [3,4,-10,-14,5,13,12,8,-2,-6,2,-3]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[3,4,-10,-14,5,13,12,8,-2,-6,2,-3]"))
  , test "9" (\_ -> equal (Ok (SimpleRecord04 {qux = [-2,-3,16]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[-2,-3,16]"))
  , test "10" (\_ -> equal (Ok (SimpleRecord04 {qux = [18,18,-6]})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[18,18,-6]"))
  , test "11" (\_ -> equal (Ok (SimpleRecord04 {qux = []})) (Json.Decode.decodeString (jsonDecSimpleRecord04 (Json.Decode.list Json.Decode.int)) "[]"))
  ]

sumEncodeUntagged : Test
sumEncodeUntagged = describe "Sum encode Untagged"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList []))))
  , test "2" (\_ -> equalHack "[-1,-2]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [-1,-2]))))
  , test "3" (\_ -> equalHack "-2"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt (-2)))))
  , test "4" (\_ -> equalHack "-5"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt (-5)))))
  , test "5" (\_ -> equalHack "-6"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt (-6)))))
  , test "6" (\_ -> equalHack "[10]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [10]))))
  , test "7" (\_ -> equalHack "[2]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [2]))))
  , test "8" (\_ -> equalHack "-4"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt (-4)))))
  , test "9" (\_ -> equalHack "[-15,-1,15,-7,9,-8,-3,14,8,-12,-6,9,-13,-5,-16,-1]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [-15,-1,15,-7,9,-8,-3,14,8,-12,-6,9,-13,-5,-16,-1]))))
  , test "10" (\_ -> equalHack "10"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMInt 10))))
  , test "11" (\_ -> equalHack "[-18,13,14,10,6]"(Json.Encode.encode 0 (jsonEncSumUntagged(Json.Encode.list Json.Encode.int) (SMList [-18,13,14,10,6]))))
  ]

sumDecodeUntagged : Test
sumDecodeUntagged = describe "Sum decode Untagged"
  [ test "1" (\_ -> equal (Ok (SMList [])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[]"))
  , test "2" (\_ -> equal (Ok (SMList [-1,-2])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[-1,-2]"))
  , test "3" (\_ -> equal (Ok (SMInt (-2))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-2"))
  , test "4" (\_ -> equal (Ok (SMInt (-5))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-5"))
  , test "5" (\_ -> equal (Ok (SMInt (-6))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-6"))
  , test "6" (\_ -> equal (Ok (SMList [10])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[10]"))
  , test "7" (\_ -> equal (Ok (SMList [2])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[2]"))
  , test "8" (\_ -> equal (Ok (SMInt (-4))) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "-4"))
  , test "9" (\_ -> equal (Ok (SMList [-15,-1,15,-7,9,-8,-3,14,8,-12,-6,9,-13,-5,-16,-1])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[-15,-1,15,-7,9,-8,-3,14,8,-12,-6,9,-13,-5,-16,-1]"))
  , test "10" (\_ -> equal (Ok (SMInt 10)) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "10"))
  , test "11" (\_ -> equal (Ok (SMList [-18,13,14,10,6])) (Json.Decode.decodeString (jsonDecSumUntagged (Json.Decode.list Json.Decode.int)) "[-18,13,14,10,6]"))
  ]

ntDecode1 : Test
ntDecode1 = describe "NT decode 1"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT1 "[]"))
  , test "2" (\_ -> equal (Ok ([0])) (Json.Decode.decodeString jsonDecNT1 "[0]"))
  , test "3" (\_ -> equal (Ok ([0,4,-4,1])) (Json.Decode.decodeString jsonDecNT1 "[0,4,-4,1]"))
  , test "4" (\_ -> equal (Ok ([-3,0,-2,-6])) (Json.Decode.decodeString jsonDecNT1 "[-3,0,-2,-6]"))
  , test "5" (\_ -> equal (Ok ([-2])) (Json.Decode.decodeString jsonDecNT1 "[-2]"))
  , test "6" (\_ -> equal (Ok ([7])) (Json.Decode.decodeString jsonDecNT1 "[7]"))
  , test "7" (\_ -> equal (Ok ([6,-2,1,-7,1,6,0,-4,3,-7,5])) (Json.Decode.decodeString jsonDecNT1 "[6,-2,1,-7,1,6,0,-4,3,-7,5]"))
  , test "8" (\_ -> equal (Ok ([-13,14,10,10])) (Json.Decode.decodeString jsonDecNT1 "[-13,14,10,10]"))
  , test "9" (\_ -> equal (Ok ([3,-15,12,2,13,12,-13,-4,-4,1,13,5,12,16,2,11])) (Json.Decode.decodeString jsonDecNT1 "[3,-15,12,2,13,12,-13,-4,-4,1,13,5,12,16,2,11]"))
  , test "10" (\_ -> equal (Ok ([-16,11,3,18,16,-13,-17,-9])) (Json.Decode.decodeString jsonDecNT1 "[-16,11,3,18,16,-13,-17,-9]"))
  , test "11" (\_ -> equal (Ok ([-1,15,4,4,-11,-3,17,19,-12,-5,9,9,-5,19])) (Json.Decode.decodeString jsonDecNT1 "[-1,15,4,4,-11,-3,17,19,-12,-5,9,9,-5,19]"))
  ]

ntEncode1 : Test
ntEncode1 = describe "NT encode 1"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT1 ([]))))
  , test "2" (\_ -> equalHack "[0]"(Json.Encode.encode 0 (jsonEncNT1 ([0]))))
  , test "3" (\_ -> equalHack "[0,4,-4,1]"(Json.Encode.encode 0 (jsonEncNT1 ([0,4,-4,1]))))
  , test "4" (\_ -> equalHack "[-3,0,-2,-6]"(Json.Encode.encode 0 (jsonEncNT1 ([-3,0,-2,-6]))))
  , test "5" (\_ -> equalHack "[-2]"(Json.Encode.encode 0 (jsonEncNT1 ([-2]))))
  , test "6" (\_ -> equalHack "[7]"(Json.Encode.encode 0 (jsonEncNT1 ([7]))))
  , test "7" (\_ -> equalHack "[6,-2,1,-7,1,6,0,-4,3,-7,5]"(Json.Encode.encode 0 (jsonEncNT1 ([6,-2,1,-7,1,6,0,-4,3,-7,5]))))
  , test "8" (\_ -> equalHack "[-13,14,10,10]"(Json.Encode.encode 0 (jsonEncNT1 ([-13,14,10,10]))))
  , test "9" (\_ -> equalHack "[3,-15,12,2,13,12,-13,-4,-4,1,13,5,12,16,2,11]"(Json.Encode.encode 0 (jsonEncNT1 ([3,-15,12,2,13,12,-13,-4,-4,1,13,5,12,16,2,11]))))
  , test "10" (\_ -> equalHack "[-16,11,3,18,16,-13,-17,-9]"(Json.Encode.encode 0 (jsonEncNT1 ([-16,11,3,18,16,-13,-17,-9]))))
  , test "11" (\_ -> equalHack "[-1,15,4,4,-11,-3,17,19,-12,-5,9,9,-5,19]"(Json.Encode.encode 0 (jsonEncNT1 ([-1,15,4,4,-11,-3,17,19,-12,-5,9,9,-5,19]))))
  ]

ntDecode2 : Test
ntDecode2 = describe "NT decode 2"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT2 "[]"))
  , test "2" (\_ -> equal (Ok ([1,-1])) (Json.Decode.decodeString jsonDecNT2 "[1,-1]"))
  , test "3" (\_ -> equal (Ok ([4,4])) (Json.Decode.decodeString jsonDecNT2 "[4,4]"))
  , test "4" (\_ -> equal (Ok ([3,-3,5])) (Json.Decode.decodeString jsonDecNT2 "[3,-3,5]"))
  , test "5" (\_ -> equal (Ok ([-6,-1,2])) (Json.Decode.decodeString jsonDecNT2 "[-6,-1,2]"))
  , test "6" (\_ -> equal (Ok ([-5,-4,-3,-9,-2,-3,6,9,-10])) (Json.Decode.decodeString jsonDecNT2 "[-5,-4,-3,-9,-2,-3,6,9,-10]"))
  , test "7" (\_ -> equal (Ok ([12,-10,-5,-7,7,-7,1,-7,6])) (Json.Decode.decodeString jsonDecNT2 "[12,-10,-5,-7,7,-7,1,-7,6]"))
  , test "8" (\_ -> equal (Ok ([-13,-11,10,-1,12,8,4,5,-6,4,-1,10,-2])) (Json.Decode.decodeString jsonDecNT2 "[-13,-11,10,-1,12,8,4,5,-6,4,-1,10,-2]"))
  , test "9" (\_ -> equal (Ok ([-7,-9])) (Json.Decode.decodeString jsonDecNT2 "[-7,-9]"))
  , test "10" (\_ -> equal (Ok ([-9,-13,2,-5,-2,2,18])) (Json.Decode.decodeString jsonDecNT2 "[-9,-13,2,-5,-2,2,18]"))
  , test "11" (\_ -> equal (Ok ([10,9,-6,-3,20,16,11,-17,15,5,-1,-14,-16,1,-1,-11,11])) (Json.Decode.decodeString jsonDecNT2 "[10,9,-6,-3,20,16,11,-17,15,5,-1,-14,-16,1,-1,-11,11]"))
  ]

ntEncode2 : Test
ntEncode2 = describe "NT encode 2"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT2 ([]))))
  , test "2" (\_ -> equalHack "[1,-1]"(Json.Encode.encode 0 (jsonEncNT2 ([1,-1]))))
  , test "3" (\_ -> equalHack "[4,4]"(Json.Encode.encode 0 (jsonEncNT2 ([4,4]))))
  , test "4" (\_ -> equalHack "[3,-3,5]"(Json.Encode.encode 0 (jsonEncNT2 ([3,-3,5]))))
  , test "5" (\_ -> equalHack "[-6,-1,2]"(Json.Encode.encode 0 (jsonEncNT2 ([-6,-1,2]))))
  , test "6" (\_ -> equalHack "[-5,-4,-3,-9,-2,-3,6,9,-10]"(Json.Encode.encode 0 (jsonEncNT2 ([-5,-4,-3,-9,-2,-3,6,9,-10]))))
  , test "7" (\_ -> equalHack "[12,-10,-5,-7,7,-7,1,-7,6]"(Json.Encode.encode 0 (jsonEncNT2 ([12,-10,-5,-7,7,-7,1,-7,6]))))
  , test "8" (\_ -> equalHack "[-13,-11,10,-1,12,8,4,5,-6,4,-1,10,-2]"(Json.Encode.encode 0 (jsonEncNT2 ([-13,-11,10,-1,12,8,4,5,-6,4,-1,10,-2]))))
  , test "9" (\_ -> equalHack "[-7,-9]"(Json.Encode.encode 0 (jsonEncNT2 ([-7,-9]))))
  , test "10" (\_ -> equalHack "[-9,-13,2,-5,-2,2,18]"(Json.Encode.encode 0 (jsonEncNT2 ([-9,-13,2,-5,-2,2,18]))))
  , test "11" (\_ -> equalHack "[10,9,-6,-3,20,16,11,-17,15,5,-1,-14,-16,1,-1,-11,11]"(Json.Encode.encode 0 (jsonEncNT2 ([10,9,-6,-3,20,16,11,-17,15,5,-1,-14,-16,1,-1,-11,11]))))
  ]

ntDecode3 : Test
ntDecode3 = describe "NT decode 3"
  [ test "1" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT3 "[]"))
  , test "2" (\_ -> equal (Ok ([1,-2])) (Json.Decode.decodeString jsonDecNT3 "[1,-2]"))
  , test "3" (\_ -> equal (Ok ([4,2,0,2])) (Json.Decode.decodeString jsonDecNT3 "[4,2,0,2]"))
  , test "4" (\_ -> equal (Ok ([])) (Json.Decode.decodeString jsonDecNT3 "[]"))
  , test "5" (\_ -> equal (Ok ([-6,-2])) (Json.Decode.decodeString jsonDecNT3 "[-6,-2]"))
  , test "6" (\_ -> equal (Ok ([-2,-3,5,6,1,-9,2])) (Json.Decode.decodeString jsonDecNT3 "[-2,-3,5,6,1,-9,2]"))
  , test "7" (\_ -> equal (Ok ([7,0,10,-10,-1,10,-6,-7,-4,-3])) (Json.Decode.decodeString jsonDecNT3 "[7,0,10,-10,-1,10,-6,-7,-4,-3]"))
  , test "8" (\_ -> equal (Ok ([-11,13,-4])) (Json.Decode.decodeString jsonDecNT3 "[-11,13,-4]"))
  , test "9" (\_ -> equal (Ok ([-16,-12,11,14,-3,6,12,16,8,15,7,2,-1])) (Json.Decode.decodeString jsonDecNT3 "[-16,-12,11,14,-3,6,12,16,8,15,7,2,-1]"))
  , test "10" (\_ -> equal (Ok ([-16,3,16,-10,-15,-15,-6,-5,-5,-8,-4])) (Json.Decode.decodeString jsonDecNT3 "[-16,3,16,-10,-15,-15,-6,-5,-5,-8,-4]"))
  , test "11" (\_ -> equal (Ok ([-14,0,4,12,-15,16,-15,11,-3,-1,-12])) (Json.Decode.decodeString jsonDecNT3 "[-14,0,4,12,-15,16,-15,11,-3,-1,-12]"))
  ]

ntEncode3 : Test
ntEncode3 = describe "NT encode 3"
  [ test "1" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT3 ([]))))
  , test "2" (\_ -> equalHack "[1,-2]"(Json.Encode.encode 0 (jsonEncNT3 ([1,-2]))))
  , test "3" (\_ -> equalHack "[4,2,0,2]"(Json.Encode.encode 0 (jsonEncNT3 ([4,2,0,2]))))
  , test "4" (\_ -> equalHack "[]"(Json.Encode.encode 0 (jsonEncNT3 ([]))))
  , test "5" (\_ -> equalHack "[-6,-2]"(Json.Encode.encode 0 (jsonEncNT3 ([-6,-2]))))
  , test "6" (\_ -> equalHack "[-2,-3,5,6,1,-9,2]"(Json.Encode.encode 0 (jsonEncNT3 ([-2,-3,5,6,1,-9,2]))))
  , test "7" (\_ -> equalHack "[7,0,10,-10,-1,10,-6,-7,-4,-3]"(Json.Encode.encode 0 (jsonEncNT3 ([7,0,10,-10,-1,10,-6,-7,-4,-3]))))
  , test "8" (\_ -> equalHack "[-11,13,-4]"(Json.Encode.encode 0 (jsonEncNT3 ([-11,13,-4]))))
  , test "9" (\_ -> equalHack "[-16,-12,11,14,-3,6,12,16,8,15,7,2,-1]"(Json.Encode.encode 0 (jsonEncNT3 ([-16,-12,11,14,-3,6,12,16,8,15,7,2,-1]))))
  , test "10" (\_ -> equalHack "[-16,3,16,-10,-15,-15,-6,-5,-5,-8,-4]"(Json.Encode.encode 0 (jsonEncNT3 ([-16,3,16,-10,-15,-15,-6,-5,-5,-8,-4]))))
  , test "11" (\_ -> equalHack "[-14,0,4,12,-15,16,-15,11,-3,-1,-12]"(Json.Encode.encode 0 (jsonEncNT3 ([-14,0,4,12,-15,16,-15,11,-3,-1,-12]))))
  ]

ntDecode4 : Test
ntDecode4 = describe "NT decode 4"
  [ test "1" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "2" (\_ -> equal (Ok (NT4 {foo = [2,0]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[2,0]}"))
  , test "3" (\_ -> equal (Ok (NT4 {foo = [4,4,-1]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[4,4,-1]}"))
  , test "4" (\_ -> equal (Ok (NT4 {foo = [1,-4]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[1,-4]}"))
  , test "5" (\_ -> equal (Ok (NT4 {foo = [1,0,-7]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[1,0,-7]}"))
  , test "6" (\_ -> equal (Ok (NT4 {foo = [4,-9,3,8,-2,-7,9,6,-6]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[4,-9,3,8,-2,-7,9,6,-6]}"))
  , test "7" (\_ -> equal (Ok (NT4 {foo = [-12,9,12,-7,-1]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-12,9,12,-7,-1]}"))
  , test "8" (\_ -> equal (Ok (NT4 {foo = [-6,12,-6,-12,7,12,4,-11,-9,-11,0,-9]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-6,12,-6,-12,7,12,4,-11,-9,-11,0,-9]}"))
  , test "9" (\_ -> equal (Ok (NT4 {foo = []})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[]}"))
  , test "10" (\_ -> equal (Ok (NT4 {foo = [-10,-10,0,-2,9,7,18,18,-13]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[-10,-10,0,-2,9,7,18,18,-13]}"))
  , test "11" (\_ -> equal (Ok (NT4 {foo = [16,7,6,-3,-6,19,-11,-20,14,8,10,10,-19,-20,13,-12,19,14]})) (Json.Decode.decodeString (jsonDecNT4 ) "{\"foo\":[16,7,6,-3,-6,19,-11,-20,14,8,10,10,-19,-20,13,-12,19,14]}"))
  ]

ntEncode4 : Test
ntEncode4 = describe "NT encode 4"
  [ test "1" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "2" (\_ -> equalHack "{\"foo\":[2,0]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [2,0]}))))
  , test "3" (\_ -> equalHack "{\"foo\":[4,4,-1]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [4,4,-1]}))))
  , test "4" (\_ -> equalHack "{\"foo\":[1,-4]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [1,-4]}))))
  , test "5" (\_ -> equalHack "{\"foo\":[1,0,-7]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [1,0,-7]}))))
  , test "6" (\_ -> equalHack "{\"foo\":[4,-9,3,8,-2,-7,9,6,-6]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [4,-9,3,8,-2,-7,9,6,-6]}))))
  , test "7" (\_ -> equalHack "{\"foo\":[-12,9,12,-7,-1]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-12,9,12,-7,-1]}))))
  , test "8" (\_ -> equalHack "{\"foo\":[-6,12,-6,-12,7,12,4,-11,-9,-11,0,-9]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-6,12,-6,-12,7,12,4,-11,-9,-11,0,-9]}))))
  , test "9" (\_ -> equalHack "{\"foo\":[]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = []}))))
  , test "10" (\_ -> equalHack "{\"foo\":[-10,-10,0,-2,9,7,18,18,-13]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [-10,-10,0,-2,9,7,18,18,-13]}))))
  , test "11" (\_ -> equalHack "{\"foo\":[16,7,6,-3,-6,19,-11,-20,14,8,10,10,-19,-20,13,-12,19,14]}"(Json.Encode.encode 0 (jsonEncNT4 (NT4 {foo = [16,7,6,-3,-6,19,-11,-20,14,8,10,10,-19,-20,13,-12,19,14]}))))
  ]

