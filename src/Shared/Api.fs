namespace Shared.Api
open Newtonsoft.Json.Serialization
open Newtonsoft.Json

[<Struct>]
type Snowflake =
    { v: uint64 }
    with
        static member Create v = { v = v }

type SnowflakeConverter() =
    inherit Newtonsoft.Json.JsonConverter<Snowflake>()

    override x.WriteJson(writer: Newtonsoft.Json.JsonWriter, value: Snowflake, serializer: Newtonsoft.Json.JsonSerializer) =
        writer.WriteValue(string value.v)

    override x.ReadJson(reader: Newtonsoft.Json.JsonReader, objectType: System.Type, existingValue: Snowflake, hasExistingValue: bool, serializer: Newtonsoft.Json.JsonSerializer): Snowflake =
        if hasExistingValue then
            existingValue
        else
            let res = reader.Value :?> string
            match System.UInt64.TryParse res with
            | true, v -> Snowflake.Create v
            | false, _ -> failwithf "snowflake parse error: %A" res

type NullableSnowflakeConverter() =
    inherit Newtonsoft.Json.JsonConverter<System.Nullable<Snowflake>>()

    override x.WriteJson(writer: Newtonsoft.Json.JsonWriter, value: System.Nullable<Snowflake>, serializer: Newtonsoft.Json.JsonSerializer) =
        if value.HasValue then
            writer.WriteValue(string value.Value.v)
        else
            writer.WriteValue(null: obj)

    override x.ReadJson(reader: Newtonsoft.Json.JsonReader, objectType: System.Type, existingValue: System.Nullable<Snowflake>, hasExistingValue: bool, serializer: Newtonsoft.Json.JsonSerializer): System.Nullable<Snowflake> =
        if hasExistingValue then
            existingValue
        else
            match reader.Value with
            | null -> System.Nullable()
            | :? string as v ->
                match System.UInt64.TryParse v with
                | true, v -> System.Nullable(Snowflake.Create v)
                | false, _ -> failwithf "snowflake parse error: %A" v
            | x -> failwithf "snowflake parse error: expected string but %A" x

module Serializer =
    open FsharpMyExtension

    let jsonSerializer =
        JsonSerializer.Create(
            let x =
                new JsonSerializerSettings(
                    ContractResolver = new CamelCasePropertyNamesContractResolver()
                )

            x.Converters.Add (new SnowflakeConverter())
            x.Converters.Add (new NullableSnowflakeConverter())
            x.Converters.Add FSharpJsonType.SerializeOption.converter
            x
        )

    let ser x =
        use file = new System.IO.StringWriter()
        use st = new JsonTextWriter(file)
        jsonSerializer.Serialize(st, x)
        file.ToString()

    let des (str: string) =
        use file = new System.IO.StringReader(str)
        use st = new JsonTextReader(file)
        jsonSerializer.Deserialize<_>(st)
