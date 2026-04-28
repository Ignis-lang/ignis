mod common;

#[test]
fn std_serializer_json_scalar_entrypoints_round_trip() {
  let result = common::compile_project_and_run_with_workspace_std(
    r#"
import Json from "std::json";

function main(): i32 {
    let expectedBoolean: String = String::create("true");
    let expectedNumber: String = String::create("42");
    let expectedText: String = String::create("\"line\\n\\\"quoted\\\"\"");

    match (Json::stringify(true)) {
        Result::OK(booleanJson) -> {
            if (!booleanJson.equals(&expectedBoolean)) {
                return 1;
            }
        },
        Result::ERROR(_) -> {
            return 10;
        },
    };

    match (Json::stringify(42)) {
        Result::OK(numberJson) -> {
            if (!numberJson.equals(&expectedNumber)) {
                return 2;
            }
        },
        Result::ERROR(_) -> {
            return 11;
        },
    };

    match (Json::stringify("line\n\"quoted\"")) {
        Result::OK(textJson) -> {
            if (!textJson.equals(&expectedText)) {
                return 3;
            }
        },
        Result::ERROR(_) -> {
            return 12;
        },
    };

    return 0;
}
"#,
  )
  .expect("workspace std serializer scalar program should compile and run");

  assert_eq!(result.exit_code, 0, "stderr: {}", result.stderr);
}

#[test]
fn std_serializer_record_helpers_support_json_and_toml() {
  let result = common::compile_project_and_run_with_workspace_std(
    r#"
import Json from "std::json";
import Serialize, Serializer from "std::serializer";
import Toml from "std::toml";

@implements(Serialize)
record Payload {
    public value: i32;
    public name: str;

    serialize(&self, output: &mut String, writer: &Serializer::Writer): Result<boolean, Serializer::Error> {
        Serializer::beginRecord(output, writer)!;
        Serializer::writeField(output, "value", writer, true)!;
        Serializer::write(output, self.value, writer)!;
        Serializer::writeField(output, "name", writer, false)!;
        Serializer::write(output, self.name, writer)!;
        Serializer::endRecord(output, writer)!;
        return Result::OK(true);
    }
}

function main(): i32 {
    let payload: Payload = Payload { value: 7, name: "ignis" };
    let mut json: String = String::new();
    let mut toml: String = String::new();
    let expectedJson: String = String::create("{\"value\":7,\"name\":\"ignis\"}");
    let expectedToml: String = String::create("value = 7\nname = \"ignis\"");

    let jsonWriter: Serializer::Writer = Json::writer();
    let tomlWriter: Serializer::Writer = Toml::writer();

    match (payload.serialize(&mut json, &jsonWriter)) {
        Result::OK(_) -> {},
        Result::ERROR(_) -> {
            return 10;
        },
    };

    match (payload.serialize(&mut toml, &tomlWriter)) {
        Result::OK(_) -> {},
        Result::ERROR(_) -> {
            return 11;
        },
    };

    if (!json.equals(&expectedJson)) {
        return 1;
    }

    if (!toml.equals(&expectedToml)) {
        return 2;
    }

    return 0;
}
"#,
  )
  .expect("workspace std serializer record program should compile and run");

  assert_eq!(result.exit_code, 0, "stderr: {}", result.stderr);
}

#[test]
fn std_serializer_toml_rejects_scalar_roots() {
  let result = common::compile_project_and_run_with_workspace_std(
    r#"
import Serializer from "std::serializer";
import Toml from "std::toml";

function main(): i32 {
    let result: Result<String, Serializer::Error> = Toml::stringify(42);

    return match (result) {
        Result::OK(_) -> 1,
        Result::ERROR(error) -> {
            return match (error.kind) {
                Serializer::ErrorKind::UNSUPPORTED_ROOT_SHAPE -> 0,
                _ -> 2,
            };
        },
    };
}
"#,
  )
  .expect("workspace std serializer toml scalar program should compile and run");

  assert_eq!(result.exit_code, 0, "stderr: {}", result.stderr);
}

#[test]
fn std_serializer_core_supports_custom_writer_layouts() {
  let result = common::compile_project_and_run_with_workspace_std(
    r#"
import Serialize, Serializer from "std::serializer";

@implements(Serialize)
record Payload {
    public value: i32;
    public name: str;

    serialize(&self, output: &mut String, writer: &Serializer::Writer): Result<boolean, Serializer::Error> {
        Serializer::beginRecord(output, writer)!;
        Serializer::writeField(output, "value", writer, true)!;
        Serializer::write(output, self.value, writer)!;
        Serializer::writeField(output, "name", writer, false)!;
        Serializer::write(output, self.name, writer)!;
        Serializer::endRecord(output, writer)!;
        return Result::OK(true);
    }
}

function main(): i32 {
    let payload: Payload = Payload { value: 7, name: "ignis" };
    let mut output: String = String::new();
    let writer: Serializer::Writer = Serializer::Writer {
        recordPrefix: "<",
        recordSuffix: ">",
        entrySeparator: " | ",
        fieldSeparator: " => ",
        supportsScalarRoot: true,
        quoteFieldKeys: false,
    };
    let expected: String = String::create("<value => 7 | name => \"ignis\">");

    match (payload.serialize(&mut output, &writer)) {
        Result::OK(_) -> {},
        Result::ERROR(_) -> {
            return 10;
        },
    };

    if (!output.equals(&expected)) {
        return 1;
    }

    return 0;
}
"#,
  )
  .expect("workspace std serializer custom writer program should compile and run");

  assert_eq!(result.exit_code, 0, "stderr: {}", result.stderr);
}
