mod common;

use std::fs;
use std::path::PathBuf;

use ignis_config::{IgnisSTDManifest, TargetBackend};

fn workspace_std_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std")
}

fn load_std_manifest() -> IgnisSTDManifest {
  let manifest_path = workspace_std_path().join("manifest.toml");
  let content = fs::read_to_string(&manifest_path).expect("read std manifest");
  toml::from_str(&content).expect("parse std manifest")
}

#[test]
fn std_serializer_manifest_registers_module_without_autoload() {
  let manifest = load_std_manifest();

  assert_eq!(
    manifest.get_module_path("serializer").map(|path| path.as_str()),
    Some("serializer/mod.ign")
  );
  assert!(
    !manifest.is_auto_load("serializer"),
    "serializer should remain opt-in and stay out of auto_load.modules"
  );
}

#[test]
fn std_serializer_build_std_succeeds() {
  let attempt = common::build_std_with_target(TargetBackend::C).expect("temporary std build setup should succeed");

  assert!(
    attempt.result.is_ok(),
    "expected build_std to succeed with serializer prerequisites present"
  );
  assert!(
    attempt.output_dir.join("std/lib/libignis_std.a").exists(),
    "expected build_std to emit the std archive"
  );
}

#[test]
fn std_serializer_surface_and_hash_map_traversal_compile_with_workspace_std() {
  let attempt = common::compile_project_single_file_with_workspace_std(
    r#"
import Eq from "std::collections";
import Hash from "std::collections";
import HashMap from "std::collections";
import Hasher from "std::hash";
import Json from "std::json";
import Serialize from "std::serializer";
import Toml from "std::toml";
import Serializer from "std::serializer";

@implements(Hash, Eq)
record Key {
    public id: i32;
    public bucket: u64;

    hash(&self, hasher: &mut Hasher): void {
        let mut state: &mut Hasher = hasher;
        state.writeU64(self.bucket);
    }

    public equals(&self, other: &Key): boolean {
        return self.id == other.id;
    }
}

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

function requireJson<T: Serialize>(value: &T): Result<String, Serializer::Error> {
    return Json::stringify<T>(value);
}

function requireToml<T: Serialize>(value: &T): Result<String, Serializer::Error> {
    return Toml::stringify<T>(value);
}

function main(): i32 {
    let mut map: HashMap<Key, i32> = HashMap::init<Key, i32>();
    let payload: Payload = Payload { value: 7, name: "ignis" };
    let jsonResult: Result<String, Serializer::Error> = requireJson<Payload>(&payload);
    let tomlResult: Result<String, Serializer::Error> = requireToml<Payload>(&payload);
    let _jsonSurface: Result<String, Serializer::Error> = jsonResult;
    let _tomlSurface: Result<String, Serializer::Error> = tomlResult;

    map.insert(Key { id: 2, bucket: 1 }, 2);
    map.insert(Key { id: 1, bucket: 1 }, 1);

    let mut firstOrder: i32 = 0;
    let mut cursor: Option<u64> = map.firstEntryCursor();
    while (true) {
        match (cursor) {
            Option::SOME(entryCursor) -> {
                firstOrder = (firstOrder * 100) + (map.entryKey(entryCursor).id * 10) + *map.entryValue(entryCursor);
                cursor = map.nextEntryCursor(entryCursor);
            },
            Option::NONE -> {
                break;
            },
        };
    }

    let mut secondOrder: i32 = 0;
    cursor = map.firstEntryCursor();
    while (true) {
        match (cursor) {
            Option::SOME(entryCursor) -> {
                secondOrder = (secondOrder * 100) + (map.entryKey(entryCursor).id * 10) + *map.entryValue(entryCursor);
                cursor = map.nextEntryCursor(entryCursor);
            },
            Option::NONE -> {
                break;
            },
        };
    }

    if (firstOrder != secondOrder) {
        return 1;
    }

    if (firstOrder == 0) {
        return 3;
    }

    return 0;
}
"#,
    TargetBackend::C,
  )
  .expect("temporary workspace-std compile setup should succeed");

  assert!(
    attempt.result.is_ok(),
    "expected workspace std program to compile with serializer surface and traversal helper"
  );
}
