mod cli;

use clap::Parser as ClapParser;
use ignis_parser::compile_file;
use std::{sync::Arc, fs::File, io::Read, path::Path};

use cli::{Cli, SubCommand};
use ignis_config;

fn load_project_config() -> Option<ignis_config::IgnisProjectConfig> {
  let binding = std::env::current_dir().unwrap();
  let file_path = binding.to_str().unwrap();

  let file_path = Path::new(file_path).join("ignis.toml");

  if file_path.exists() {
    let file = File::open(file_path);
    if file.is_err() {
      println!("Failed to open file");
      std::process::exit(1);
    }

    let mut file = file.unwrap();
    let mut content = String::new();

    let _ = file.read_to_string(&mut content);

    let config: ignis_config::IgnisProjectConfig = toml::from_str(&content).unwrap();

    return Some(config);
  }

  return None;
}

fn check_if_project() -> bool {
  let binding = std::env::current_dir().unwrap();
  let file_path = binding.to_str().unwrap();

  let file_path = Path::new(file_path).join("ignis.toml");

  if file_path.exists() {
    let file = File::open(file_path);
    if file.is_err() {
      return false;
    }

    return true;
  }

  false
}

fn load_manifest(std_path: &str) -> ignis_config::IgnisSTDManifest {
  let file_path = Path::new(std_path).join("manifest.toml");

  if file_path.exists() {
    let file = File::open(file_path);
    if file.is_err() {
      println!("Failed to open file");
      std::process::exit(1);
    }

    let mut file = file.unwrap();
    let mut content = String::new();

    let _ = file.read_to_string(&mut content);

    let config: ignis_config::IgnisSTDManifest = toml::from_str(&content).unwrap();

    return config;
  }

  return ignis_config::IgnisSTDManifest::default();
}

fn parse_cli_to_config(cli: &Cli) -> Arc<ignis_config::IgnisConfig> {
  let mut config =
    ignis_config::IgnisConfig::new_basic(cli.debug.iter().map(|x| x.into()).collect(), cli.quiet, cli.verbose);

  config.std = !cli.std;
  config.auto_load_std = !cli.auto_load_std;

  if cli.std_path.eq("IGNIS_STD_PATH") {
    if let Ok(v) = std::env::var("IGNIS_STD_PATH") {
      config.std_path = v;
    } else {
      // println!("Failed to load std path from environment variable");
      // std::process::exit(1);
    }
  }

  let manifest = load_manifest(&config.std_path);
  config.manifest = manifest;

  match &cli.subcommand {
    SubCommand::Build(build) => {
      let is_project = check_if_project();

      if build.file_path.is_none() && !is_project {
        println!("No file path provided. Please provide a file path or run `ignis init` to create a new project.");
        println!("For more information, run `ignis --help`");
        std::process::exit(1);
      }

      if is_project {
        let mut project_config = load_project_config().unwrap();

        project_config.build.target = build.target.clone().into();

        if build.optimize != project_config.build.optimize {
          project_config.build.optimize = build.optimize;
        }

        if build.output_dir != "build" {
          project_config.build.output_dir = build.output_dir.clone();
        }

        if build.file_path.is_some() {
          project_config.build.main_file = build.file_path.as_ref().unwrap().to_string();
        }

        config.project_config = Some(project_config);
      }

      config.build = true;
      config
        .build_config
        .clone_from(&Some(ignis_config::IgnisBuildConfig::new(
          build.file_path.clone(),
          build.target.clone().into(),
          is_project,
          build.optimize.clone(),
          build.output_dir.clone(),
        )));
    },
    SubCommand::Init(init) => {
      config.init = true;
      config.init_config.clone_from(&Some(ignis_config::IgnisInitConfig::new(
        init.name.clone(),
        init.project_version.clone(),
        init.authors.clone(),
        init.description.clone(),
        init.keywords.clone(),
        init.license.clone(),
        init.repository.clone(),
        init.git,
        init.target.clone().into(),
      )));
    },
  };

  return Arc::new(config);
}

fn main() {
  let cli = Cli::parse();

  let config = parse_cli_to_config(&cli);

  let build_config = config.build_config.clone().unwrap();
  let file_path = build_config.file.unwrap();

  compile_file(config, &file_path);
}
